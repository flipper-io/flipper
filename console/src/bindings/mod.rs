//! Generate high level language bindings from native Flipper modules.
//!
//! Flipper modules can define a public API which becomes remotely executable
//! from host platforms like mobile and desktop machines. These platforms can
//! make use of higher level programming languages. This bindings module serves
//! to automatically generate the language bindings necessary to control Flipper
//! from these high level languages.
//!
//! Public APIs are exported in a binary section called
//! `.lf.funcs`. This module uses ELF and DWARF file metadata to read
//! the signatures of each exported function in a flipper module. This metadata
//! is the foundation for auto-generating high level language bindings to
//! remotely execute the modules.
//!
//! The tasks necessary to perform binding generation are as follows:
//!
//! 1) Parse a Flipper module and read the signatures of functions in its
//! public API.
//!
//! 2) Store the module API in an intermediary format which can be generically
//! translated into any high-level language API.
//!
//! 3) Pass the API description into binding generators which create
//! high-level language packages which expose a functionally equivalent API
//! whose implementation remotely executes the modules on Flipper.
//!
//! This module exposes structs which represent type information parsed from a
//! binary with DWARF debug information. The DWARF format contains enough
//! information to reconstruct the type signatures of every function in a
//! binary. We parse this information into a general format given by the types
//! in this module, and later use this format to generate high level language
//! bindings.

#![allow(missing_docs)]

pub mod dwarf;
pub mod generators;

use std::rc::Rc;
use std::ops::Range;
use object::{
    self,
    Object,
    ObjectSection,
};
use failure::Error;

/// Represents errors that can occur when parsing ELF and/or DWARF files.
#[derive(Debug, Fail)]
pub enum BindingError {
    #[fail(display = "failed to read binary section: {}", _0)]
    SectionReadError(String),
    #[fail(display = "failed to parse elf section: {}", _0)]
    ElfReadError(String),
    #[fail(display = "failed to parse macho section: {}", _0)]
    MachoReadError(String),
    #[fail(display = "failed to read dwarf {}", _0)]
    DwarfReadError(String),
    #[fail(display = "failed to parse dwarf {}", _0)]
    DwarfParseError(String),
    #[fail(display = "failed to resolve {}", _0)]
    ResolutionError(String),
}

/// Represents a type of value which can be expressed in a Flipper module.
///
/// Types can be concrete base types such as `int`, aliases (typedefs) such
/// as `uint8_t`, pointers such as `char *`, or other types which are
/// currently unsupported in a concrete sense but which can be referenced
/// as generic data (such as structs).
#[derive(Debug, PartialOrd, PartialEq, Ord, Eq)]
pub enum Type {
    /// Base types encapsulate all of the information necessary to represent
    /// a value in the program they were parsed from. This is used to
    /// generate high level bindings using representations that match those
    /// of the low level code.
    Base {
        /// The name of this base type, e.g. `int` or `char`.
        name: String,
        /// The memory footprint of this type, e.g. 4 for `int`.
        size: u64,
    },
    /// Aliases are used to represent types that stand in for other types.
    /// Notably, they are used for `typedef`s in C. Aliased types indicate
    /// that their value is exactly the type referred to (as opposed to
    /// `Reference`s, which represent one level of indirection to their
    /// referred type, i.e. a pointer).
    Alias {
        /// The name of the alias, e.g. the "int" in `typedef int uint32_t`
        name: String,
        /// A reference to the `Type` that this alias is equivalent to.
        /// Note that one alias may refer to another alias or to a reference,
        /// but that the chain of types must always terminate in a `Type::Base`
        /// or a `Type::Reference` with a `None` type (i.e. `void *`).
        typ: Rc<Type>,
    },
    /// A `Reference` is a type which is not complete on its own, but rather
    /// refers to one or more `Reference`s and eventually a `Base` type.
    /// This is used to represent pointers.
    Reference {
        /// A reference to another `Type`. This can be used to represent
        /// a type such as `char *`.
        typ: Rc<Type>,
    },
    /// There are additional types which may be parsed from a dwarf DIE tree,
    /// but until they're all supported, References which try to use those
    /// types will instead bottom out to the `Unsupported` variant.
    Unsupported,
}

impl Type {
    /// Finds the name of this `Type`.
    ///
    /// If this is a base type, the name is simply the base type name.
    ///
    /// If this is an alias (typedef), then the name is the name of the
    /// typedef itself, not the base type it refers to.
    ///
    /// If this is a reference, then the name is the name of whatever
    /// the referenced type is, with a `*` appended to it. This is
    /// recursive, so a reference to a reference to a char would
    /// have a name of `char**`.
    ///
    /// All other types (i.e. "unsupported) have a name of `void`.
    /// Unsupported types will never be concretely used, but may be the
    /// subject of a `Reference`. In this case, we use a `void *` to
    /// represent the data generically without giving it a specific type.
    pub fn name(&self) -> String {
        match *self {
            Type::Base { ref name, .. } => name.to_owned(),
            Type::Alias { ref name, .. } => name.to_owned(),
            Type::Reference { ref typ, .. } => {
                let mut name = typ.name().clone();
                name.push('*');
                name
            },
            _ => "void".to_owned(),
        }
    }

    /// Returns the size of this `Type`.
    pub fn size(&self) -> u64 {
        match *self {
            Type::Base { size, .. } => size,
            Type::Alias { ref typ, .. } => typ.size(),
            Type::Reference { ref typ, .. } => typ.size(),
            _ => 0,
        }
    }
}

/// A single parameter of a Flipper function.
///
/// For example, given the following function in C:
///
/// ```c
/// int foo(char letter, uint8_t count);
/// ```
///
/// The parameters would be named `letter` and `count`, and the types would refer
/// to `Type`s named `char` and `uint8_t`.
#[derive(Debug, PartialOrd, PartialEq, Ord, Eq)]
pub struct Parameter {
    /// The name of the formal parameter as defined in the original program,
    /// e.g. "greeting" in `void say_hello(char *greeting);`.
    pub name: String,
    /// The type of the parameter, e.g. a representation of the "char *" in
    /// `void say_hello(char *greeting);`. A type of `None` represents `void`.
    pub typ: Rc<Type>,
}

/// A single function signature of a Flipper module.
///
/// The name and parameters are used for generating FFI bindings to this function.
/// The address is captured so it's possible to tell if the function belongs to a
/// certain binary section.
#[derive(Debug, PartialOrd, PartialEq, Ord, Eq)]
pub struct Function {
    /// The name of the function as defined in the original program,
    /// e.g. "say_hello" in `void say_hello(char *greeting);`.
    pub name: String,
    /// The address in the binary where this function's compiled code lives.
    pub address: u64,
    /// A list of the parameters of this function.
    pub parameters: Vec<Parameter>,
    /// The return type of this function.
    pub ret: Rc<Type>,
}

/// A fully formed Flipper module which can have bindings generated for it.
#[derive(Debug, PartialOrd, PartialEq, Ord, Eq)]
pub struct Module {
    name: String,
    description: String,
    functions: Vec<Function>,
}

impl Module {
    /// Parse Flipper module metadata from a debug-enabled binary.
    pub fn parse(name: String, description: String, binary: &[u8]) -> Result<Self, Error> {
        let functions = dwarf::parse(binary)?;
        let range = read_section_address(binary, ".lf.funcs")?;
        let functions: Vec<_> = functions.into_iter().filter(|f| range.start <= f.address && f.address < range.end).collect();

        Ok(Module {
            name,
            description,
            functions,
        })
    }
}

fn read_section_address(data: &[u8], section_name: &str) -> Result<Range<u64>, Error> {
    let bin = object::File::parse(data)
        .map_err(|_| BindingError::SectionReadError(section_name.to_owned()))?;

    for section in bin.sections() {
        if let Some(name) = section.name() {
            if name == section_name {
                let address = section.address();
                let size = section.size();
                return Ok(address..address + size);
            }
        }
    }

    Err(BindingError::SectionReadError(section_name.to_owned()).into())
}

#[cfg(test)]
mod test {
    use super::*;
    use super::generators::c::*;
    use std::io::Cursor;

    /// Performs an end-to-end test for parsing a Flipper module binary and generating
    /// an appropriate C binding.
    #[test]
    fn test_parse_generate_c() {
        let dwarf: &[u8] = include_bytes!("./test_resources/module_binding_test");
        let expected: &[u8] = include_bytes!("./test_resources/module_binding_test_expected.c");

        let module = Module::parse("user".to_owned(), "User module description".to_owned(), dwarf);
        assert!(module.is_ok());
        let module = module.unwrap();
        let mut binding: Cursor<Vec<u8>> = Cursor::new(Vec::new());
        let _ = generate_module(module, &mut binding);

        assert_eq!(&*binding.into_inner(), expected);
    }

    #[test]
    fn test_parse_generate_c_macho() {
        let dwarf: &[u8] = include_bytes!("./test_resources/dwarf_parse_test_macho");
        let expected: &[u8] = include_bytes!("./test_resources/dwarf_parse_test_macho_expected.c");

        let module = Module::parse("user".to_owned(), "User module description".to_owned(), dwarf);
        assert!(module.is_ok());
        let module = module.unwrap();
        let mut binding: Cursor<Vec<u8>> = Cursor::new(Vec::new());
        let _ = generate_module(module, &mut binding);

        assert_eq!(&*binding.into_inner(), expected);
    }
}
