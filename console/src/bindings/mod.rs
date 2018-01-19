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
//! 3) Pass the API description into binding deserializers which generate
//! high-level language packages which expose a functionally equivalent API
//! whose implementation automatically executes the modules on Flipper.
//!
//! This module exposes structs which represent type information parsed from a
//! binary with DWARF debug information. The DWARF format contains enough
//! information to reconstruct the type signatures of every function in a
//! binary. We parse this information into a general format given by the types
//! in this module, and later use this format to generate high level language
//! bindings.
//!
//! See the [parser] to see how the type information is read from DWARF.
//!
//! [parser](./binary_parser)

#![allow(missing_docs)]

pub mod dwarf;
pub mod generators;

use std::io::Read;
use std::ops::Range;
use std::rc::Rc;
use failure::Error;
use goblin::elf::Elf;

/// Represents errors that can occur when parsing ELF and/or DWARF files.
#[derive(Debug, Fail)]
pub enum BindingError {
    #[fail(display = "failed to read elf file")]
    ElfReadError,
    #[fail(display = "failed to parse elf section: {}", _0)]
    ElfSectionError(String),
    #[fail(display = "failed to read dwarf section: {}", _0)]
    DwarfReadError(&'static str),
    #[fail(display = "failed to parse dwarf {}", _0)]
    DwarfParseError(String),
    #[fail(display = "failed to resolve {}", _0)]
    ResolutionError(String),
}

/// Represents a type in the program whose symbols are parsed. Types can be
/// concrete base types such as `int`, aliases such as `uint8_t`, pointers
/// such as `char *`, or unsupported types.
#[derive(Debug, PartialOrd, PartialEq, Ord, Eq)]
pub enum Type {
    /// Base types encapsulate all of the information necessary to represent
    /// a value in the program they were parsed from. This is used to
    /// generate high level bindings using representations that match those
    /// of the low level code.
    Base {
        /// The name of this base type, e.g. `int` or `uint16_t`.
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
    ///
    ///
    pub fn size(&self) -> u64 {
        match *self {
            Type::Base { size, .. } => size,
            Type::Alias { ref typ, .. } => typ.size(),
            Type::Reference { ref typ, .. } => typ.size(),
            _ => 0,
        }
    }
}

/// Represents a parameter to a subprogram as given by the subprogram definition.
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

/// Represents a specific `subprogram` parsed from a DWARF file. The name will be
/// used for generating FFI bindings to this function. The address is captured
/// so it's possible to tell if the subprogram belongs to a certain binary section.
#[derive(Debug, PartialOrd, PartialEq, Ord, Eq)]
pub struct Subprogram {
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

/// Represents a fully formed Flipper module which can have bindings generated
/// for it. This is created by parsing the module metadata from the DWARF debug
/// symbols of a Flipper module binary.
#[derive(Debug, PartialOrd, PartialEq, Ord, Eq)]
pub struct Module {
    name: String,
    description: String,
    functions: Vec<Subprogram>,
}

impl Module {
    pub fn parse(name: String, description: String, binary: &[u8]) -> Result<Self, Error> {
        let functions = dwarf::parse(binary)?;

        Ok(Module {
            name,
            description,
            functions,
        })
    }
}
