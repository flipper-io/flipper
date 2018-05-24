//! Parse function signature metadata from DWARF debug symbols.
//!
//! Within the DIE tree, entries refer to one another by specifying the
//! offset of the other entry. After fully parsing the information we want
//! from the DIE tree, we resolve these offset links into more traceable
//! `Rc`s which we can use to follow references until we hit a base case
//! such as `DW_TAG_base_type`.
//!
//! During parsing, base types are the only ones which are fully qualified from
//! the start. All other types (which will eventually be represented as
//! `Type::Reference`s) are parsed into an "Unresolved" struct. The unresolved types
//! are later resolved into references after the entire dwarf tree has been parsed.

#![allow(non_snake_case)]

use std::rc::Rc;
use std::collections::HashMap;
use std::ops::{
    Deref,
    DerefMut,
};

use fail::Error;
use fallible_iterator::FallibleIterator;
use object;
use object::Object;
use gimli;
use gimli::{
    Reader,
    ReaderOffset,
    AttributeValue,
    DebugInfo,
    DebugAbbrev,
    DebugStr,
    DebuggingInformationEntry,
};

use super::{
    Type,
    Parameter,
    Function,
    BindingError,
};

/// Represents reference types, which cannot be immediately resolved.
#[derive(Debug)]
struct UnresolvedReference {
    /// The offset into the DIE tree which represents the type of this entry.
    typ: Option<u64>,
    /// The offset into the DIE tree at which this entry lives.
    offset: u64,
}

impl UnresolvedReference {
    /// Converts an `UnresolvedReference` yielded by the parser into a fully
    /// resolved `Type::Reference`.
    fn into_resolved(self, types: &TypeRegistry) -> Result<Type, Error> {
        Ok(match self.typ {
            None => Type::Reference { typ: types.void() },
            Some(ref typ_offset) => {
                let typ = types.get(typ_offset).map(|rc| rc.clone());
                match typ {
                    Some(typ) => Type::Reference { typ },
                    None => Type::Unsupported,
                }
            }
        })
    }
}

/// Represents alias types, which cannot be immediately resolved.
#[derive(Debug)]
struct UnresolvedAlias {
    /// The name of the alias, e.g. the "int" in `typedef int uint32_t`.
    name: String,
    /// The offset into the DIE tree of the type this alias resolves to.
    typ: u64,
    /// The offset into the DIE tree at which this entry lives.
    offset: u64,
}

impl UnresolvedAlias {
    /// Converts an `UnresolvedAlias` yielded by the parser into a fully
    /// resolved `Type::Alias`.
    fn into_resolved(self, types: &TypeRegistry) -> Result<Type, Error> {
        let typ = types.get(&self.typ).map(|rc| rc.clone());
        Ok(match typ {
            Some(typ) => Type::Alias { name: self.name, typ },
            None => Type::Unsupported,
        })
    }
}

/// Represents a single formal parameter of a subprogram.
#[derive(Debug)]
struct UnresolvedParameter {
    /// The name of the formal parameter this was parsed from,
    /// e.g. the "greeting" in `void say_hello(char *greeting);`
    name: String,
    /// The offset into the DIE tree of the type of this parameter.
    typ: Option<u64>,
    /// The offset into the DIE tree of this formal parameter.
    offset: u64,
}

impl UnresolvedParameter {
    /// Converts an `UnresolvedParameter` yielded by the parser into a fully
    /// resolved `Parameter`.
    fn into_resolved(self, types: &TypeRegistry) -> Result<Parameter, Error> {
        Ok(match self.typ {
            None => Parameter { typ: types.void(), name: self.name },
            Some(ref typ_offset) => {
                let typ = types.get(typ_offset).map(|rc| rc.clone())
                    .ok_or(BindingError::ResolutionError(format!("parameter type for {}", self.name)))?;
                Parameter { typ, name: self.name }
            }
        })
    }
}

/// Represents a single function of a dwarf-infused binary.
#[derive(Debug)]
struct UnresolvedSubprogram {
    /// The name given to this subprogram, e.g. "foo" in `void foo(int bar);`
    name: String,
    /// The address at which the function lives in memory. This can be used
    /// to determine which binary section a function belongs to.
    address: u64,
    /// A list of offsets where the formal parameter entries are stored in
    /// the dwarf DIE tree.
    parameters: Vec<UnresolvedParameter>,
    /// The offset to the return type of this function in the DIE tree.
    ret: Option<u64>,
}

impl UnresolvedSubprogram {
    /// Converts an `UnresolvedSubprogram` yielded by the parser into a fully
    /// resolved `Subprogram`.
    fn into_resolved(self, types: &TypeRegistry) -> Result<Function, Error> {
        let mut parameters: Vec<Parameter> = Vec::with_capacity(self.parameters.capacity());
        for parameter in self.parameters.into_iter() {
            parameters.push(parameter.into_resolved(types)?);
        }

        let ret = self.ret.and_then(|ref key| types.get(key).map(|rc| rc.clone())).unwrap_or(types.void());

        Ok(Function {
            name: self.name,
            address: self.address,
            parameters,
            ret,
        })
    }
}

/// Parses a base type from a `DW_TAG_base_type` entry in the DIE tree.
fn parse_base_type<'a, R: Reader>(entry: &'a DebuggingInformationEntry<R, R::Offset>, strings: &'a DebugStr<R>) -> Result<(u64, Type), Error> {
    entry.attrs()

        // Iterate over the attributes of the entry to collect the name, encoding, and size
        .fold((None, None, None), |(name, encoding, size), attr| {
            match (attr.name(), attr.value()) {
                (gimli::DW_AT_name, AttributeValue::DebugStrRef(name)) => (strings.get_str(name).ok(), encoding, size), // Used for elf files
                (gimli::DW_AT_name, AttributeValue::String(name)) => (Some(name), encoding, size), // Used for mach-o files
                (gimli::DW_AT_encoding, AttributeValue::Encoding(encoding)) => (name, Some(encoding), size),
                (gimli::DW_AT_byte_size, AttributeValue::Udata(size)) => (name, encoding, Some(size)),
                _ => (name, encoding, size),
            }
        })

        // If the `entry.attrs()` fallible iterator does fail, give an appropriate error
        .map_err(|_| BindingError::DwarfParseError(format!("base type attributes at 0x{:08X}", entry.offset().0.into_u64())).into())

        // If we iterated through all attributes successfully, unwrap each property in the tuple
        .and_then(|(name, encoding, size)| {

            // If the name is Some, convert it to Cow<str>, then convert it to a String
            let name = name.and_then(|name| name.to_string().map(|name| (*name).to_owned()).ok());

            // For some reason the type "int" fails the name lookup in the strings section.
            // If the name is None but the encoding is "signed", hardcode the name to "int".
            let name = if name.is_some() { name } else {
                if let Some(gimli::DW_ATE_signed) = encoding {
                    Some("int".to_owned())
                } else {
                    None
                }
            };

            // Unwrap the elements in the tuple.
            name.ok_or(BindingError::DwarfParseError(format!("base type name at 0x{:08X}", entry.offset().0.into_u64())).into())
                .and_then(|name|
                    size.ok_or(BindingError::DwarfParseError(format!("base type size at 0x{:08X}", entry.offset().0.into_u64())).into())
                        .map(|size| (name, size)))
        })

        // Use the name, encoding, and size to build a DwarfType representation of this entry.
        .map(|(name, size)| (entry.offset().0.into_u64(), Type::Base { name, size }))
}

/// Parses a pointer type from a `DW_TAG_pointer_type` entry in the DIE tree.
fn parse_pointer_type<'a, R: Reader>(entry: &'a DebuggingInformationEntry<R, R::Offset>) -> Result<UnresolvedReference, Error> {
    entry.attrs()

        // Iterate over the attributes of the entry, collecting the size and type.
        .fold((None, None), |(size, typ), attr| {
            match (attr.name(), attr.value()) {
                (gimli::DW_AT_byte_size, AttributeValue::Udata(size)) => (Some(size), typ),
                (gimli::DW_AT_type, AttributeValue::UnitRef(typ)) => (size, Some(typ)),
                _ => (size, typ),
            }
        })

        // If the iterator fails, give an appropriate error.
        .map_err(|_| BindingError::DwarfParseError(format!("pointer type attributes at 0x{:08X}", entry.offset().0.into_u64())).into())

        // Unwrap the elements in the tuple.
        .and_then(|(size, typ)| {
            size.ok_or(BindingError::DwarfParseError(format!("pointer size at 0x{:08X}", entry.offset().0.into_u64())).into())
                .map(|size| (size, typ.map(|typ| typ.0.into_u64())))
        })

        // Map the size and type into an unresolved Type to resolve later.
        .map(|(_, typ)| {
            UnresolvedReference {
                typ,
                offset: entry.offset().0.into_u64(),
            }
        })
}

/// Parses an alias from a `DW_TAG_typedef` entry in the DIE tree.
fn parse_typedef<'a, R: Reader>(entry: &'a DebuggingInformationEntry<R, R::Offset>, strings: &'a DebugStr<R>) -> Result<UnresolvedAlias, Error> {
    entry.attrs()

        // Iterate over the attributes of the entry, collecting the name and type.
        .fold((None, None), |(name, typ), attr| {
            match (attr.name(), attr.value()) {
                (gimli::DW_AT_name, AttributeValue::DebugStrRef(name)) => (strings.get_str(name).ok(), typ), // Used for elf files
                (gimli::DW_AT_name, AttributeValue::String(name)) => (Some(name), typ), // Used for macho files
                (gimli::DW_AT_type, AttributeValue::UnitRef(typ)) => (name, Some(typ)),
                _ => (name, typ),
            }
        })

        // If the `entry.attrs()` fallible iterator does fail, give an appropriate error
        .map_err(|_| BindingError::DwarfParseError(format!("typedef attributes at 0x{:08X}", entry.offset().0.into_u64())).into())

        // If we iterated through all attributes successfully, unwrap each property in the tuple
        .and_then(|(name, typ)| {

            // If the name is Some, convert it to Cow<str>, then convert it to a String
            let name = name.and_then(|name| name.to_string().map(|name| (*name).to_owned()).ok());

            // Unwrap the elements in the tuple
            name.ok_or(BindingError::DwarfParseError(format!("typedef name at 0x{:08X}", entry.offset().0.into_u64())).into())
                .and_then(|name|
                    typ.ok_or(BindingError::DwarfParseError(format!("typedef type at 0x{:08X}", entry.offset().0.into_u64())).into())
                        .map(|typ| (name, typ)))
        })

        // Map the parsed entry into an UnresolvedAlias
        .map(|(name, typ)| {
            UnresolvedAlias {
                name,
                typ: typ.0.into_u64(),
                offset: entry.offset().0.into_u64(),
            }
        })
}

/// Parses a parameter from a `DW_TAG_formal_parameter` entry in the DIE tree.
fn parse_parameter<'a, R: Reader>(entry: &'a DebuggingInformationEntry<R, R::Offset>, strings: &'a DebugStr<R>) -> Result<UnresolvedParameter, Error> {
    entry.attrs()

        // Iterate over the attributes of the entry, collecting the name and type.
        .fold((None, None), |(name, typ), attr| {
            match (attr.name(), attr.value()) {
                (gimli::DW_AT_name, AttributeValue::DebugStrRef(name)) => (strings.get_str(name).ok(), typ), // Used for elf files
                (gimli::DW_AT_name, AttributeValue::String(name)) => (Some(name), typ), // Used for macho files
                (gimli::DW_AT_type, AttributeValue::UnitRef(typ)) => (name, Some(typ)),
                _ => (name, typ),
            }
        })

        // If the `entry.attrs()` fallible iterator does fail, give an appropriate error
        .map_err(|_| BindingError::DwarfParseError(format!("parameter attributes at 0x{:08}", entry.offset().0.into_u64())).into())

        // If we iterated through all attributes successfully, unwrap each property in the tuple
        .and_then(|(name, typ)| {

            // If the name is Some, convert it to Cow<str>, then convert it to a String
            let name = name.and_then(|name| name.to_string().map(|name| (*name).to_owned()).ok());

            // Unwrap the elements in the tuple
            name.ok_or(BindingError::DwarfParseError(format!("parameter name at 0x{:08X}", entry.offset().0.into_u64())).into())
                .and_then(|name|
                    typ.ok_or(BindingError::DwarfParseError(format!("parameter type at 0x{:08X}", entry.offset().0.into_u64())).into())
                        .map(|typ| (name, typ)))
        })

        // Map the parsed entry into an UnresolvedParameter
        .map(|(name, typ)| {
            UnresolvedParameter {
                name,
                typ: Some(typ.0.into_u64()),
                offset: entry.offset().0.into_u64(),
            }
        })
}

/// Parses a subprogram from a `DW_TAG_subprogram` entry in the DIE tree.
fn parse_subprogram<'a, R: Reader>(entry: &'a DebuggingInformationEntry<R, R::Offset>, strings: &'a DebugStr<R>) -> Result<UnresolvedSubprogram, Error> {
    entry.attrs()

        // Iterate over the entry attributes to collect the function's name and address
        .fold((None, None, None), |(name, address, ret), attr| {
            match (attr.name(), attr.value()) {
                (gimli::DW_AT_name, AttributeValue::DebugStrRef(name)) => (strings.get_str(name).ok(), address, ret), // Used for elf files
                (gimli::DW_AT_name, AttributeValue::String(name)) => (Some(name), address, ret), // Used for macho files
                (gimli::DW_AT_low_pc, AttributeValue::Addr(a)) => (name, Some(a), ret),
                (gimli::DW_AT_type, AttributeValue::UnitRef(r)) => (name, address, Some(r)),
                _ => (name, address, ret),
            }
        })

        // If the `entry.attrs()` fallible iterator does fail, give an appropriate error.
        .map_err(|_| BindingError::DwarfParseError(format!("subprogram attributes at 0x{:08}", entry.offset().0.into_u64())).into())

        // If we iterated successfully through the attributes, unwrap each property in the tuple.
        .and_then(|(name, address, ret)| {
            let name = name.and_then(|name| name.to_string().map(|name| (*name).to_owned()).ok());

            // Unwrap the elements in the tuple.
            name.ok_or(BindingError::DwarfParseError(format!("subprogram name at 0x{:08X}", entry.offset().0.into_u64())).into())
                .and_then(|name|
                    address.ok_or(BindingError::DwarfParseError(format!("subprogram address at 0x{:08X}", entry.offset().0.into_u64())).into())
                        .map(|address| (name, address, ret.map(|ret| ret.0.into_u64()))))
        })

        // Map the tuple of properties into a DwarfFunction.
        .map(|(name, address, ret)| {
            UnresolvedSubprogram {
                name,
                address,
                parameters: Vec::new(),
                ret,
            }
        })
}

/// The dwarf parser is modeled as a State machine in order to better reason about
/// its behavior. This enum represents the states that the machine moves through
/// while parsing subprograms.
enum State {
    /// The parser is in the `Search` state when it is not currently at a
    /// `DW_TAG_subprogram` node or any child of a `DW_TAG_subprogram`. In this
    /// state, the only input the machine is looking for is an `Event::NewSubprogram`,
    /// at which point the machine transitions to `State::Read`.
    Search,
    /// The `Read` state indicates that the parser has located a `DW_TAG_subprogram`
    /// node and is now actively reading from it. The parser will remain in this state
    /// until one of two things happens:
    /// 1) The parser finds a new `DW_TAG_subprogram`, at which point the current
    ///    subprogram is saved and a new one begins. The machine remains in the `Read`
    ///    state, but begins reading a new subprogram.
    /// 2) The parser steps above the depth in the tree that the `DW_TAG_subprogram`
    ///    it's currently reading began. This means that incoming data is no longer
    ///    associated with the previous subprogram. The machine transitions back to
    ///    the `Search` state.
    Read {
        /// When the parser is in the `Read` state, it actively looks for properties
        /// to append to the current subprogram, such as `DW_AT_formal_parameter`s.
        /// All relevant data is appended to the `func` field of this state.
        func: UnresolvedSubprogram,
        /// When the parser begins reading a new subprogram, it notes the depth at
        /// which it found the subprogram so it knows when to stop associating data
        /// with that subprogram (when the DFS search steps above the subprogram).
        dep: isize,
    },
}

enum Event {
    NewSubprogram(isize),
    NewParameter,
    Step(isize),
}

struct DwarfParser {
    /// The state is held in an Option as a workaround for not being able to move
    /// the state out of a borrowed context. Instead, we use `Option.take` to
    /// "move" the state out and into the match statement, then we use
    /// `Option.get_or_insert` to put back the new state.
    state: Option<State>,
    subprograms: Vec<UnresolvedSubprogram>,
}

impl DwarfParser {
    fn new() -> Self {
        DwarfParser {
            state: Some(State::Search),
            subprograms: Vec::new(),
        }
    }

    fn step<'a, R: Reader>(&mut self, event: Event, entry: &'a DebuggingInformationEntry<R, R::Offset>, strings: &'a DebugStr<R>) -> Result<(), Error> {
        let state = match (self.state.take().unwrap(), event) {
            // If we were searching for a subprogram and have just found one,
            // begin reading the subprogram, noting its depth so we know when
            // to quit.
            (State::Search, Event::NewSubprogram(dep)) => {
                let func = parse_subprogram(entry, strings)?;
                State::Read { func, dep }
            }
            // If we're reading a subprogram and find a new parameter, push
            // the parameter onto our parameter list and continue searching
            // for more parameters.
            (State::Read { mut func, dep, .. }, Event::NewParameter) => {
                let param = parse_parameter(entry, strings)?;
                func.parameters.push(param);
                State::Read { func, dep }
            }
            // If we were reading one subprogram but stepped up to a new one,
            // save the previous function we built and begin a new one.
            (State::Read { func, .. }, Event::NewSubprogram(dep)) => {
                self.subprograms.push(func);
                let func = parse_subprogram(entry, strings)?;
                State::Read { func, dep }
            }
            // If we step up in the DIE tree to a depth higher than the one we
            // began reading this subprogram in, save the function we were building
            // and begin searching for the next subprogram.
            (State::Read { func, dep, .. }, Event::Step(depth)) => {
                if depth < dep {
                    self.subprograms.push(func);
                    State::Search
                } else {
                    State::Read { func, dep }
                }
            }
            (state, _) => state,
        };
        self.state.get_or_insert(state);
        Ok(())
    }

    /// If, at the end of reading an entry, there is a function that has yet to
    /// be saved, save it.
    fn step_zero(&mut self) {
        if let State::Read { func, .. } = self.state.take().unwrap() {
            self.subprograms.push(func);
        }
        self.state.get_or_insert(State::Search);
    }
}

/// A thin wrapper around HashMap for storing parsed Types.
///
/// This is used primarily as a means to store a common `void`
/// type. Previously, Types were represented as `Option`s, and
/// `void` was simply a `None` variant. This led to unnecessarily
/// complicated code, so now instead we use the concrete `void`
/// type.
struct TypeRegistry {
    types: HashMap<u64, Rc<Type>>,
    void: Rc<Type>,
}

impl Deref for TypeRegistry {
    type Target = HashMap<u64, Rc<Type>>;
    fn deref(&self) -> &Self::Target {
        &self.types
    }
}

impl DerefMut for TypeRegistry {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.types
    }
}

impl TypeRegistry {
    /// Creates a new, empty `TypeRegistry`.
    fn new() -> Self {
        TypeRegistry {
            types: HashMap::new(),
            void: Rc::new(Type::Base{ name: "void".to_owned(), size: 0 }),
        }
    }

    /// Returns a reference to the `void` type.
    fn void(&self) -> Rc<Type> {
        self.void.clone()
    }
}

/// Parses the buffer of a DWARF binary to extract the debugging information.
pub fn parse(buffer: &[u8]) -> Result<Vec<Function>, Error> {
    let bin = object::File::parse(buffer)
        .map_err(|_| BindingError::DwarfReadError("binary file".to_owned()))?;

    let endian = if bin.is_little_endian() {
        gimli::RunTimeEndian::Little
    } else {
        gimli::RunTimeEndian::Big
    };

    let debug_info = bin.section_data_by_name(".debug_info")
        .map(|info| DebugInfo::new(info, endian))
        .ok_or(BindingError::DwarfReadError(".debug_info".to_owned()))?;

    let debug_abbrev = bin.section_data_by_name(".debug_abbrev")
        .map(|abbrev| DebugAbbrev::new(abbrev, endian))
        .ok_or(BindingError::DwarfReadError(".debug_abbrev".to_owned()))?;

    let debug_strings = bin.section_data_by_name(".debug_str")
        .map(|strings| DebugStr::new(strings, endian))
        .ok_or(BindingError::DwarfReadError(".debug_str".to_owned()))?;

    let mut resolved_types = TypeRegistry::new();
    let mut unresolved_aliases: HashMap<u64, UnresolvedAlias> = HashMap::new();
    let mut unresolved_references: HashMap<u64, UnresolvedReference> = HashMap::new();

    let mut parser = DwarfParser::new();
    let mut units = debug_info.units();

    while let Some(unit) = units.next()? {
        let abbrevs = unit.abbreviations(&debug_abbrev)?;
        let mut entries = unit.entries(&abbrevs);
        let mut depth = 0;

        while let Some((delta, entry)) = entries.next_dfs()? {
            depth += delta;
            match (depth, entry.tag()) {
                (_, gimli::DW_TAG_base_type) => {
                    let (offset, typ) = parse_base_type(&entry, &debug_strings)?;
                    resolved_types.insert(offset, Rc::new(typ));
                },
                (_, gimli::DW_TAG_pointer_type) => {
                    let reference = parse_pointer_type(&entry)?;
                    let offset = reference.offset;
                    unresolved_references.insert(offset, reference);
                },
                (_, gimli::DW_TAG_typedef) => {
                    let alias = parse_typedef(&entry, &debug_strings)?;
                    let offset = alias.offset;
                    unresolved_aliases.insert(offset, alias);
                },
                (_, gimli::DW_TAG_formal_parameter) => {
                    parser.step(Event::NewParameter, &entry, &debug_strings)?;
                },
                (depth, gimli::DW_TAG_subprogram) => {
                    parser.step(Event::NewSubprogram(depth), &entry, &debug_strings)?;
                },
                (depth, _) => parser.step(Event::Step(depth), &entry, &debug_strings)?,
            }
        }
        parser.step_zero();
    }

    // Resolve all typedefs/aliases
    for (offset, unresolved_alias) in unresolved_aliases.into_iter() {
        let resolved_alias = unresolved_alias.into_resolved(&resolved_types)?;
        resolved_types.insert(offset, Rc::new(resolved_alias));
    }

    // Resolve all pointers/references
    for (offset, unresolved_type) in unresolved_references.into_iter() {
        let resolved_type = unresolved_type.into_resolved(&resolved_types)?;
        resolved_types.insert(offset, Rc::new(resolved_type));
    }

    // Resolve all subprograms
    let unresolved_subprograms = parser.subprograms;
    let mut resolved_subprograms = Vec::<Function>::new();
    for subprogram in unresolved_subprograms.into_iter() {
        resolved_subprograms.push(subprogram.into_resolved(&resolved_types)?)
    }

    Ok(resolved_subprograms)
}

/// Test the dwarf parser for correctness.
///
/// These tests rely on files in `test_resources/`, so any changes to that
/// directory may break tests.
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parser() {
        let dwarf: &[u8] = include_bytes!("./test_resources/dwarf_parse_test");
        let result = parse(dwarf);
        assert!(result.is_ok());

        // Expected values //

        // Base types
        let void = Rc::new(Type::Base { name: "void".to_owned(), size: 0 });
        let b0 = Rc::new(Type::Base { name: "int".to_owned(), size: 4 });
        let b1 = Rc::new(Type::Base { name: "unsigned char".to_owned(), size: 1 });
        let b2 = Rc::new(Type::Base { name: "short unsigned int".to_owned(), size: 2 });
        let b3 = Rc::new(Type::Base { name: "unsigned int".to_owned(), size: 4 });
        let b4 = Rc::new(Type::Base { name: "char".to_owned(), size: 1 });

        // Alias types
        let a0 = Rc::new(Type::Alias { name: "uint8_t".to_owned(), typ: b1.clone() });
        let a1 = Rc::new(Type::Alias { name: "uint16_t".to_owned(), typ: b2.clone() });
        let a2 = Rc::new(Type::Alias { name: "uint32_t".to_owned(), typ: b3.clone() });

        // Reference types
        let r0 = Rc::new(Type::Reference { typ: b4.clone() });

        // Parameters
        let p0 = Parameter { name: "first".to_owned(), typ: a0.clone() };
        let p1 = Parameter { name: "second".to_owned(), typ: a1.clone() };
        let p2 = Parameter { name: "third".to_owned(), typ: a2.clone() };
        let p3 = Parameter { name: "letter".to_owned(), typ: b4.clone() };
        let p4 = Parameter { name: "count".to_owned(), typ: b0.clone() };

        // Subprograms
        let expected_subprograms = vec![
            Function { name: "main".to_owned(), address: 1692, parameters: vec![], ret: b0.clone() },
            Function { name: "test_four".to_owned(), address: 1665, parameters: vec![p0, p1, p2], ret: r0.clone() },
            Function { name: "test_three".to_owned(), address: 1649, parameters: vec![p3], ret: b0.clone() },
            Function { name: "test_two".to_owned(), address: 1639, parameters: vec![p4], ret: void.clone() },
            Function { name: "test_one".to_owned(), address: 1632, parameters: vec![], ret: void.clone() },
        ];

        // Actual values //

        let actual_subprograms = result.unwrap();

        // Compare
        assert_eq!(actual_subprograms, expected_subprograms);
    }

    #[test]
    fn test_parser_macho() {
        let dwarf: &[u8] = include_bytes!("./test_resources/dwarf_parse_test_macho");
        let result = parse(dwarf);
        assert!(result.is_ok());

        // Expected values //

        // Base types
        let b0 = Rc::new(Type::Base { name: "int".to_owned(), size: 4 });
        let b1 = Rc::new(Type::Base { name: "char".to_owned(), size: 1 });
        let b2 = Rc::new(Type::Base { name: "long int".to_owned(), size: 8 });

        // Parameters
        let p0 = Parameter { name: "a".to_owned(), typ: b0.clone() };
        let p1 = Parameter { name: "b".to_owned(), typ: b1.clone() };
        let p2 = Parameter { name: "c".to_owned(), typ: b2.clone() };

        let expected_subprograms = vec![
            Function { name: "test".to_owned(), address: 0, parameters: vec![p0, p1, p2], ret: b0.clone() },
        ];

        // Actual values //

        let actual_subprograms = result.unwrap();

        // Compare
        assert_eq!(actual_subprograms, expected_subprograms);
    }
}