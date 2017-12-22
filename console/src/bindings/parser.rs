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

use std::io::Read;
use std::rc::Rc;
use std::string::ToString;
use std::collections::HashMap;

use failure::Error;
use fallible_iterator::FallibleIterator;
use object;
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

use super::BindingError;
use super::meta::{
    Type,
    Parameter,
    Subprogram,
};

/// Represents any parsed types which cannot be immediately resolved, such
/// as pointers. Base types are always immediately resolved.
#[derive(Debug)]
struct UnresolvedType {
    /// The offset into the DIE tree which represents the type of this entry.
    typ: Option<u64>,
    /// The offset into the DIE tree at which this entry lives.
    offset: u64,
}

impl UnresolvedType {
    /// Converts an `UnresolvedType` yielded by the parser into a fully
    /// resolved `Type`.
    fn into_resolved(self, resolved_types: &HashMap<u64, Rc<Type>>) -> Result<Type, Error> {
        Ok(match self.typ {
            None => Type::Reference { typ: None, offset: self.offset },
            Some(ref typ_offset) => {
                let typ = resolved_types.get(typ_offset).map(|rc| rc.clone());
                match typ {
                    Some(typ) => Type::Reference { typ: Some(typ), offset: self.offset },
                    None => Type::Unsupported,
                }
            },
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
    fn into_resolved(self, resolved_types: &HashMap<u64, Rc<Type>>) -> Result<Parameter, Error> {
        Ok(match self.typ {
            None => Parameter{ typ: None, name: self.name },
            Some(ref typ_offset) => {
                let typ = resolved_types.get(typ_offset).map(|rc| rc.clone())
                    .ok_or(BindingError::TypeResolutionError("parameter types"))?;
                Parameter{ typ: Some(typ), name: self.name }
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
    fn into_resolved(self, resolved_types: &HashMap<u64, Rc<Type>>) -> Result<Subprogram, Error> {
        let mut parameters: Vec<Parameter> = Vec::with_capacity(self.parameters.capacity());
        for parameter in self.parameters.into_iter() {
            parameters.push(parameter.into_resolved(resolved_types)?);
        }

        let ret = self.ret.and_then(|ref key| resolved_types.get(key).map(|rc| rc.clone()));

        Ok(Subprogram {
            name: self.name,
            address: self.address,
            parameters,
            ret,
        })
    }
}

/// Parses a base type from a `DW_TAG_base_type` entry in the DIE tree.
fn parse_base_type<'a, R: Reader>(entry: &'a DebuggingInformationEntry<R, R::Offset>, strings: &'a DebugStr<R>) -> Result<Type, Error> {
    entry.attrs()

        // Iterate over the attributes of the entry to collect the name, encoding, and size
        .fold((None, None, None), |(name, encoding, size), attr| {
            match (attr.name(), attr.value()) {
                (gimli::DW_AT_name, AttributeValue::DebugStrRef(name)) => (strings.get_str(name).ok(), encoding, size),
                (gimli::DW_AT_encoding, AttributeValue::Encoding(encoding)) => (name, Some(encoding), size),
                (gimli::DW_AT_byte_size, AttributeValue::Udata(size)) => (name, encoding, Some(size)),
                _ => (name, encoding, size),
            }
        })

        // If the `entry.attrs()` fallible iterator does fail, give an appropriate error
        .map_err(|_| BindingError::DwarfParseError("base type attributes").into())

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
            name.ok_or(BindingError::DwarfParseError("base type name").into())
                .and_then(|name|
                    encoding.ok_or(BindingError::DwarfParseError("base type encoding").into())
                        .and_then(|encoding|
                            size.ok_or(BindingError::DwarfParseError("base type size").into())
                                .map(|size| (name, encoding, size))))
        })

        // Use the name, encoding, and size to build a DwarfType representation of this entry.
        .map(|(name, encoding, size)| Type::Base { name, encoding, size, offset: entry.offset().0.into_u64() })
}

/// Parses a pointer type from a `DW_TAG_pointer_type` entry in the DIE tree.
fn parse_pointer_type<'a, R: Reader>(entry: &'a DebuggingInformationEntry<R, R::Offset>, strings: &'a DebugStr<R>) -> Result<UnresolvedType, Error> {
    entry.attrs()

        // Iterate over the attributes of the entry, collecting the size and type.
        .fold((None, None), |(size, typ), attr| {
            match (attr.name(), attr.value()) {
                (DW_AT_byte_size, AttributeValue::Udata(size)) => (Some(size), typ),
                (DW_AT_type, AttributeValue::UnitRef(typ)) => (size, Some(typ)),
                _ => (size, typ),
            }
        })

        // If the iterator fails, give an appropriate error.
        .map_err(|_| BindingError::DwarfParseError("pointer type attributes").into())

        // Unwrap the elements in the tuple.
        .and_then(|(size, typ)| {

            size.ok_or(BindingError::DwarfParseError("pointer size").into())
                .map(|size| (size, typ.map(|typ| typ.0.into_u64())))
        })

        // Map the size and type into an unresolved Type to resolve later.
        .map(|(size, typ)| {
            UnresolvedType {
                typ,
                offset: entry.offset().0.into_u64()
            }
        })
}

/// Parses a parameter from a `DW_TAG_formal_parameter` entry in the DIE tree.
fn parse_parameter<'a, R: Reader>(entry: &'a DebuggingInformationEntry<R, R::Offset>, strings: &'a DebugStr<R>) -> Result<UnresolvedParameter, Error> {
    entry.attrs()

        // Iterate over the attributes of the entry, collecting the name and type.
        .fold((None, None), |(name, typ), attr| {
            match (attr.name(), attr.value()) {
                (DW_AT_name, AttributeValue::DebugStrRef(name)) => (strings.get_str(name).ok(), typ),
                (DW_AT_type, AttributeValue::UnitRef(typ)) => (name, Some(typ)),
                _ => (name, typ),
            }
        })

        // If the `entry.attrs()` fallible iterator does fail, give an appropriate error
        .map_err(|_| BindingError::DwarfParseError("parameter attributes").into())

        // If we iterated through all attributes successfully, unwrap each property in the tuple
        .and_then(|(name, typ)| {

            // If the name is Some, convert it to Cow<str>, then convert it to a String
            let name = name.and_then(|name| name.to_string().map(|name| (*name).to_owned()).ok());

            // Unwrap the elements in the tuple
            name.ok_or(BindingError::DwarfParseError("parameter name").into())
                .and_then(|name|
                    typ.ok_or(BindingError::DwarfParseError("parameter type").into())
                        .map(|typ| (name, typ)))
        })

        // Map the parsed entry into a DwarfParameter
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
                (gimli::DW_AT_name, AttributeValue::DebugStrRef(name)) => (strings.get_str(name).ok(), address, ret),
                (gimli::DW_AT_low_pc, AttributeValue::Addr(a)) => (name, Some(a), ret),
                (gimli::DW_AT_type, AttributeValue::UnitRef(r)) => (name, address, Some(r)),
                _ => (name, address, ret),
            }
        })

        // If the `entry.attrs()` fallible iterator does fail, give an appropriate error.
        .map_err(|e| BindingError::DwarfParseError("subprogram attributes").into())

        // If we iterated successfully through the attributes, unwrap each property in the tuple.
        .and_then(|(name, address, ret)| {

            let name = name.and_then(|name| name.to_string().map(|name| (*name).to_owned()).ok());

            // Unwrap the elements in the tuple.
            name.ok_or(BindingError::DwarfParseError("subprogram name").into())
                .and_then(|name|
                    address.ok_or(BindingError::DwarfParseError("subprogram address").into())
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

enum State {
    Search,
    Read {
        func: UnresolvedSubprogram,
        dep: isize,
    },
}

enum Event {
    NewSubprogram {
        function: UnresolvedSubprogram,
        depth: isize,
    },
    NewParameter(UnresolvedParameter),
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

    fn step(&mut self, event: Event) {
        let state = match (self.state.take().unwrap(), event) {
            // If we were searching for a subprogram and have just found one,
            // begin reading the subprogram, noting its depth so we know when
            // to quit.
            (State::Search, Event::NewSubprogram { function, depth, .. }) => {
                State::Read { func: function, dep: depth }
            }
            // If we're reading a subprogram and find a new parameter, push
            // the parameter onto our parameter list and continue searching
            // for more parameters.
            (State::Read { mut func, dep, .. }, Event::NewParameter(param)) => {
                func.parameters.push(param);
                State::Read { func, dep }
            }
            // If we were reading one subprogram but stepped up to a new one,
            // save the previous function we built and begin a new one.
            (State::Read { func, .. }, Event::NewSubprogram { function, depth }) => {
                println!("Saved function: {:?}", func);
                self.subprograms.push(func);
                State::Read { func: function, dep: depth }
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
    }
}

/// Parses a file to extract the debugging information.
pub fn parse_dwarf<R: Read>(file: &mut R) -> Result<Vec<Subprogram>, Error> {
    let buffer = {
        let mut v = Vec::new();
        file.read_to_end(&mut v).unwrap();
        v
    };

    let elf = object::File::parse(&buffer).map_err(|_| BindingError::ElfReadError)?;

    let endian = if elf.is_little_endian() {
        gimli::RunTimeEndian::Little
    } else {
        gimli::RunTimeEndian::Big
    };

    let debug_info = elf.get_section(".debug_info")
        .map(|info| DebugInfo::new(info, endian))
        .ok_or(BindingError::DwarfReadError(".debug_info".to_owned()))?;

    let debug_abbrev = elf.get_section(".debug_abbrev")
        .map(|abbrev| DebugAbbrev::new(abbrev, endian))
        .ok_or(BindingError::DwarfReadError(".debug_abbrev".to_owned()))?;

    let debug_strings = elf.get_section(".debug_str")
        .map(|strings| DebugStr::new(strings, endian))
        .ok_or(BindingError::DwarfReadError(".debug_str".to_owned()))?;

    let mut resolved_types: HashMap<u64, Rc<Type>> = HashMap::new();
    let mut unresolved_types: HashMap<u64, UnresolvedType> = HashMap::new();

    let mut parser = DwarfParser::new();
    let mut units = debug_info.units();

    let mut errors: Vec<Error> = Vec::new();

    while let Some(unit) = units.next()? {
        let abbrevs = unit.abbreviations(&debug_abbrev)?;
        let mut entries = unit.entries(&abbrevs);
        let mut depth = 0;

        while let Some((delta, entry)) = entries.next_dfs()? {
            depth += delta;

            // Self-executing closure to catch Result::Err's thrown by '?'.
            let result: Result<(), Error> = (|| {
                match (depth, entry.tag()) {
                    (_, gimli::DW_TAG_base_type) => {
                        let typ = parse_base_type(&entry, &debug_strings)?;
                        let offset = match typ {
                            Type::Base { offset, .. } => offset,
                            _ => return Err(BindingError::DwarfParseError("base type").into()),
                        };
                        resolved_types.insert(offset, Rc::new(typ));
                    }
                    (_, gimli::DW_TAG_pointer_type) => {
                        let typ = parse_pointer_type(&entry, &debug_strings)?;
                        let offset = typ.offset;
                        unresolved_types.insert(offset, typ);
                    },
                    (_, gimli::DW_TAG_formal_parameter) => {
                        parser.step(Event::NewParameter(parse_parameter(&entry, &debug_strings)?));
                    },
                    (depth, gimli::DW_TAG_subprogram) => parser.step(Event::NewSubprogram {
                        function: parse_subprogram(&entry, &debug_strings)?,
                        depth,
                    }),
                    (depth, _) => parser.step(Event::Step(depth)),
                }
                Ok(())
            })();
            if let Err(e) = result {
                errors.push(e);
            }
        }
        parser.step(Event::Step(0));
    }

    if errors.len() > 0 {
        println!("Got errors while parsing:");
        for error in errors {
            println!("{}", error);
        }
    }

    for (offset, unresolved_type) in unresolved_types.into_iter() {
        let resolved_type = unresolved_type.into_resolved(&resolved_types)?;
        resolved_types.insert(offset, Rc::new(resolved_type));
    }

    let unresolved_subprograms = parser.subprograms;

    let mut resolved_subprograms = Vec::<Subprogram>::new();
    for subprogram in unresolved_subprograms.into_iter() {
        resolved_subprograms.push(subprogram.into_resolved(&resolved_types)?)
    }

    Ok(resolved_subprograms)
}
