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

use std::rc::Rc;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

use gimli::DwAte;

/// Represents a type in the program whose symbols are parsed. Types can be
/// concrete base types such as `uint8_t` and `int`, pointers such as `char *`,
/// or struct types. If a type is not a base type, it is represented by the
/// `Reference` variant of Type.
#[derive(Debug)]
pub enum Type {
    /// Base types encapsulate all of the information necessary to represent
    /// a value in the program they were parsed from. This is used to
    /// generate high level bindings using representations that match those
    /// of the low level code.
    Base {
        /// The name of this base type, e.g. `int` or `uint16_t`.
        name: String,
        /// The offset into the dwarf DIE tree that this type was defined.
        offset: u64,
        /// The memory footprint of this type, e.g. 4 for `int`.
        size: u64,
        /// The encoding of the value represented by this Type.
        encoding: DwAte,
    },
    /// A `Reference` is a type which is not complete on its own, but rather
    /// refers to one or more `Reference`s and eventually a `Base` type.
    /// This is used to represent pointers.
    Reference {
        /// A reference to another `Type`, or `None`. A `None` type is allowed
        /// because some types such as a `void *` are encoded in Dwarf as an
        /// entry with a size but no type.
        typ: Option<Rc<Type>>,
        /// The offset into the dwarf DIE tree that this type was defined.
        offset: u64,
    },
    /// There are additional types which may be parsed from a dwarf DIE tree,
    /// but until they're all supported, References which try to use those
    /// types will instead bottom out to the `Unsupported` variant.
    Unsupported,
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
#[derive(Debug)]
pub struct Parameter {
    /// The name of the formal parameter as defined in the original program,
    /// e.g. "greeting" in `void say_hello(char *greeting);`.
    pub name: String,
    /// The type of the parameter, e.g. a representation of the "char *" in
    /// `void say_hello(char *greeting);`. A type of `None` represents `void`.
    pub typ: Option<Rc<Type>>,
}

/// Represents a specific `subprogram` parsed from a DWARF file. The name will be
/// used for generating FFI bindings to this function. The address is captured
/// so it's possible to tell if the subprogram belongs to a certain binary section.
#[derive(Debug)]
pub struct Subprogram {
    /// The name of the function as defined in the original program,
    /// e.g. "say_hello" in `void say_hello(char *greeting);`.
    pub name: String,
    /// The address in the binary where this function's compiled code lives.
    pub address: u64,
    /// A list of the parameters of this function.
    pub parameters: Vec<Parameter>,
    /// The return type of this function. `None` indicates a `void` type.
    pub ret: Option<Rc<Type>>,
}
