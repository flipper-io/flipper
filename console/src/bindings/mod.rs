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

#![allow(missing_docs)]

pub mod meta;
pub mod parser;

use std::io::Read;
use std::ops::Range;
use failure::Error;
use gimli;
use goblin::elf::Elf;

/// Represents errors that can occur when parsing ELF and/or DWARF files.
#[derive(Debug, Fail)]
pub enum BindingError {
    #[fail(display = "failed to read elf file")]
    ElfReadError,
    #[fail(display = "failed to parse elf section: {}", _0)]
    ElfSectionError(String),
    #[fail(display = "failed to read dwarf section: {}", _0)]
    DwarfReadError(String),
    #[fail(display = "failed to parse dwarf {}", _0)]
    DwarfParseError(&'static str),
    #[fail(display = "failed to resolve {}", _0)]
    TypeResolutionError(&'static str),
}

/// Represents relevant binary sections of Flipper executables. This includes:
///
/// 1) ".lf.funcs", which stores the binary of functions which are FMR compatible.
#[derive(Debug)]
pub enum FlipperSection {
    /// Contains the functions which are capable of being executed via FMR.
    /// In C, these are functions which are annotated with LF_FUNC.
    ///
    /// ```c
    /// LF_FUNC uint8_t do_something(uint16_t arg_one) { ... } // Appears in .lf.funcs
    /// uint16_t do_something_else(uint32_t arg_one) { ... } // Not in .lf.funcs
    /// ```
    Funcs(Range<u64>),
    /// This variant is used only to suppress exhaustiveness checks for the
    /// test cases while there is only one FlipperSection variant. To be removed
    /// when a second variant is added.
    #[doc(hidden)]
    Other,
}

/// Parses a binary file and prints information about the sections.
pub fn parse_elf<R: Read>(file: &mut R) -> Result<FlipperSection, Error> {
    let buffer = {
        let mut v = Vec::new();
        file.read_to_end(&mut v).unwrap();
        v
    };
    read_section_offset(&buffer, ".lf.funcs")
}

/// Given the binary of an ELF file and the name of an ELF section, return
/// the address range of that section.
fn read_section_offset(buffer: &[u8], section: &str) -> Result<FlipperSection, Error> {
    let elf: Elf = Elf::parse(&buffer).map_err(|_| BindingError::ElfReadError)?;

    for header in &elf.section_headers {
        if let Some(Ok(name)) = elf.shdr_strtab.get(header.sh_name) {
            if name == section {
                let offset = header.sh_offset;
                let size = header.sh_size;
                return Ok(FlipperSection::Funcs((offset..offset + size)));
            }
        }
    }
    Err(BindingError::ElfSectionError(section.to_owned()).into())
}

mod test {
    use super::*;

    #[test]
    fn test_read_section_offset() {
        let elf: &[u8] = include_bytes!("./test_resources/elf_section_test");
        let result = read_section_offset(&elf, ".lf.funcs");
        assert!(result.is_ok());

        if let FlipperSection::Funcs(range) = result.unwrap() {
            assert_eq!(range.start, 0x0792);
            assert_eq!(range.end, 0x07B2);
        } else {
            panic!("expected to find '.lf.funcs' but did not");
        }
    }
}
