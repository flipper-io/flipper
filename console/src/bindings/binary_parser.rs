//! Flipper module "public APIs" are exported in a binary section called
//! `.flipper_exports`. This module uses ELF and DWARF file metadata to read
//! the signatures of each exported function in a flipper module. This metadata
//! is the foundation for auto-generating high level language bindings to
//! remotely execute the modules.

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use failure::{Fail, Error};

use goblin::elf::{Elf, Sym};
use gimli;
use object;

#[derive(Debug, Fail)]
enum BinaryError {
    #[fail(display = "file '{}' is not valid DWARF", path)]
    InvalidDwarfFile {
        path: String,
    },
    #[fail(display = "failed to parse elf file")]
    ElfParseError
}

/// Parses a binary file and prints information about the sections.
pub fn parse_elf(file: &mut File) -> Result<(), Error> {

    let buffer = { let mut v = Vec::new(); file.read_to_end(&mut v).unwrap(); v};

    let elf = Elf::parse(&buffer)
        .map_err(|_| BinaryError::ElfParseError)?;
    let syms = elf.syms;
    let strtab = elf.strtab;
    let section_headers = elf.section_headers;
    let section_strings = elf.shdr_strtab;

    for sym in syms {

        let index = sym.st_shndx;

        if index >= section_headers.len() { continue; }
        if index == 0 { continue; }

        let section_header = &section_headers[index];
        let section_name = &section_strings[section_header.sh_name];

        println!("Index: {:2}   section: {:20} name: {:40} addr: {:08X}",
                 index,
                 section_name,
                 &strtab[sym.st_name],
                 sym.st_value
        );
    }

    Ok(())
}

/// Parses a file to extract the debugging information.
pub fn parse_dwarf(file: &mut File) {

    fn load_section<'input, 'file, S, Endian>(
        file: &'file object::File<'input>,
        endian: Endian,
    ) -> S
        where
            S: gimli::Section<gimli::EndianBuf<'input, Endian>>,
            Endian: gimli::Endianity,
            'file: 'input,
    {
        let data = file.get_section(S::section_name()).unwrap_or(&[]);
        S::from(gimli::EndianBuf::new(data, endian))
    }

    let buffer = { let mut v = Vec::new(); file.read_to_end(&mut v).unwrap(); v};
    let mut object_file = object::File::parse(&buffer);
}
