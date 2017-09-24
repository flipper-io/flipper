use std::fs::File;
use std::io::Read;
use goblin::elf::{Elf, Sym};
use gimli;
use object;

pub fn parse_elf(file: &mut File) {

    let buffer = { let mut v = Vec::new(); file.read_to_end(&mut v).unwrap(); v};

    let elf = Elf::parse(&buffer).unwrap();
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
}

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
