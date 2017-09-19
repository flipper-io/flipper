use std::ops::Deref;
use std::path::PathBuf;
use std::fmt::{Display, Formatter, Result};
use std::fs::File;
use std::io::Read;
use clap::{App, Arg, ArgMatches};
use goblin::elf::{Elf, Sym};

pub fn make_subcommands<'a, 'b>() -> Vec<App<'a, 'b>> {
    vec![
        bind::make_subcommand(),
    ]
}

pub fn execute(command: &str, args: &ArgMatches) {
    match command {
        "bind" => bind::execute(args),
        _ => println!("Unrecognized command!"),
    };
}

pub mod bind {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("bind")
            .arg(Arg::with_name("file")
                .index(1)
                .required(true)
            )
    }

    pub fn execute(args: &ArgMatches) {
        if let Some(filename) = args.value_of("file") {

            println!("flipper bind got {}", filename);
            let path = PathBuf::from(filename);
            let mut file = File::open(path).unwrap();

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
    }
}
