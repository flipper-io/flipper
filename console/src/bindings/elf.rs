use std::ops::Range;
use goblin::elf::Elf;
use failure::Error;

use bindings::BindingError;

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

impl FlipperSection {
    pub fn range(&self) -> Range<u64> {
        match *self {
            FlipperSection::Funcs(ref range) => range.clone(),
            _ => unimplemented!(),
        }
    }
}

/// Given the binary of an ELF file and the name of an ELF section, return
/// the address range of that section.
pub fn read_section_offset(buffer: &[u8], section: &str) -> Result<FlipperSection, Error> {
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
