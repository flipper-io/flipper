//! Flipper Device Firmware Upgrade
//!
//! The `fdfu` crate defines how to flash new firmware onto Flipper's hardware.
//! It utilizes Flipper's own Rust language bindings to perform the necessary
//! GPIO and UART hardware interactions to enter update mode, copy the firmware,
//! and reboot the board.

use std::io::{Read, Write, Cursor};
use std::fs::File;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::{thread, time};
use std::io::Error as IoError;
use byteorder::{BigEndian, ReadBytesExt};
use xmodem;
use xmodem::Xmodem;
use failure::{Fail, Error};

use flipper;
use flipper::StandardModule;
use flipper::fsm::uart0::{Uart0, UartBaud};
use flipper::fsm::gpio;

#[derive(Debug, Fail)]
enum FdfuError {
    /// Indicates that an io::Error occurred involving a given file.
    /// _0: The name of the file involved.
    /// _1: The io::Error with details.
    #[fail(display = "File error: {}", _0)]
    FileError(IoError),
    #[fail(display = "Failed to enter normal mode")]
    NormalModeError,
    #[fail(display = "Failed to enter update mode")]
    UpdateModeError,
    #[fail(display = "Failed to copy data to SAM")]
    SambaCopyError,
    #[fail(display = "XModem transfer failed: {:?}", _0)]
    XModemError(xmodem::Error),
    #[fail(display = "The device's security bit is set")]
    SecurityBitError,
    #[fail(display = "Flash write error on page {}", _0)]
    FlashWriteError(usize)
}

const IRAM_ADDR: u32 = 0x20000000;
const IRAM_SIZE: u32 = 0x00020000;
const IFLASH_ADDR: u32 = 0x00400000;

const APPLET_ADDR: u32 = IRAM_ADDR + 0x800;
const APPLET_STACK: u32 = APPLET_ADDR;
const APPLET_ENTRY: u32 = APPLET_ADDR + 0x04;
const APPLET_DESTINATION: u32 = APPLET_ADDR + 0x30;
const APPLET_SOURCE: u32 = APPLET_DESTINATION + 0x04;
const APPLET_PAGE: u32 = APPLET_SOURCE + 0x04;
const APPLET_WORDS: u32 = APPLET_PAGE + 0x04;
const PAGE_BUFFER: &'static Fn(usize) -> u32 = &|applet_size| (APPLET_ADDR + applet_size as u32);

const EFC0: u32 = 0x400E0A00;
const EFC_SGPB: u8 = 0x0B;
const EFC_GGPB: u8 = 0x0D;
const EEFC_FCR: u32 = EFC0 + 0x04;
const EEFC_FSR: u32 = EFC0 + 0x08;
const EEFC_FRR: u32 = EFC0 + 0x0C;
const EEFC_FCR_FKEY_POS: u32 = 24;
const EEFC_FCR_FKEY_MSK: u32 = 0xff << EEFC_FCR_FKEY_POS;
const EEFC_FCR_FKEY: &'static Fn(u32) -> u32 = &|value| EEFC_FCR_FKEY_MSK & ((value) << EEFC_FCR_FKEY_POS);
const EEFC_FCR_FARG: &'static Fn(u32) -> u32 = &|arg| EEFC_FCR_FKEY_MSK & (arg << EEFC_FCR_FKEY_POS);
const EEFC_FCR_FCMD: &'static Fn(u32) -> u32 = &|command| EEFC_FCR_FKEY_MSK & (command << EEFC_FCR_FKEY_POS);

struct SamBa<'a> {
    applet: &'a [u8],
    bus: &'a mut Uart0,
}

impl<'a> SamBa<'a> {
    pub fn new(bus: &'a mut Uart0) -> SamBa<'a> {
        let applet: &[u8] = include_bytes!("./copy.bin");
        SamBa { applet, bus }
    }

    fn jump(&mut self, address: u32) {
        write!(self.bus, "G{:08x}#", address);
    }

    fn write_byte(&mut self, address: u32, byte: u8) {
        write!(self.bus, "O{:08x},{:02x}#", address, byte);
    }

    fn write_word(&mut self, address: u32, word: u32) {
        write!(self.bus, "W{:08x},{:08x}#", address, word);
    }

    fn write_efc_fcr(&mut self, command: u8, arg: u32) {
        let word = EEFC_FCR_FKEY(0x5A) | EEFC_FCR_FARG(arg) | EEFC_FCR_FCMD(command as u32);
        &self.write_word(EEFC_FCR, word);
    }

    fn read_byte(&mut self, address: u32) -> u8 {
        write!(self.bus, "o{:08x},#", address);

        let mut buffer = [0u8; 1];
        &self.bus.read(&mut buffer[..]);
        buffer[0]
    }

    fn read_word(&mut self, address: u32) -> u32 {
        write!(self.bus, "w{:08x},#", address);
        let mut buffer = [0u8; 4];
        &self.bus.read(&mut buffer[..]);
        Cursor::new(buffer).read_u32::<BigEndian>().unwrap()
    }

    fn enter_update_mode(&mut self) -> Result<(), Error> {
        println!("Setting bus to DFU baud");
        &self.bus.configure(&UartBaud::DFU, false);
        println!("Bus set to DFU baud");

        let mut ack = [0u8; 3];
        for _ in 0..3 {
            println!("Writing update command");
            &self.bus.write(&[b'#']);
            &self.bus.read_exact(&mut ack);
            println!("Received ack: {}{}{}", ack[0], ack[1], ack[2]);

            if &ack[..] == b"\n\r>" { return Ok(()) }
            sam_enter_dfu()
        }

        Err(FdfuError::UpdateModeError.into())
    }

    fn enter_normal_mode(&mut self) -> Result<(), Error> {
        println!("Setting bus to DFU baud");
        &self.bus.configure(&UartBaud::DFU, false);
        println!("Bus set to DFU baud");

        println!("Entering normal mode");
        let mut ack: [u8; 2] = [0; 2];
        for _ in 0..8 {
            &self.bus.write(b"N#");
            &self.bus.read_exact(&mut ack);
            if ack == [0x0A, 0x0D] {
                println!("Successfully entered normal mode");
                return Ok(());
            }
        }
        Err(FdfuError::NormalModeError.into())
    }

    /// Moves data from the host to the device's RAM using the SAM-BA
    /// and XMODEM protocol.
    fn copy_page(&mut self, address: u32, data: &[u8]) -> Result<(), Error> {
        write!(&mut self.bus, "S{:08X},{:08X}#", address, data.len());

        for _ in 0..8 {
            if !&self.bus.ready() { continue } else { break }
            return Err(FdfuError::SambaCopyError.into());
        }

        let mut buffer = [0u8; 1];
        &self.bus.read_exact(&mut buffer);
        let byte = buffer[0];
        if byte != b'C' {
            return Err(FdfuError::SambaCopyError.into());
        }

        let mut xmodem = Xmodem::new();
        let mut stream = Cursor::new(data);
        xmodem.send(&mut self.bus, &mut stream)
            .map_err(|e| FdfuError::XModemError(e).into())
    }

    fn upload(&mut self, destination: u32, data: &[u8]) -> Result<(), Error> {
        println!("Entering update mode");
        self.enter_update_mode()?;
        println!("Entering normal mode");
        self.enter_normal_mode()?;
        self.write_efc_fcr(EFC_GGPB, 0);
        let word = self.read_word(EEFC_FRR);
        if word & 0x01 != 0 { return Err(FdfuError::SecurityBitError.into()); }

        // Write the copy applet into RAM
        self.copy_page(APPLET_ADDR, self.applet);

        // Configure the applet's stack, entry point, destination, and source
        self.write_word(APPLET_STACK, IRAM_ADDR + IRAM_SIZE);
        self.write_word(APPLET_ENTRY, APPLET_ADDR + 0x09);
        self.write_word(APPLET_DESTINATION, destination);
        self.write_word(APPLET_SOURCE, PAGE_BUFFER(self.applet.len()));

        for (i, page) in data.chunks(512).enumerate() {
            self.copy_page(PAGE_BUFFER(self.applet.len()), page);
            self.write_word(APPLET_PAGE, EEFC_FCR_FARG(i as u32));
            self.jump(APPLET_ADDR);
            for _ in 0..4 {
                let fsr: u8 = self.read_byte(EEFC_FSR);
                if fsr & 0x01 != 0 { break; }
                if fsr & 0x0E != 0 { return Err(FdfuError::FlashWriteError(i).into()); }
            }
        };

        self.write_efc_fcr(EFC_SGPB, 0x01);
        self.write_efc_fcr(EFC_GGPB, 0x00);

        if self.read_byte(EEFC_FRR) & (1 << 1) == 0 {
            self.write_efc_fcr(EFC_SGPB, 0x01);
            self.write_efc_fcr(EFC_GGPB, 0x00);
        }

        Ok(())
    }
}

const SAM_ERASE_PIN: u32 = 0x06;
const SAM_RESET_PIN: u32 = 0x05;

fn sam_reset() {
    gpio::write(0, (1 << SAM_RESET_PIN));
    thread::sleep(time::Duration::from_millis(10));
    gpio::write((1 << SAM_RESET_PIN), 0);
}

fn sam_enter_dfu() {
    gpio::write((1 << SAM_ERASE_PIN), (1 << SAM_RESET_PIN));
    thread::sleep(time::Duration::from_millis(5000));
    gpio::write((1 << SAM_RESET_PIN), (1 << SAM_ERASE_PIN));
    sam_reset();
}

/// Given a binary buffer, flash the contents of the buffer onto Flipper.
pub fn flash(firmware: &[u8]) -> Result<(), Error> {
//    let flipper = flipper::Flipper::attach_hostname("localhost");
    let flipper = flipper::Flipper::attach();
    let mut bus = Uart0::bind(&flipper);
    let mut samba = SamBa::new(&mut bus);
    samba.upload(IFLASH_ADDR, &firmware)
}
