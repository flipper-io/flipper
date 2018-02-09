//! Flipper Device Firmware Upgrade
//!
//! The `fdfu` crate defines how to flash new firmware onto Flipper's hardware.
//! It utilizes Flipper's own Rust language bindings to perform the necessary
//! GPIO and UART hardware interactions to enter update mode, copy the firmware,
//! and reboot the board.

#![allow(unsafe_code)]

use std::io::{Read, Write, Cursor};
use std::ffi::CString;
use std::{thread, time};
use byteorder::{BigEndian, ReadBytesExt};
use xmodem;
use xmodem::Xmodem;
use failure::Error;

use flipper::Flipper;
use flipper::StandardModule;
use flipper::fsm::uart0::{Uart0, UartBaud};
use flipper::fsm::gpio::Gpio;

#[derive(Debug, Fail)]
enum FdfuError {
    #[fail(display = "failed to enter normal mode")]
    NormalMode,
    #[fail(display = "failed to enter update mode")]
    UpdateMode,
    #[fail(display = "failed to execute boot assistant command: {}", _0)]
    SamBaCommand(String),
    #[fail(display = "boot assistant did not respond. Expected {}", _0)]
    SamBaRead(String),
    #[fail(display = "failed to write {} to boot assistant", _0)]
    SamBaWrite(String),
    #[fail(display = "xmodem transfer failed, {}", _0)]
    XModem(String),
    #[fail(display = "device flash security bit is set")]
    SecurityBit,
    #[fail(display = "failed to upload page {} to flash", _0)]
    FlashWrite(usize),
    #[fail(display = "firmware verification failed. Found {} errors", _0)]
    Verify(usize),
}

impl From<xmodem::Error> for FdfuError {
    fn from(xe: xmodem::Error) -> Self {
        use xmodem::Error as XError;
        let message: String = match xe {
            XError::ExhaustedRetries => "ran out of retries".to_owned(),
            XError::Canceled => "sam unexpectedly cancelled transmission".to_owned(),
            XError::Io(ioe) => ioe.to_string(),
        };
        FdfuError::XModem(message)
    }
}

const IRAM_ADDR: u32 = 0x20000000;
const IRAM_SIZE: u32 = 0x00020000;
const IFLASH0_ADDR: u32 = 0x00400000;

const APPLET_ADDR: u32 = IRAM_ADDR + 0x800;
const APPLET_STACK: u32 = APPLET_ADDR;
const APPLET_ENTRY: u32 = APPLET_ADDR + 0x04;
const APPLET_DESTINATION: u32 = APPLET_ADDR + 0x30;
const APPLET_SOURCE: u32 = APPLET_DESTINATION + 0x04;
const APPLET_PAGE: u32 = APPLET_SOURCE + 0x04;
const PAGE_BUFFER: &'static Fn(usize) -> u32 = &|applet_size| (APPLET_ADDR + applet_size as u32);

const SAM_ERASE_PIN: u32 = 0x06;
const SAM_RESET_PIN: u32 = 0x05;

const EFC0: u32 = 0x400E0A00;
const EEFC_FCR: u32 = EFC0 + 0x04;
const EEFC_FSR: u32 = EFC0 + 0x08;
const EEFC_FRR: u32 = EFC0 + 0x0C;

// EEFC Commands
const EFC_SGPB: u8 = 0x0B; // Set General-Purpose Non-Volatile Memory (GPNVM) bit
const EFC_GGPB: u8 = 0x0D; // Get GPNVM bit

const EEFC_FCR_FCMD_POS: u32 = 0;
const EEFC_FCR_FARG_POS: u32 = 8;
const EEFC_FCR_FKEY_POS: u32 = 24;

const EEFC_FCR_FCMD_MSK: u32 = 0xff << EEFC_FCR_FCMD_POS;
const EEFC_FCR_FARG_MSK: u32 = 0xffff << EEFC_FCR_FARG_POS;
const EEFC_FCR_FKEY_MSK: u32 = 0xff << EEFC_FCR_FKEY_POS;

const EEFC_FCR_FCMD: &'static Fn(u32) -> u32 = &|value| EEFC_FCR_FCMD_MSK & (value << EEFC_FCR_FCMD_POS);
const EEFC_FCR_FARG: &'static Fn(u32) -> u32 = &|value| EEFC_FCR_FARG_MSK & (value << EEFC_FCR_FARG_POS);
const EEFC_FCR_FKEY: &'static Fn(u32) -> u32 = &|value| EEFC_FCR_FKEY_MSK & (value << EEFC_FCR_FKEY_POS);

/// The SAM Boot Assistant is the mechanism for uploading and executing code
/// to the ATSAM4S used on Flipper. This struct is a toolkit for interacting
/// with the SAM-BA, and includes references to the Copy Applet binary, the
/// Uart0 bus used to talk to the SAM-BA, and the Gpio driver used to set
/// required modes on the SAM.
struct SamBa<'a> {
    applet: &'a [u8],
    bus: &'a mut Uart0,
    gpio: &'a mut Gpio,
}

impl<'a> SamBa<'a> {
    fn new(bus: &'a mut Uart0, gpio: &'a mut Gpio) -> SamBa<'a> {
        let applet: &[u8] = include_bytes!("./copy.bin");
        SamBa { applet, bus, gpio }
    }

    /// Tells the SAM to branch to a given address and begin executing code.
    /// This is used to instruct the copy applet to copy a page of data into
    /// flash.
    fn jump(&mut self, address: u32) -> Result<(), Error> {
        let command = CString::new(format!("G{:08X}#", address)).unwrap();
        self.bus.write(command.as_bytes()).map(|_| ())
            .map_err(|_| FdfuError::SamBaCommand(format!("jump to {:08X}", address)).into())
    }

    fn _write_byte(&mut self, address: u32, byte: u8) -> Result<(), Error> {
        let command = CString::new(format!("O{:08X},{:02}#", address, byte)).unwrap();
        self.bus.write(command.as_bytes()).map(|_| ())
            .map_err(|_| FdfuError::SamBaCommand(format!("write byte {:02X} to {:08X}", byte, address)).into())
    }

    /// Writes a 4 byte word to a given address in the ATSAM's memory.
    fn write_word(&mut self, address: u32, word: u32) -> Result<(), Error> {
        let command = CString::new(format!("W{:08X},{:08X}#", address, word)).unwrap();
        self.bus.write(command.as_bytes()).map(|_| ())
            .map_err(|_| FdfuError::SamBaCommand(format!("write word {:08X} to {:08X}", word, address)).into())
    }

    /// Writes a flash command to the ATSAM.
    ///
    /// See the EEFC section of the ATSAM4S16B datasheet.
    fn write_efc_fcr(&mut self, command: u8, arg: u32) -> Result<(), Error> {
        let word = EEFC_FCR_FKEY(0x5A) | EEFC_FCR_FARG(arg) | EEFC_FCR_FCMD(command as u32);
        self.write_word(EEFC_FCR, word)
    }

    /// Reads a byte from a given address in the ATSAM's memory.
    fn read_byte(&mut self, address: u32) -> Result<u8, Error> {
        let command = CString::new(format!("o{:08X},#", address)).unwrap();
        self.bus.write(command.as_bytes())?;

        let mut buffer = [0];
        self.bus.read_exact(&mut buffer)
            .map_err(|_| FdfuError::SamBaRead(format!("a byte from {:08X}", address)))?;
        Ok(buffer[0])
    }

    /// Reads a 4 byte word from a given address in the ATSAM's memory.
    fn read_word(&mut self, address: u32) -> Result<u32, Error> {
        let command = CString::new(format!("w{:08X},#", address)).unwrap();
        self.bus.write(command.as_bytes())?;

        let mut buffer = [0u8; 4];
        self.bus.read(&mut buffer)
            .map_err(|_| FdfuError::SamBaRead(format!("a word from {:08X}", address)))?;
        let word = Cursor::new(buffer).read_u32::<BigEndian>().unwrap();
        Ok(word)
    }

    /// Reset the ATSAM chip.
    ///
    /// This may be necessary if it doesn't immediately switch to update or normal mode.
    fn reset(&mut self) {
        self.gpio.write(0, 1 << SAM_RESET_PIN);
        thread::sleep(time::Duration::from_millis(10));
        self.gpio.write(1 << SAM_RESET_PIN, 0);
    }

    /// Toggles the right GPIO pins to cause the ATSAM to enter DFU mode.
    fn enter_dfu(&mut self) {
        self.gpio.write(1 << SAM_ERASE_PIN, 0);
        thread::sleep(time::Duration::from_millis(8000));
        self.gpio.write(0, 1 << SAM_ERASE_PIN);
        self.reset();
    }

    /// Sets the ATSAM into Device Firmware Update (DFU) mode.
    fn enter_update_mode(&mut self) -> Result<(), Error> {
        debug!("Entering update mode");
        self.bus.configure(UartBaud::DFU, false);

        let mut ack = [0u8; 3];
        for _ in 0..3 {
            self.bus.write(&[b'#']).map_err(|_| FdfuError::SamBaWrite("update mode command '#'".to_owned()))?;
            self.bus.read_exact(&mut ack).map_err(|_| FdfuError::SamBaRead("update mode ack 0x0A0D3E".to_owned()))?;

            if &ack[..] == b"\n\r>" {
                info!("Successfully entered update mode");
                return Ok(())
            }
            self.enter_dfu();
        }

        Err(FdfuError::UpdateMode.into())
    }

    /// Sets the ATSAM into normal mode (i.e. not firmware update mode).
    fn enter_normal_mode(&mut self) -> Result<(), Error> {
        debug!("Entering normal mode");
        self.bus.configure(UartBaud::DFU, false);

        let mut ack = [0u8; 2];
        for _ in 0..8 {
            self.bus.write(b"N#").map_err(|_| FdfuError::SamBaWrite("normal mode command 'N#'".to_owned()))?;
            self.bus.read_exact(&mut ack).map_err(|_| FdfuError::SamBaRead("normal mode ack 0x0A0D".to_owned()))?;

            if ack == [0x0A, 0x0D] {
                info!("Successfully entered normal mode");
                return Ok(());
            }
        }
        Err(FdfuError::NormalMode.into())
    }

    /// Moves data from the host to the device's RAM using the SAM-BA
    /// and XMODEM protocol.
    fn copy(&mut self, address: u32, data: &[u8]) -> Result<(), Error> {
        debug!("Copying {} bytes to 0x{:08}", data.len(), address);
        let command = CString::new(format!("S{:08X},{:08X}#", address, data.len())).unwrap();
        self.bus.write(command.as_bytes())?;

        let mut xmodem = Xmodem::new();
        xmodem.pad_byte = 0x0;
        let mut stream = Cursor::new(data);
        xmodem.send(&mut self.bus, &mut stream).map_err(|e| FdfuError::from(e).into())
    }

    /// Uploads the given buffer of data into the given address on the ATSAM.
    ///
    /// Due to the nature of the SAM-BA and Flash on the ATSAM, there are several steps:
    ///
    /// 1) Upload a copy applet into the device's RAM using the SAM Boot Assistant (SAM-BA).
    /// 2) While there is more data to flash:
    ///    a) Upload a 512-byte page of the firmware into RAM using the SAM-BA
    ///    b) Configure the copy applet to copy the page to an address in internal flash
    ///    c) Use SAM-BA to execute the copy applet
    fn upload(&mut self, destination: u32, data: &[u8]) -> Result<(), Error> {
        self.enter_update_mode()?;
        self.enter_normal_mode()?;

        // Check the device's security bit
        self.write_efc_fcr(EFC_GGPB, 0x0)?;
        let word = self.read_word(EEFC_FRR)?;
        if word & 0x01 != 0 { Err(FdfuError::SecurityBit)? }

        // Write the copy applet into RAM
        self.copy(APPLET_ADDR, self.applet)?;

        // Configure the applet's stack, entry point, destination, and source
        self.write_word(APPLET_STACK, IRAM_ADDR + IRAM_SIZE)?;
        self.write_word(APPLET_ENTRY, APPLET_ADDR + 0x09)?;
        self.write_word(APPLET_DESTINATION, destination)?;
        self.write_word(APPLET_SOURCE, PAGE_BUFFER(self.applet.len()))?;

        // Send the firmware, page by page
        for (i, page) in data.chunks(512).enumerate() {
            debug!("Writing page {}", i);
            self.copy(PAGE_BUFFER(self.applet.len()), page)?;
            self.write_word(APPLET_PAGE, EEFC_FCR_FARG(i as u32))?;
            self.jump(APPLET_ADDR)?;
            for _ in 0..4 {
                let fsr: u8 = self.read_byte(EEFC_FSR)?;
                if fsr & 0x01 != 0 { break; }
                if fsr & 0x0E != 0 { Err(FdfuError::FlashWrite(i))? }
            }
        };

        let retries = 4u8;
        for i in 0..retries {
            // Set the GPNVM1 to boot from flash memory.
            self.write_efc_fcr(EFC_SGPB, 0x1)?;

            // Check that the GPNVM1 bit is set.
            self.write_efc_fcr(EFC_GGPB, 0x0)?;
            let byte = self.read_byte(EEFC_FRR)?;
            if byte & (1 << 1) != 0 { break }

            // If the bit is still not set after all retries, return an error
            if i >= retries - 1 { Err(FdfuError::SecurityBit)? }
        }

        Ok(())
    }

    /// Verifies that the data from the given buffer matches the data on the ATSAM
    /// beginning at the base Flash address of the device. This is used to check that
    /// a firmware image was uploaded correctly.
    fn verify(&mut self, data: &[u8]) -> Result<usize, Error> {
        let word_size = 4;
        let mut errors = 0;
        for (word_count, word_bytes) in data.chunks(word_size).enumerate() {
            let address = IFLASH0_ADDR + (word_count * word_size) as u32;
            let sam_word = self.read_word(address)?;
            let firmware_word = Cursor::new(word_bytes).read_u32::<BigEndian>().unwrap();

            if sam_word != firmware_word {
                errors += 1;
            }
        }

        Ok(errors)
    }
}

/// Given a binary buffer, flash the contents of the buffer onto Flipper.
pub fn flash(firmware: &[u8]) -> Result<(), Error> {
    let flipper = Flipper::attach();
    flipper.select_u2_gpio();

    let mut bus = Uart0::new();
    bus.configure(UartBaud::DFU, false);
    let mut gpio = Gpio::new();
    let mut samba = SamBa::new(&mut bus, &mut gpio);
    samba.upload(IFLASH0_ADDR, &firmware)?;

    println!("Flash successful");
    Ok(())
}

/// Given a binary buffer, verify that the boot image on Flipper matches.
pub fn verify(firmware: &[u8]) -> Result<(), Error> {
    let flipper = Flipper::attach();
    flipper.select_u2_gpio();

    let mut bus = Uart0::new();
    bus.configure(UartBaud::DFU, false);
    let mut gpio = Gpio::new();
    let mut samba = SamBa::new(&mut bus, &mut gpio);

    let error_count = samba.verify(&firmware)?;
    if error_count > 0 {
        Err(FdfuError::Verify(error_count).into())
    } else {
        Ok(())
    }
}
