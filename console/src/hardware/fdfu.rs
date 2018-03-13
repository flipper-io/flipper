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
use flipper::fsm::uart0::{Uart0, UartBaud};
use flipper::fsm::gpio::Gpio;

use std::sync::{
    Arc,
    mpsc,
};

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

/// Reports the progress of the flashing and/or verification of a firmware image.
/// Since the `flash` function is asynchronous, `Progress` variants are sent
/// through the returned channel in order to report the status of flashing.
#[derive(Debug)]
pub enum Progress {
    /// Indicates that the device was successfully placed in update mode.
    UpdateMode,
    /// Indicates that the device was successfully placed in normal mode.
    NormalMode,
    /// Indicates that the copy applet was successfully uploaded to the device.
    Applet,
    /// Indicates that page _0 was successfully flashed.
    Flashing(usize),
    /// Indicates that the flashing process completed without errors.
    FlashComplete,
    /// Indicates that word _0 has been checked. Note that this does _not_ mean
    /// that the word was verified to be correct. The total number of verification
    /// errors is reported by `VerifyComplete`.
    Verifying(usize),
    /// Indicates that the verification process completed without failure. The number
    /// _0 given is the number of words checked which did not match the verification
    /// image.
    VerifyComplete(usize),
    /// Indicates that some failure occurred while flashing or verifying the firmware.
    Failed(Error),
    /// Indicates that the flashing and/or verifying process completed successfully.
    Complete,
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

const SAM_RESET_PIN: u32 = 0x04;
const SAM_ERASE_PIN: u32 = 0x06;

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
struct SamBa<'a, 'b> {
    applet: &'a [u8],
    bus: Uart0<'a>,
    gpio: Gpio<'a>,
    sender: &'b mut mpsc::Sender<Progress>,
}

impl<'a, 'b> SamBa<'a, 'b> {
    fn new(bus: Uart0<'a>, gpio: Gpio<'a>, sender: &'b mut mpsc::Sender<Progress>) -> SamBa<'a, 'b> {
        let applet: &[u8] = include_bytes!("./copy.bin");
        SamBa { applet, bus, gpio, sender }
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
        self.bus.read_exact(&mut buffer)
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
        self.bus.configure(UartBaud::DFU, false);

        let mut ack = [0u8; 3];
        for _ in 0..3 {
            self.bus.write(&[b'#']).map_err(|_| FdfuError::SamBaWrite("update mode command '#'".to_owned()))?;
            self.bus.read_exact(&mut ack).map_err(|_| FdfuError::SamBaRead("update mode ack 0x0A0D3E".to_owned()))?;

            if &ack[..] == b"\n\r>" {
                debug!("Entered update mode");
                let _ = self.sender.send(Progress::UpdateMode);
                return Ok(())
            }
            self.enter_dfu();
        }

        Err(FdfuError::UpdateMode.into())
    }

    /// Sets the ATSAM into normal mode (i.e. not firmware update mode).
    fn enter_normal_mode(&mut self) -> Result<(), Error> {
        self.bus.configure(UartBaud::DFU, false);

        let mut ack = [0u8; 2];
        for _ in 0..8 {
            self.bus.write(b"N#").map_err(|_| FdfuError::SamBaWrite("normal mode command 'N#'".to_owned()))?;
            self.bus.read_exact(&mut ack).map_err(|_| FdfuError::SamBaRead("normal mode ack 0x0A0D".to_owned()))?;

            if ack == [0x0A, 0x0D] {
                debug!("Entered normal mode");
                let _ = self.sender.send(Progress::NormalMode);
                return Ok(());
            }
        }
        Err(FdfuError::NormalMode.into())
    }

    /// Moves data from the host to the device's RAM using the SAM-BA
    /// and XMODEM protocol.
    fn copy(&mut self, address: u32, data: &[u8]) -> Result<(), Error> {
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
    fn upload(&mut self, destination: u32, data: &[u8]) {
        info!("Uploading {} byte image to address 0x{:08X}", data.len(), destination);
        let result = (||{
            self.enter_update_mode()?;
            self.enter_normal_mode()?;

            // Check the device's security bit
            self.write_efc_fcr(EFC_GGPB, 0x0)?;
            let word = self.read_word(EEFC_FRR)?;
            if word & 0x01 != 0 { Err(FdfuError::SecurityBit)? }

            // Write the copy applet into RAM
            self.copy(APPLET_ADDR, self.applet)?;
            let _ = self.sender.send(Progress::Applet);

            // Configure the applet's stack, entry point, destination, and source
            self.write_word(APPLET_STACK, IRAM_ADDR + IRAM_SIZE)?;
            self.write_word(APPLET_ENTRY, APPLET_ADDR + 0x09)?;
            self.write_word(APPLET_DESTINATION, destination)?;
            self.write_word(APPLET_SOURCE, PAGE_BUFFER(self.applet.len()))?;

            // Send the firmware, page by page
            let page_size = 512;
            for (i, page) in data.chunks(page_size).enumerate() {
                debug!("Writing page {}", i);
                self.copy(PAGE_BUFFER(self.applet.len()), page)?;
                self.write_word(APPLET_PAGE, EEFC_FCR_FARG(i as u32))?;
                self.jump(APPLET_ADDR)?;
                for _ in 0..4 {
                    let fsr: u8 = self.read_byte(EEFC_FSR)?;
                    if fsr & 0x01 != 0 { break; }
                    if fsr & 0x0E != 0 { Err(FdfuError::FlashWrite(i))? }
                }
                let _ = self.sender.send(Progress::Flashing(i));
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
        })();

        let _ = match result {
            Ok(_) => self.sender.send(Progress::FlashComplete),
            Err(e) => self.sender.send(Progress::Failed(e)),
        };
    }

    /// Verifies that the data from the given buffer matches the data on the ATSAM
    /// beginning at the base Flash address of the device. This is used to check that
    /// a firmware image was uploaded correctly.
    fn verify(&mut self, data: &[u8]) {
        let result = (|| {
            let word_size = 4;
            let mut errors = 0;
            let words = data.len() / 4;
            for (word_count, word_bytes) in data.chunks(word_size).enumerate() {
                let firmware_word = Cursor::new(word_bytes).read_u32::<BigEndian>().unwrap();
                let address = IFLASH0_ADDR + (word_count * word_size) as u32;
                let sam_word = self.read_word(address)?;
                trace!("Verifying word {:04} of {:04} at address 0x{:08X}", word_count, words, address);
                let _ = self.sender.send(Progress::Verifying(word_count));

                // Compare the top 2 bytes of every word
                let mask = 0xFFFF0000;
                if sam_word & mask != firmware_word & mask {
                    errors += 1;
                }
            }
            Ok(errors)
        })();

        let _ = match result {
            Ok(num) => self.sender.send(Progress::VerifyComplete(num)),
            Err(e) => self.sender.send(Progress::Failed(e)),
        };
    }
}

/// Flashes the contents of `firmware` onto Flipper. If `verify` is true, also read
/// back each byte in memory from the device and compare it to the original firmware.
///
/// Returns a channel receiver of `Progress`, which reports the status of the flashing
/// process. This is arranged this way so that the console binary can simply display
/// a TUI output based on progress, but any library consumers can programmatically
/// watch progress (and respond accordingly) without their standard output being polluted.
pub fn flash(firmware: Arc<[u8]>, verify: bool) -> mpsc::Receiver<Progress> {

    let (mut sender, receiver) = mpsc::channel();

    thread::spawn(move || {
        let flipper = match Flipper::attach() {
            Ok(flipper) => flipper,
            Err(e) => {
                let _ = sender.send(Progress::Failed(e.into()));
                return;
            }
        };
        // TODO implement new select_u2_gpio
        // flipper.select_u2_gpio();

        let bus = Uart0::new(&flipper);
        bus.configure(UartBaud::DFU, false);
        let gpio = Gpio::new(&flipper);
        let mut samba = SamBa::new(bus, gpio, &mut sender);

        samba.upload(IFLASH0_ADDR, &firmware);
        if verify { samba.verify(&firmware); }
        samba.reset();
        let _ = samba.sender.send(Progress::Complete);
    });

    receiver
}
