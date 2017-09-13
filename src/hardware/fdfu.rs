use byteorder::{BigEndian, ReadBytesExt};
use std::io::Cursor;
use flipper_rust::fsm::{uart0, gpio};
use std::thread;

pub fn sam_ba_jump(address: u32) {
    uart0::write(format!("G{:08x}#", address).as_bytes());
}

/// Write a byte into the given memory address on the device.
pub fn sam_ba_write_byte(address: u32, byte: u8) {
    uart0::write(format!("O{:08x},{:02x}#", address, byte).as_bytes());
}

/// Write a word into the given memory address on the device.
pub fn sam_ba_write_word(address: u32, word: u32) {
    uart0::write(format!("W{:08x},{:08x}#", address, word).as_bytes());
}

/// Read a byte from the given memory address on the device.
pub fn sam_ba_read_byte(address: u32) -> u8 {
    uart0::write(format!("o{:08x},#", address).as_bytes());
    for _ in 0..8 { if uart0::ready() { break; } }
    let mut buffer = [0u8; 1];
    uart0::read(&mut buffer[..], 0xff);
    buffer[0]
}

/// Read a word from the given memory address on the device.
pub fn sam_ba_read_word(address: u32) -> u32 {
    uart0::write(format!("w{:08x},#", address).as_bytes());
    for _ in 0..8 { if uart0::ready() { break; }}
    let mut buffer = [0u8; 4];
    uart0::read(&mut buffer[..], 0xff);
    Cursor::new(buffer).read_u32::<BigEndian>().unwrap()
}

/// Write the given command and argument into the EEFC_FCR register.
pub fn sam_ba_write_efc_fcr(command: u8, arg: u32) {

    let efc_base_address: u32 = 0x400E0A00;                                          // SAM4S16.h:7479
    let eefc_fcr_offset = 0x04;                                                      // SAM4S16.h:1125

    let eefc_fcr_fkey_pos = 24;                                                      // SAM4S16.h:1144
    let eefc_fcr_fkey_msk = 0xff << eefc_fcr_fkey_pos;                               // SAM4S16.h:1145
    let eefc_fcr_fkey = eefc_fcr_fkey_msk & ((0x5A) << eefc_fcr_fkey_pos);           // SAM4S16.h:1146
    let eefc_fcr_farg = eefc_fcr_fkey_msk & ((arg) << eefc_fcr_fkey_pos);            // SAM4S16.h:1143
    let eefc_fcr_fcmd = eefc_fcr_fkey_msk & ((command as u32) << eefc_fcr_fkey_pos); // SAM4S16.h:1140

    let eefc_word: u32 = eefc_fcr_fkey | eefc_fcr_farg | eefc_fcr_fcmd;
    let eefc_address = efc_base_address + eefc_fcr_offset;

    sam_ba_write_word(efc_base_address, eefc_word);
}

#[repr(C)]
#[repr(packed)]
struct XPacket {
    header: u8,
    number: u8,
    _number: u8,
    data: [u8; 128],
    checksum: u16,
}

use std::fmt;
impl fmt::Display for XPacket {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Header: {:02X}, Number: {:02X}, Checksum: {:02X}",
                 self.header,
                 self.number,
                 self.checksum);

        let mut output = writeln!(f, "Data:");
        for chunk in (&self.data).chunks(8) {
            output = write!(f, "{:?} ", chunk);
        }
        output
    }
}

/// Moves data from the host to the device's RAM using the SAM-BA
/// and XMODEM protocol.
pub fn sam_ba_copy(address: u32, data: &[u8]) {
    use std::io::Write;

    let mut buffer: Vec<u8> = Vec::with_capacity(20);
    write!(&mut buffer, "S{:08X},{:08X}#", address, data.len());
    uart0::write(&buffer);

    for _ in 0..8 { if uart0::ready() { break; } }

    println!("ABOUT TO READ BYTE");

    let mut buffer = [0u8; 1];
    uart0::read(&mut buffer[..], 0xff);
    let byte = buffer[0];

    println!("READ BYTE");

    if byte == b'C' { return; }

    let SOH = 0x01;
    for chunk in data.chunks(128).enumerate() {
        match chunk {
            (i, chunk) => {
                let mut data = [0u8; 128];
                data.clone_from_slice(chunk);
                let packet = XPacket {
                    header: SOH,
                    number: (i + 1) as u8,
                    _number: !(i + 1) as u8,
                    data,
                    checksum: 0x00,
                };
                println!("Packet: {}", packet);
            },
            _ => break,
        }
    }
}

const SAM_ERASE_PIN: u32 = 0x06;
const SAM_RESET_PIN: u32 = 0x05;

pub fn sam_reset() {
    gpio::write(0, (1 << SAM_RESET_PIN));
    thread::sleep_ms(10);
    gpio::write((1 << SAM_RESET_PIN), 0);
}

pub fn sam_enter_dfu() -> bool {
    gpio::write((1 << SAM_ERASE_PIN), (1 << SAM_RESET_PIN));
    thread::sleep_ms(5000);
    gpio::write((1 << SAM_RESET_PIN), (1 << SAM_ERASE_PIN));
    sam_reset();
    return true;
}

pub fn load_page_data() {

}

pub fn enter_update_mode() -> bool {
    println!("Setting uart0 to DFU baud");
    uart0::dfu();
    println!("Uart0 set to DFU baud");

    uart0::write(&[b'#']);
    let mut buffer = [0u8; 3];
    uart0::read(&mut buffer, 0xff);

    if &buffer[..] == &[b'\n', b'\r', b'>'] {
        println!("Already in update mode");
        true
    } else {
        if sam_enter_dfu() {
            println!("Successfully entered update mode");
            true
        } else {
            println!("Failed to enter update mode");
            false
        }
    }
}

pub fn enter_normal_mode() {

}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
