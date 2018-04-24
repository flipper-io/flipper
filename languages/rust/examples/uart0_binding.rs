extern crate flipper;

use std::io::Write;
use flipper::{Flipper, StandardModule};
use flipper::fsm::uart0::{Uart0, UartBaud};

fn main() {
    let flipper = Flipper::attach_hostname("localhost").expect("should attach to fvm");
    let mut uart = Uart0::bind(&flipper);
    uart.configure(UartBaud::DFU, true);
    let _ = uart.write(b"deadbeef deadbeef");
}