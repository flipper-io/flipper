use std::io::{Read, Write, Result};
use ::{
    lf,
    Flipper,
};

pub enum UartBaud {
    FMR,
    DFU,
}

impl UartBaud {
    fn to_baud(&self) -> u8 {
        match *self {
            UartBaud::FMR => 0x00,
            UartBaud::DFU => 0x08,
        }
    }
}

pub struct Uart0<'a> {
    flipper: &'a Flipper,
}

impl<'a> Uart0<'a> {
    pub fn new(flipper: &'a Flipper) -> Self {
        Uart0 { flipper }
    }

    /// Configures the Uart0 module with a given baud rate and
    /// interrupts enabled flag.
    pub fn configure(&self, baud: UartBaud, interrupts: bool) {
        let args = lf::Args::new()
            .append(baud.to_baud())
            .append(if interrupts { 1u8 } else { 0u8 });
        lf::invoke(self.flipper, "uart0", 0, args)
    }

    /// Indicates whether the Uart0 bus is ready to read or write.
    pub fn ready(&self) -> bool {
        let args = lf::Args::new();
        let ret: u8 = lf::invoke(self.flipper, "uart0", 1, args);
        ret != 0
    }
}

impl<'a> Write for Uart0<'a> {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        let args = lf::Args::new();
        lf::push::<()>(self.flipper, "uart0", 2, buf, args);
        Ok(buf.len())
    }
    fn flush(&mut self) -> Result<()> {
        Ok(())
    }
}

impl<'a> Read for Uart0<'a> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        if buf.len() == 0 { return Ok(0) }
        let args = lf::Args::new();
        lf::pull::<()>(self.flipper, "uart0", 3, buf, args);
        Ok(buf.len())
    }
}
