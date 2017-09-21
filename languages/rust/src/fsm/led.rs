#[link(name = "flipper")]
extern {
    fn led_configure() -> u32;
    fn led_rgb(r: u8, g: u8, b: u8);
}

pub fn configure() -> u32 {
    unsafe {
        led_configure()
    }
}

pub fn rgb(r: u8, g: u8, b: u8) {
    unsafe {
        led_rgb(r, g, b)
    }
}
