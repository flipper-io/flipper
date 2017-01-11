#[link(name = "flipper")]
extern {
    fn led_configure() -> i32;
    fn led_rgb(r: i8, g: i8, b: i8);
}

pub fn configure() -> i32 {
    unsafe {
        return led_configure();
    };
}

pub fn rgb(r: i8, g: i8, b: i8) {
    unsafe {
        led_rgb(r, g, b);
    };
}
