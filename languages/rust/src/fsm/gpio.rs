#[link(name = "flipper")]
extern {
    fn gpio_configure() -> i32;
    fn gpio_enable(enable: i32, disable: i32);
    fn gpio_write(set: i32, clear: i32);
}

pub fn configure() -> i32 {
    unsafe {
        return gpio_configure();
    };
}

pub fn enable(enable: i32, disable: i32) {
    unsafe {
        gpio_enable(enable, disable);
    };
}

pub fn write(set: i32, clear: i32) {
    unsafe {
        gpio_write(set, clear);
    };
}
