flipper_module!(Led, led,
    0, fn rgb(red: u8, green: u8, blue: u8) -> (),
    1, fn configure() -> (),
);

flipper_module! (Gpio, gpio,
    0, fn configure() -> (),
    1, fn enable(enable: u32, disable: u32) -> (),
    2, fn write(set: u32, clear: u32) -> (),
    3, fn read(mask: u32) -> u32,
);
