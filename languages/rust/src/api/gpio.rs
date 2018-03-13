use lf;
use Flipper;

pub struct Gpio<'a> {
    flipper: &'a Flipper,
}

impl<'a> Gpio<'a> {
    pub fn new(flipper: &'a Flipper) -> Gpio<'a> {
        Gpio { flipper }
    }

    pub fn configure(&self) {
        let args = lf::Args::new();
        lf::invoke(self.flipper, "gpio", 0, args)
    }

    pub fn enable(&self, enable: u32, disable: u32) {
        let args = lf::Args::new()
            .append(enable)
            .append(disable);
        lf::invoke(&self.flipper, "gpio", 1, args)
    }

    pub fn write(&self, set: u32, clear: u32) {
        let args = lf::Args::new()
            .append(set)
            .append(clear);
        lf::invoke(&self.flipper, "gpio", 2, args)
    }

    pub fn read(&self, mask: u32) -> u32 {
        let args = lf::Args::new()
            .append(mask);
        lf::invoke(&self.flipper, "gpio", 3, args)
    }
}
