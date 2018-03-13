use ::{
    lf,
    Flipper,
};

pub struct Led<'a> {
    flipper: &'a Flipper,
}

impl<'a> Led<'a> {
    pub fn new(flipper: &'a Flipper) -> Led<'a> {
        Led { flipper }
    }

    pub fn configure(&self) {
        let args = lf::Args::new();
        lf::invoke(self.flipper, "led", 1, args)
    }

    pub fn rgb(&self, r: u8, g: u8, b: u8) {
        let args = lf::Args::new()
            .append(r)
            .append(g)
            .append(b);
        lf::invoke(self.flipper, "led", 0, args)
    }
}
