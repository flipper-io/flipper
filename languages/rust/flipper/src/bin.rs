extern crate flipper;
use flipper::led;
use flipper::gpio;

fn main() {
    /* Attach to a device. */
    let device = flipper::attach();
    /* Print the device info. */
    println!("{}", device);
    /* Configure the LED peripheral. */
    led::configure();
    /* Set the LED color. */
    led::rgb(0, 10, 0);
    /* Do some gpio stuff. */
    gpio::enable((1 << 0), 0);
    gpio::write((1 << 0), 0);
}
