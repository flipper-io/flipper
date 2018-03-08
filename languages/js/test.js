// This is a test file representing code that a user would write
// to control Flipper. This example demonstrates using the LED.

const { Flipper, Led }  = require('./index');

const flipper = new Flipper();
const led = new Led(flipper);

led.rgb(0, 0, 10);
