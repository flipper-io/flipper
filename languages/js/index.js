// This is the entry point for the Flipper Javascript bindings.
//
// The "Flipper" type and all of the standard packages are exported by
// this module for ease of access. All Flipper programs begin by
// creating a Flipper object, followed by the package binding being
// used. An example demonstrating the Led package is shown below:
//
// ```js
// const { Flipper, Led } = require('flipper');
//
// const flipper = new Flipper();
// const led = new Led(flipper);
//
// led.rgb(0, 0, 10); // Sets the LED to blue.
// ```

const Flipper = require('./flipper');
const Led = require('./led');

module.exports = {
    Flipper,
    Led,
};
