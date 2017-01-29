/**
 * @author Nick Mosher <nicholastmosher@gmail.com>
 */
ref = require('ref');
Flipper = require('./index.js');

console.log("Before new Flipper");

var flipper = new Flipper();

console.log("After new Flipper");

const gpioModuleDef = {
    'gpio_configure': [ 'int', [ ] ],
    'gpio_enable': [ ref.types.void, [ ref.types.uint32, ref.types.uint32 ] ],
    'gpio_write': [ ref.types.void, [ ref.types.uint32, ref.types.uint32 ] ]
};

var gpioModule = flipper.bindModule(gpioModuleDef, "gpio");

console.log("After bind");

gpioModule.gpio_configure();
// gpioModule.gpio_enable(1, 0);
// gpioModule.gpio_write(0, 1);
