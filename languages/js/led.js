const ref = require('ref');

const led_interface = {
    // int configure();
    'configure': [ ref.types.uint32, [ ] ],

    // void rgb(uint8_t red, uint8_t green, uint8_t blue);
    'rgb': [ ref.types.void, [ ref.types.uint8, ref.types.uint8, ref.types.uint8 ] ],
};

function Led(flipper) {
    this.device = flipper;
    this.module = this.device.bind(led_interface, "led");
}

Led.prototype.configure = function() {
    return this.module.configure();
};

Led.prototype.rgb = function (red, green, blue) {
    return this.module.rgb(red, green, blue);
};

module.exports = Led;
