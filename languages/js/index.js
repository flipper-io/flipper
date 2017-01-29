var ffi = require('ffi');
var ref = require('ref');
var Struct = require('ref-struct');

// typedefs
const lf_version_t = ref.types.uint16;
const lf_crc_t = ref.types.uint16;

const fmr_va = ref.types.uint64;
const fmr_arg = ref.types.uint32;
const fmr_module = ref.types.uint32;
const fmr_function = ref.types.uint8;
const fmr_argc = ref.types.uint8;
const fmr_types = ref.types.uint16;
const fmr_return = ref.types.uint32;

const _fmr_type = [
	ref.types.uint8,
	ref.types.uint16,
	ref.types.uint32
];

const fmr_type = ref.types.uint8;

// const _fmr_arg = Struct({
// 	'value': fmr_arg,
// 	'type':  fmr_type,
// 	'next':  'pointer' // Points to another _fmr_arg
// });
//
// const _fmr_parameters = Struct({
// 	'argc': fmr_argc,
// 	'argv': ref.refType(_fmr_arg)
// });

const _lf_module = Struct({
	'name':        'string',
	'description': 'string',
	'version':     lf_version_t,
	'identifier':  lf_crc_t,
    'index':       ref.types.uint16,
	'device':      'pointer'
});

const libflipper = ffi.Library('libflipper', {

	// _lf_device *flipper_attach()
	'flipper_attach': [ 'pointer', [ ] ],

	'flipper_select': [ 'int', [ 'pointer' ] ],

	// fmr_return lf_invoke(struct _lf_module *module, fmr_function function, struct _fmr_parameters *parameters)
	'lf_invoke':      [ fmr_return, [ ref.refType(_lf_module), fmr_function, 'pointer' ] ],

	// int lf_bind(struct _lf_module *module, String name)
	'lf_bind':        [ 'int', [ ref.refType(_lf_module), 'string' ] ],

    'fmr_build':      [ 'pointer', [ fmr_argc ] ],

    'fmr_append':     [ ref.types.void, [ fmr_type, fmr_arg ] ],

	// led_configure()
	'led_configure':  [ ref.types.void, [ ] ],

	// led_rgb(uint8_t red, uint8_t green, uint8_t blue)
	'led_rgb':        [ ref.types.void, [ref.types.uint8, ref.types.uint8, ref.types.uint8 ] ]
});

const _create_fmr_parameters = function (paramTypes, args) {
	var list = libflipper.fmr_build(0);

	var i, arg, argType;
	for (i = 0; i < args.length; i++) {
        arg = args[i];
        argType = paramTypes[i];

        if (!_fmr_type.includes(argType)) {
            console.log("Invalid parameter type for argument " + i);
            return null;
        }

        libflipper.fmr_append(list, _fmr_type.indexOf(argType), arg);
    }

	return list;
};

function Flipper ( ) {
    this.device = libflipper.flipper_attach( );
}

// Using a node-ffi style interface declaration, bind the given interface to Flipper.
Flipper.prototype.bindModule = function(iface, name) {

    // Hold a reference to this flipper for inner functions to use.
    var self = this;

    var module = new _lf_module();

    console.log("After module insantiation");

    console.log(self.device);
    console.log("Module device: ");
    console.log(module.device);

	// Populates the above module object with metadata.
    var success = libflipper.lf_bind(module.ref(), name);

    console.log("After module bind");

	var bindings = { };

	console.log("Before loop");

	// For each function in the given interface, generate a proxy for it and attach it to the bindings.
	Object.keys(iface || {}).forEach(function (func, i) {

		// Get interface metadata for the current function definition.
		var info = iface[func]
		  , resultType = info[0]
		  , paramTypes = info[1];

		// Generate a proxy function and assign it to the bindings.
		bindings[func] = function() {
		    console.log("Before libflipper select");
			// libflipper.select(self.device);
            console.log("After libflipper select");

			if (arguments.length !== paramTypes.length) {
				console.log("Expected " + paramTypes.length + " args, got " + arguments.length);
    		}

			var params = _create_fmr_parameters(paramTypes, arguments);

			console.log("Before lf_invoke");

			var result = libflipper.lf_invoke(module.ref(), i, ref.NULL);
			// if (typeof result != resultType) {
			// 	console.log("Return type does not equal expected.");
			// }
			return result;
		}
	});

	return bindings;
};

module.exports = Flipper;

// libflipper.flipper_attach();
// libflipper.led_configure();
// libflipper.led_rgb(10, 0, 0);