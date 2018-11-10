var ffi = require('ffi');
var ref = require('ref');
var Struct = require('ref-struct');

// typedefs
const lf_version_t = ref.types.uint16;
const lf_crc_t = ref.types.uint16;

const lf_arg = ref.types.uint32;
const lf_function = ref.types.uint8;
const lf_argc = ref.types.uint8;
const lf_return_t = ref.types.uint32;

const _lf_type = [
  ref.types.uint8,
  ref.types.uint16,
  ref.types.uint32
];

const lf_type = ref.types.uint8;

const _lf_module = Struct({
  'name': 'string',
  'description': 'string',
  'version' lf_version_t,
  'identifier': lf_crc_t,
  'index': ref.types.uint16,
  'device': 'pointer'
});

const libflipper = ffi.Library('libflipper', {

  // _lf_device *flipper_attach()
  'flipper_attach': [ 'pointer', [ ] ],

  'flipper_select': [ 'int', [ 'pointer' ] ],

  // lf_return_t lf_invoke(lf_get_selected(), struct _lf_module *module, lf_function function, struct _fmr_parameters *parameters)
  'lf_invoke': [ lf_return_t, [ ref.refType(_lf_module), lf_function, 'pointer' ] ],

  // int lf_bind(struct _lf_module *module, String name)
  'lf_bind': [ 'int', [ ref.refType(_lf_module), 'string' ] ],

  'fmr_build': [ 'pointer', [ lf_argc ] ],

  'lf_append': [ ref.types.void, [ 'pointer', lf_type, lf_arg ] ],

  // led_configure()
  'led_configure': [ ref.types.void, [ ] ],

  // led_rgb(uint8_t red, uint8_t green, uint8_t blue)
  'led_rgb': [ ref.types.void, [ref.types.uint8, ref.types.uint8, ref.types.uint8 ] ]
});

const _create_fmr_parameters = function (paramTypes, args) {
  var list = libflipper.fmr_build(0);

  var i, arg, argType;
  for (i = 0; i < args.length; i++) {
    arg = args[i];
    argType = paramTypes[i];
    libflipper.lf_append(list, _lf_type.indexOf(argType), arg);
  }

  return list;
};

function Flipper ( ) {
  this.device = libflipper.flipper_attach();
}

// Using a node-ffi style interface declaration, bind the given interface to Flipper.
Flipper.prototype.bindModule = function(iface, name) {

  // Hold a reference to this flipper for inner functions to use.
  var self = this;

  var module = new _lf_module();

  // Populates the above module object with metadata.
  var success = libflipper.lf_bind(module.ref(), name);

  var bindings = {};

  // For each function in the given interface, generate a proxy for it and attach it to the bindings.
  Object.keys(iface || {}).forEach(function (func, i) {

    // Get interface metadata for the current function definition.
    var info = iface[func];
    var resultType = info[0];
    var paramTypes = info[1];

    // Generate a proxy function and assign it to the bindings.
    bindings[func] = function() {
      libflipper.flipper_select(self.device);

      if (arguments.length !== paramTypes.length) {
        console.log("Expected " + paramTypes.length + " args, got " + arguments.length);
      }

      var params = _create_fmr_parameters(paramTypes, arguments);

      return libflipper.lf_invoke(lf_get_selected(), module.ref(), i, params);
    }
  });

  return bindings;
};

module.exports = Flipper;
