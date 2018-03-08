const ffi = require('ffi');
const ref = require('ref');
const Struct = require('ref-struct');

// typedefs
const lf_version_t = ref.types.uint16;
const lf_crc_t = ref.types.uint16;

const lf_value = ref.types.uint32;
const lf_function = ref.types.uint8;
const lf_return_t = ref.types.uint32;

const _lf_type = [
    ref.types.uint8,
    ref.types.uint16,
    ref.types.void,
    ref.types.uint32,
];

const lf_type = ref.types.uint8;

const _lf_module = Struct({
    'name': 'string',
    'description': 'string',
    'version': lf_version_t,
    'identifier': lf_crc_t,
    'index': ref.types.uint32,
    'data': 'pointer',
    'size': ref.refType(ref.types.uint32),
});

const _lf_arg = Struct({
    'arg_type': lf_type,
    'arg_value': lf_value,
});

const libflipper = ffi.Library('libflipper', {

    // _lf_device *flipper_attach()
    'flipper_attach': [ 'pointer', [ ] ],

    // lf_return_t lf_invoke(void *device, void *module, lf_function function, lf_type type, void *parameters)
    'lf_invoke': [ lf_return_t, [ 'pointer', 'pointer', lf_function, lf_type, 'pointer' ] ],

    // int lf_bind(void *device, struct _lf_module *module)
    'lf_bind': [ 'int', [ 'pointer', ref.refType(_lf_module) ] ],

    // int lf_ll_append(struct _lf_ll *list, struct _lf_arg *arg, void *destructor)
    'lf_ll_append': [ ref.types.void, [ 'pointer', ref.refType(_lf_arg), 'pointer' ] ],

    // int lf_ll_release(struct _lf_ll **list)
    'lf_ll_release': [ ref.types.void, [ 'pointer' ] ],
});

const create_arglist = function(types, args) {
    let list_handle = ref.NULL_POINTER;
    for(let i = 0; i < args.length; i++) {
        const arg = new _lf_arg({
            'arg_type': _lf_type.indexOf(types[i]),
            'arg_value': args[i],
        });
        libflipper.lf_ll_append(list_handle, arg.ref(), ref.NULL_POINTER);
    }
    return list_handle;
};

function Flipper ( ) {
    this.device = libflipper.flipper_attach();
}

/**
 * Given an interface declaration and a module name, return a module object with
 * function implementations for controlling Flipper.
 *
 * The interface declaration must be given node-ffi style as follows:
 *
 * ```js
 * const my_module_interface = {
 *   'my_module_function_name': [ RETURN TYPE, [ PARAMETER TYPES ] ],
 * };
 * ```
 *
 * The types must be represented using the "ref" library, which allows you to express
 * types from C using javascript.
 *
 * As an example, the following will bind a module definition for the LED:
 *
 * ```js
 * // Provides representations of C types.
 * const ref = require('ref');
 * const Flipper = require('flipper');
 *
 * const led_interface = {
 *   'rgb': [ ref.types.void, [ ref.types.uint8, ref.types.uint8, ref.types.uint8 ] ],
 * };
 *
 * const my_flipper = new Flipper();
 * const led_module_object = my_flipper.bind(led_interface, "led");
 *
 * // Now you can execute module functions using the module object
 * led_module_object.rgb(0, 10, 0); // Sets the LED to greeen
 * ```
 *
 * @param iface An object describing the interface of the module to bind to.
 * @param name The name of the module loaded on Flipper that we're binding to.
 */
Flipper.prototype.bind = function(iface, name) {

    // Hold a reference to this flipper for inner functions to use.
    const self = this;
    const module = new _lf_module({
        'name': name,
        'description': '',
        'version': 0,
        'identifier': 0,
        'index': 0,
        'data': ref.types.NULL_POINTER,
        'size': ref.types.NULL_POINTER,
    });

    // Populates the above module object with metadata.
    libflipper.lf_bind(self.device, module.ref());

    const bindings = {};

    // For each function in the given interface, generate a proxy for it and attach it to the bindings.
    Object.keys(iface || {}).forEach(function (func, i) {

        // Get interface metadata for the current function definition.
        const info = iface[func];
        const resultType = info[0];
        const paramTypes = info[1];

        // Generate a proxy function and assign it to the bindings.
        bindings[func] = function() {
            if (arguments.length !== paramTypes.length) {
                console.log("Expected " + paramTypes.length + " args, got " + arguments.length);
            }

            const params = create_arglist(paramTypes, arguments);
            const ret = _lf_type.indexOf(resultType);
            const result = libflipper.lf_invoke(self.device, module.ref(), i, ret, params);
            libflipper.lf_ll_release(params.ref());
            return result;
        }
    });

    return bindings;
};

module.exports = Flipper;
