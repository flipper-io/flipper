package io.flipper;

import io.flipper.fmr._lf_module;
import jnr.ffi.LibraryLoader;
import jnr.ffi.Pointer;
import jnr.ffi.Runtime;
import jnr.ffi.Struct;

import java.lang.reflect.Proxy;

/**
 * @author Nick Mosher <nicholastmosher@gmail.com>
 */
public class Flipper {

    private Pointer device;

    public interface _libflipper {

        // Flipper attach/select bindings.
        Pointer flipper_attach();

        // FMR bindings
        int lf_invoke(lf_get_current_device(), _lf_module module, byte function, Pointer parameters);
        int lf_bind(Pointer module);

        Pointer fmr_build(byte argc);
        int fmr_append(Pointer list, byte type, int value);
    }

    public static final _libflipper libflipper = LibraryLoader.create(_libflipper.class).load("flipper");

    static Runtime getRuntime() {
        return Runtime.getRuntime(libflipper);
    }

    public interface FMRInvoker {
        int invoke(byte function, Pointer params);
    }

    public Flipper() {
        device = libflipper.flipper_attach();
    }

    public <T> T bindModule(Class<T> moduleInterface, String name) {

        _lf_module module = new _lf_module(getRuntime());

        module.setName(name);

        int success = libflipper.lf_bind(Struct.getMemory(module));

        /*
         * Whenever a user executes a function from their module interface, this ModuleInvocationHandler gets triggered.
         * Upon receiving an invoke call, the ModuleInvocationHandler constructs a _fmr_list parameter list from the
         * function call and uses this FMRInvoker to deliver it back to us, where we can pass it to lf_invoke.
         */
        ModuleInvocationHandler invoker = new ModuleInvocationHandler(moduleInterface, (func, params) -> libflipper.lf_invoke(lf_get_current_device(), module, func, params));

        return moduleInterface.cast(Proxy.newProxyInstance(moduleInterface.getClassLoader(), new Class[] { moduleInterface }, invoker));
    }
}
