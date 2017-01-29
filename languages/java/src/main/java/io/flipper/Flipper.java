package io.flipper;

import io.flipper.fmr._fmr_list;
import io.flipper.fmr._fmr_module;
import jnr.ffi.LibraryLoader;
import jnr.ffi.Pointer;
import jnr.ffi.Runtime;

import java.lang.reflect.Proxy;

/**
 * @author Nick Mosher <nicholastmosher@gmail.com>
 */
public class Flipper {

    public interface _libflipper {

        // Flipper attach/select bindings.
        Pointer flipper_attach();

        // FMR bindings
        int lf_invoke(_fmr_module module, byte function, _fmr_list parameters);
        int lf_bind(String name);

        long fmr_build(int fmr_argc);
        void fmr_append(Pointer fmr_list, Pointer fmr_arg);
    }

    public static final _libflipper libflipper = LibraryLoader.create(_libflipper.class).load("flipper");

    static Runtime getRuntime() {
        return Runtime.getRuntime(libflipper);
    }

    public interface FMRInvoker {
        int invoke(byte function, _fmr_list params);
    }

    public Flipper() {
        libflipper.flipper_attach();
    }

    public <T> T bindModule(Class<T> moduleInterface, String name, String description, int version, int identifier) {

//        int moduleIndex = libflipper.lf_bind(name);

        int moduleIndex = 5;

        final _fmr_module module = new _fmr_module( Runtime.getRuntime(libflipper)
                                            , name
                                            , description
                                            , version
                                            , identifier
                                            , moduleIndex);

        /*
         * Whenever a user executes a function from their module interface, this ModuleInvocationHandler gets triggered.
         * Upon receiving an invoke call, the ModuleInvocationHandler constructs a _fmr_list parameter list from the
         * function call and uses this FMRInvoker to deliver it back to us, where we can pass it to lf_invoke.
         */
        ModuleInvocationHandler invoker = new ModuleInvocationHandler((func, params) -> {
            return libflipper.lf_invoke(module, func, params);
        });

        return moduleInterface.cast(Proxy.newProxyInstance(moduleInterface.getClassLoader(), new Class[] { moduleInterface }, invoker));
    }
}
