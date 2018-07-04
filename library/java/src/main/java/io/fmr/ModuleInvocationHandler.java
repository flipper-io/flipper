package io.flipper;

import jnr.ffi.Pointer;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Nick Mosher <nicholastmosher@gmail.com>
 */
public class ModuleInvocationHandler <T> implements InvocationHandler {

    private final Class<T> moduleInterface;
    private final Flipper.FMRInvoker invoker;

    private final Map<String, Byte> lf_types = new HashMap<String, Byte>(){{
        put("byte",  (byte) 0);
        put("short", (byte) 1);
        put("int",   (byte) 2);
    }};

    ModuleInvocationHandler(Class<T> iface, Flipper.FMRInvoker invoker) {
        moduleInterface = iface;
        this.invoker = invoker;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {

        Class interfaceClass = Class.forName(moduleInterface.getTypeName());
        List<Method> iFaceMethods = Arrays.asList(interfaceClass.getDeclaredMethods());

        Parameter[] parameters = method.getParameters();
        Pointer list = Flipper.libflipper.fmr_build((byte) 0);
        if (args != null) {
            Parameter param;
            String pName;
            for (int i = 0; i < args.length; i++) {
                param = parameters[i];
                if (!lf_types.containsKey(pName = param.getType().getName())) {
                    throw new IllegalArgumentException("Illegal fmr argument type: " + pName);
                }
                Flipper.libflipper.lf_append(list, lf_types.get(pName), (Integer) args[i]);
            }
        }

        return invoker.invoke((byte) iFaceMethods.indexOf(method), list);
    }
}
