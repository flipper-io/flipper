package io.flipper;

import io.flipper.fmr._fmr_arg;
import io.flipper.fmr._fmr_list;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;

/**
 * @author Nick Mosher <nicholastmosher@gmail.com>
 */
public class ModuleInvocationHandler implements InvocationHandler {

    private final Flipper.FMRInvoker invoker;

    ModuleInvocationHandler(Flipper.FMRInvoker invoker) {
        this.invoker = invoker;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {

        if (!method.isAnnotationPresent(ModuleFunction.class)) {
            throw new FlipperModuleException("Methods in flipper bindings interfaces must be annotated with @ModuleFunction with functionId defined.");
        }

        Parameter[] parameters = method.getParameters();

        System.out.println("HELLO WE MADE IT TO " + method.getName());

        _fmr_arg arg;
        _fmr_list list = new _fmr_list(Flipper.getRuntime());

        System.out.println("Survived the Runtime");
//        for (int i = 0; i < parameters.length; i++) {
//            Parameter p = parameters[i];
//            Object a = args[i];
//            arg = new _fmr_arg(Flipper.getRuntime(), p, a);
//            list.add(arg);
//        }

        return invoker.invoke(method.getAnnotation(ModuleFunction.class).functionId(), list);
    }
}
