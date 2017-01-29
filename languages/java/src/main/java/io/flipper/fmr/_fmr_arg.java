package io.flipper.fmr;

import jnr.ffi.Runtime;
import jnr.ffi.Struct;

import java.lang.reflect.Parameter;
import java.security.InvalidParameterException;
import java.util.Arrays;
import java.util.List;

/**
 * @author Nick Mosher <nicholastmosher@gmail.com>
 */
public class _fmr_arg extends Struct {

    public static final List<java.lang.String> ARG_TYPENAMES = Arrays.asList("byte", "short", "int");

    public Struct.u_int32_t value;
    public Struct.u_int8_t type;
    public Struct.Pointer next;

    /**
     * Constructs an objcet that corresponds to the _fmr_arg struct in libflipper.
     *
     * @param runtime The jnr-ffi Runtime associated with Flipper's bindings.
     * @param p A java.reflect.Parameter object with metadata about this argument's type.
     * @param arg An Object containing the argument value which needs to be cast using the Parameter metadata.
     */
    public _fmr_arg(Runtime runtime, Parameter p, Object arg) {
        super(runtime);

        if (!ARG_TYPENAMES.contains(p.getType().getName())) {
            throw new InvalidParameterException("Flipper module functions cannot have parameters of type: " + p.getType().getName());
        }

        type = new Struct.u_int8_t(new Struct.Offset(4));
        type.set(ARG_TYPENAMES.indexOf(p.getType().getName()));

        // Check that the parameter can be cast to an Integer type (and thus be stored in this.value).
        if (Integer.class.isAssignableFrom(p.getType())) {
            value = new Struct.u_int32_t(new Struct.Offset(5));
            value.set((Integer) p.getType().cast(arg));
        } else {
            throw new InvalidParameterException("Cannot cast parameter '" + p.getName() + "' to an Integer");
        }
    }

    Pointer getNext() {
        return next;
    }

    void setNext(Pointer n) {
        next = n;
    }
}
