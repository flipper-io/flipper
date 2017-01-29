package io.flipper.fmr;

import jnr.ffi.Runtime;
import jnr.ffi.Struct;

/**
 * @author Nick Mosher <nicholastmosher@gmail.com>
 */
public class _fmr_list extends Struct {

    public Struct.u_int8_t argc;
    public Struct.Pointer next;

    public _fmr_list(Runtime runtime) {
        super(runtime);
        System.out.println("runtime = [" + runtime + "]");
        argc = new Struct.u_int8_t();
        next = null;
    }
}
