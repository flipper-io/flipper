package io.flipper.fmr;

import jnr.ffi.Runtime;
import jnr.ffi.Struct;

/**
 * @author Nick Mosher <nicholastmosher@gmail.com>
 */
public class _lf_module extends Struct {

    public Struct.UTF8StringRef name = new Struct.UTF8StringRef(64);
    public Struct.UTF8StringRef description = new Struct.UTF8StringRef(64);
    public Struct.u_int16_t version = new Struct.u_int16_t();
    public Struct.u_int16_t identifier = new Struct.u_int16_t();
    public Struct.u_int16_t index = new Struct.u_int16_t();
    public Struct.Pointer device = new Struct.Pointer();

    public _lf_module(Runtime runtime) {
        super(runtime);
    }

    public void setName(java.lang.String name) {
        System.out.println("Assigning module name: " + name);
        this.name.set(name);
    }
}
