package io.flipper.fmr;

import jnr.ffi.Runtime;
import jnr.ffi.Struct;

/**
 * @author Nick Mosher <nicholastmosher@gmail.com>
 */
public class _fmr_module extends Struct {

    public java.lang.String name;
    public java.lang.String description;
    public Struct.u_int16_t version;
    public Struct.u_int16_t identifier;
    public Struct.u_int16_t index;

    public _fmr_module( Runtime runtime
                      , java.lang.String name
                      , java.lang.String description
                      , int version
                      , int identifier
                      , int index
                      ) {
        super(runtime);

        System.out.println("runtime = [" + runtime + "], name = [" + name + "], description = [" + description + "], version = [" + version + "], identifier = [" + identifier + "], index = [" + index + "]");

        this.name = name;
        this.description = description;
        this.version = new Struct.u_int16_t();
        this.version.set((Number) version);
        this.identifier = new Struct.u_int16_t();
        this.identifier.set((Number) identifier);
        this.index = new Struct.u_int16_t();
        this.index.set((Number) index);

        System.out.println("runtime = [" + runtime + "], name = [" + name + "], description = [" + description + "], version = [" + version + "], identifier = [" + identifier + "], index = [" + index + "]");
    }
}
