package io.flipper.java.util;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;

/**
 * Created by Nick Mosher on 6/5/16.
 *
 * The Flipper driver wrappers for Java allow for data translation
 * between C and Java. A Structure can be used to make a Java class
 * represent equivalent data as a corresponding C struct so that
 * function calls requiring parameters or return types of a particular
 * struct or of void pointers may be "cast" into a Java class and
 * vice versa.
 */
public class Structure extends com.sun.jna.Structure {

    /**
     * JNA Structures require each field to be listed in order and
     * returned to this function. Here we're performing some Java
     * reflection to determine which fields belong strictly to
     * subclasses of this class, returning their names as the value
     * for the method.
     * @return an ordered list of all fields declared by subclasses
     * of Structure.
     */
    protected List getFieldOrder() {
        //Get all fields from this object instance.
        Field[] fields = this.getClass().getFields();
        int validFields = fields.length;

        //Remove fields declared in superclasses of Structure.
        for(int i = 0; i < fields.length; i++) {
            if(!Structure.class.isAssignableFrom(fields[i].getDeclaringClass())) {
                fields[i] = null;
                validFields--;
            }
        }

        //Create an array of names of the remaining fields.
        String[] names = new String[validFields];
        validFields = 0;
        for(int i = 0; i < fields.length; i++) {
            if(fields[i] != null) {
                names[validFields++] = fields[i].getName();
            }
        }

        return Arrays.asList(names);
    }

    /**
     * Helper method provided by JNA's Structure class allows for measurement
     * of the byte-size of this Structure.
     * @return The size (in bytes) of this Structure.
     */
    public int calculateSize() {
        return super.calculateSize(false);
    }
}