package com.xhachi.gae4s.datastore.meta;

import java.lang.annotation.*;

@Inherited
@Retention(RetentionPolicy.CLASS)
@Target({ElementType.FIELD, ElementType.METHOD})
public @interface property {

    String name() default "";
    boolean json() default false;
    boolean serialize() default false;
    boolean indexed() default false;
    boolean version() default false;
    boolean creationDate() default false;
    boolean modificationDate() default false;
    int order() default Integer.MAX_VALUE;

}
