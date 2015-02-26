package com.xhachi.gae4s.datastore.meta;

import java.lang.annotation.*;

@Inherited
@Retention(RetentionPolicy.CLASS)
@Target({ElementType.TYPE})
public @interface entity {

    String kind() default "";

}
