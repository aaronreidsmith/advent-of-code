package io.github.aaronreidsmith.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Simple annotation to mark solutions as slow and print a message if so
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Slow {
    boolean parsing() default false;
    boolean part1() default false;
    boolean part2() default false;
}
