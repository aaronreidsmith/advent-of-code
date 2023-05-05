package io.github.aaronreidsmith;

import org.scalatest.TagAnnotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Simple tag that allows us to run certain tests locally but not on CI. This is useful for tests that are
 * resource-intensive and CI/GitHub Actions may not have the proper allotment. Defined as a Java interface so we can
 * tag the entire suite and not have to tag individual tests
 */
@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
public @interface IgnoreOnCI {}
