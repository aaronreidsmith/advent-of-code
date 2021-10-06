package io.github.aaronreidsmith.util

import scala.language.reflectiveCalls

trait FileUtils {
  def using[A <: { def close(): Unit }, B](resource: A)(fn: A => B): B = try {
    fn(resource)
  } finally {
    resource.close()
  }
}
