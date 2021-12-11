package io.github

import scala.io.Source
import scala.util.Using

package object aaronreidsmith {
  def using[T](resourceName: String)(body: Source => T): T = Using.resource(Source.fromResource(resourceName))(body)
}
