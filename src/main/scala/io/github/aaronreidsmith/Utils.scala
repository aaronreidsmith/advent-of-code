package io.github.aaronreidsmith

import scala.io.Source
import scala.util.Using

type Grid[T] = Map[Point, T]

def usingFile[T](resourceName: String)(body: Source => T): T = Using.resource(Source.fromResource(resourceName))(body) 
