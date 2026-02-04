package io

import data.Point.Embedding

trait DataWriter {

  def write(data: Array[Embedding], labels: Array[Int]): Unit

}
