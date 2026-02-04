package io

import data.Point.Embedding

trait DataReader {

  def read: Array[Embedding]

}
