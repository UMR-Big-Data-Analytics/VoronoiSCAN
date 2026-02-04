package cluster

import data.Point
import spatial.index.QuadTree

trait Cell {

  def getId: Int

  def getPoints: Array[Point]

  def getTree: QuadTree

}
