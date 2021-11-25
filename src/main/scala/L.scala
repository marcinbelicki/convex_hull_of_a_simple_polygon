class L(one: Vertex, two: Vertex) {

  val first: Vertex = one
  val second: Vertex = two

  def checkOrientation(point: Vertex): Orientation = {
    first.checkOrientation(second,point)
  }

}
