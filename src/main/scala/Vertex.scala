class Vertex(a: Double,b:Double) {
  val x: Double = a
  val y: Double = b

  override def toString(): String = {
    s"($x,$y)"
  }

  def checkOrientation(two: Vertex,thr: Vertex): Orientation = {
    Math.signum(
        this.x * two.x
      + this.y * thr.x
      + two.x  * thr.y
      - two.y  * thr.x
      - this.x * thr.y
      - this.y * two.x
    ) match {
      case 1 => Left
      case 0 => NoTurn
      case _ => Right
    }
  }
}
