import scala.annotation.tailrec

object LeftHull extends App {

  def calculateLeftHull(simplePolygon: List[Vertex]): List[Vertex] = {
    @tailrec
    def firstBlock(simplePolygon: List[Vertex], q: List[Vertex], y: Option[Vertex]): (List[Vertex],List[Vertex],Option[Vertex]) = {
      simplePolygon match {
        case Nil => (simplePolygon, q, y)
        case head :: tail =>
          q match {
            case first :: second :: _ =>
              second.checkOrientation(first, head) match {
                case Right => (tail, head :: q, tail.headOption)
                case _ => firstBlock(tail, q, y)
              }
          }
      }
    }
    @tailrec
    def untilLeft(simplePolygon: List[Vertex], l: L, x: Vertex): (Vertex, List[Vertex]) = {
      l.checkOrientation(x) match {
        case Left => (x,simplePolygon)
        case _ => simplePolygon match {
          case head::tail => untilLeft(tail,l,head)
          case _ => (x,simplePolygon)
        }
      }
    }
    @tailrec
    def fourthBlock(q: List[Vertex], x: Vertex): (List[Vertex],Vertex) = {
      q match {
        case first::(tail @ second::_ )=>
          second.checkOrientation(first,x) match {
            case Right =>
              (x::q,second)

            case _ => fourthBlock(tail,x)
          }
      }
    }

    @tailrec
    def otherBlocks(simplePolygon: List[Vertex], q: List[Vertex], y: Vertex, q0: Vertex): List[Vertex] = {
      simplePolygon match {
        case Nil => q
        case head::tail =>
          q match {
            case first::second::_ =>
              second.checkOrientation(first,head) match {
                case Right | NoTurn =>
                  val l = y.checkOrientation(first,head) match{
                    case Left => new L(second,first)
                    case _ => new L(first,q0)
                  }
                  untilLeft(tail,l,head) match {
                    case (x, simplePolygon) =>
                      val (newQ,y) = fourthBlock(q,x)
                      otherBlocks(simplePolygon,newQ,y,q0)
                  }
                case  _ =>
                  val (newQ,y) = fourthBlock(q,head)
                  otherBlocks(simplePolygon,newQ,y,q0)

              }
          }
      }
    }

    simplePolygon match {
      case Nil | _::Nil | _::_::Nil => simplePolygon
      case first::second::tail =>
        firstBlock(tail,second::first::Nil,tail.headOption) match {
          case (_,q,None) => q
          case (rest,q,Some(y)) =>otherBlocks(rest,q,y,first)
        }
    }
  }

    println(calculateLeftHull(List(
      new Vertex(0,0),
      new Vertex(0,6),
      new Vertex(2,4),
      new Vertex(1,3),
      new Vertex(2,2)
    )))


}
