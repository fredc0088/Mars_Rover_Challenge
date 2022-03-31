import interface.{Facing, UP}

package object model {
  case class Rover(location: Position, facingPosition: Facing = UP, obstaclesDetected: Set[Position] = Set())

  case class Position(x: Int, y: Int, isObstacle: Boolean = false)

  class Plateau(sizeX: Int, sizeY: Int, obstacles: List[(Int, Int)] = Nil) {

    val xBorder: Int = sizeX - 1
    val yBorder: Int = sizeY - 1

    val squares = (0 to sizeX).toList.flatMap(
      x => (0 to sizeY).collect{
        case y if !obstacles.contains((x, y)) => ((x,y), Position(x, y))
        case y => ((x,y), Position(x, y, isObstacle = true))
      }
    ).toMap

  }

}
