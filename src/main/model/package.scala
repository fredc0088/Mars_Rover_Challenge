import interface.{Facing, UP}

package object model {
  case class Rover(location: Position, facingPosition: Facing = UP, obstaclesDetected: Set[Position] = Set())

  case class Position(x: Int, y: Int, isObstacle: Boolean = false)

  class Grid(sizeX: Int, sizeY: Int, obstacles: List[(Int, Int)] = Nil)

  }

}
