import interface.{FacingDirection, UP}

package object model {
  case class Rover(location: Position, facingDirection: FacingDirection = UP, obstaclesDetected: Set[Position] = Set())

  case class Position(x: Int, y: Int, isObstacle: Boolean = false)

  /**
   * A plateau of determined height and width, represented as a grid of 'x' x 'y' squares.
   *
   * @param sizeX: the width of the plateau
   * @param sizeY: the height of the plateau
   * @param obstacles: a list of xy coordinates each of which will represented an obstacle
   */
  class Plateau(sizeX: Int, sizeY: Int, obstacles: List[(Int, Int)] = Nil) {

    val xBorder: Int = sizeX - 1
    val yBorder: Int = sizeY - 1

    private val squares = (0 until sizeX).toList.flatMap(
      x => (0 until sizeY).collect {
        case y if !obstacles.contains((x, y)) => ((x, y), Position(x, y))
        case y => ((x, y), Position(x, y, isObstacle = true))
      }
    ).toMap

    lazy val matrixRepr: Seq[Seq[Position]] =
      squares.values
        .groupBy(_.y).toSeq.sortBy(_._1)
        .map(_._2.toSeq.sortBy(_.x))

    def getPositionOptAt(coordinates: (Int, Int)): Option[Position] =
      getPositionOptAt(coordinates._1, coordinates._2)

    def getPositionOptAt(x: Int, y: Int): Option[Position] =
      squares.get((x, y))

    def getPositionAt(coordinates: (Int, Int)): Position =
      getPositionAt(coordinates._1, coordinates._2)

    def getPositionAt(x: Int, y: Int): Position =
      getPositionOptAt(x, y).getOrElse(throw new IllegalArgumentException(s"Coordinates $x-$y are not applicable."))
  }

}
