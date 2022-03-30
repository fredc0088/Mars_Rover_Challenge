package interface

import model.{Grid, Position, Rover}

case class PlateauInterface(grid: Grid, rover: Rover) {

  private def forward: Position =
    if(rover.facingPosition == UP || rover.facingPosition == DOWN) {
      val (x,y) = rover.facingPosition.calculateNextCoordinate(grid.yBorder)(rover.location)
      Position(x, y)
    } else {
      val (x,y) = rover.facingPosition.calculateNextCoordinate(grid.xBorder)(rover.location)
      Position(x, y)
    }

  def issueCommand(command: RoverCommand): PlateauInterface =
    command match {
      case RotateLeft =>
        this.copy(rover = rover.copy(facingPosition = rover.facingPosition.rotateAntiClockWise))
      case RotateRight =>
        this.copy(rover = rover.copy(facingPosition = rover.facingPosition.rotateClockWise))
      case GoForward =>
        forward match {
          case nextPosition if nextPosition.isObstacle =>
            println(nextPosition + " is obstacle")
            this.copy(rover = rover.copy(obstaclesDetected = rover.obstaclesDetected + nextPosition))
          case nextPosition =>
            println((nextPosition.x, nextPosition.y))
            this.copy(rover = rover.copy(location = nextPosition))
        }
      case GetToLocation(x, y) =>
        if(grid.boundaries.get((x, y)).filterNot(_.isObstacle).isDefined)
          issueCommandsSequence(findShortest((x,y)))
        else throw new Exception
    }

  def issueCommandsSequence(commands: Seq[RoverCommand]) =
    commands.foldLeft(this)((state, command) => state.issueCommand(command))

  def findShortestPath(currentPosition: Position, target: Position) = {
    val (targetX, targetY) = (target.x, target.y)
    target.x
  }

  def r(tested: (Int, Int), target: (Int, Int)) = {
    tested
    Math.abs(target._1 - target._1)
  }

  def findShortest(
                    target: (Int, Int)
                  ) = {

    def z(rover: Rover, nextFacing: Facing): Option[(Rover, Seq[RoverCommand])] =
      grid.boundaries.get(nextFacing.calculateNextCoordinate{
        if(nextFacing == DOWN || nextFacing == UP) grid.yBorder
        else grid.xBorder
      }(rover.location))
        .filterNot(_.isObstacle)
        .map(potNextLoc =>
          (rover.copy(location = potNextLoc, facingPosition = nextFacing),
            rover.facingPosition.rotateToInstruction(nextFacing) :+ GoForward)
        )

    def f(newInterfaceState: PlateauInterface, commands: Seq[RoverCommand]): Seq[RoverCommand] = {
      val rover = newInterfaceState.rover
      val currentLocation = rover.location
      val (currentX, currentY) = (currentLocation.x, currentLocation.y)
      if(target._1 > rover.location.x)
        z(rover, RIGHT) match {
          case Some((newRover, newCommands)) =>
            f(newInterfaceState.copy(rover = newRover), commands ++ newCommands)
          case None => f(newInterfaceState, commands)
        }
      else if(target._1 < rover.location.x)
        z(rover, LEFT) match {
          case Some((newRover, newCommands)) =>
            f(newInterfaceState.copy(rover = newRover), commands ++ newCommands)
          case None => f(newInterfaceState, commands)
        }
      else if(target._2 > rover.location.y)
        z(rover, DOWN) match {
          case Some((newRover, newCommands)) =>
            f(newInterfaceState.copy(rover = newRover), commands ++ newCommands)
          case None => f(newInterfaceState, commands)
        } else if(target._2 < rover.location.y)
        z(rover, UP) match {
          case Some((newRover, newCommands)) =>
            f(newInterfaceState.copy(rover = newRover), commands ++ newCommands)
          case None => f(newInterfaceState, commands)
        } else commands
    }

    f(this, Nil)
    //      def findShortestPath(queue: Queue[Position], distances: Map[Position, Int], explored: Set[Position]) = {
    //
    //      }
  }
}
