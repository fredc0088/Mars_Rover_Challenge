package interface

import com.typesafe.scalalogging.LazyLogging
import model.{Plateau, Position, Rover}

import scala.annotation.tailrec

class PlateauInterface(val grid: Plateau, val rover: Rover) extends LazyLogging {

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
        this.updateRover(rover = rover.copy(facingPosition = rover.facingPosition.rotateAntiClockWise))
      case RotateRight =>
        this.updateRover(rover = rover.copy(facingPosition = rover.facingPosition.rotateClockWise))
      case GoForward =>
        forward match {
          case nextPosition if nextPosition.isObstacle =>
            this.updateRover(rover = rover.copy(obstaclesDetected = rover.obstaclesDetected + nextPosition))
          case nextPosition =>
            val updated = this.updateRover(rover = rover.copy(location = nextPosition))
            logger.info(s"${
              (updated.rover.location.x, updated.rover.location.y)
            }\n${updated.printPlateauState}\n\n\n")
            updated
        }
      case GetToLocation(x, y) =>
        if(grid.squares.get((x, y)).filterNot(_.isObstacle).isDefined)
          issueCommandsSequence(getInstructionsToShortestPath((x,y)))
        else throw new Exception
    }

  def issueCommandsSequence(commands: Seq[RoverCommand]): PlateauInterface =
    commands.foldLeft(this) { (state, command) =>
      state.issueCommand(command)
    }

  def getInstructionsToShortestPath(target: (Int, Int)): Seq[RoverCommand] = {

    def getRoverAndInstructionToNext(rover: Rover, nextFacing: Facing): Option[(Rover, Seq[RoverCommand])] =
      grid.squares.get(nextFacing.calculateNextCoordinate{
        if(nextFacing == DOWN || nextFacing == UP) grid.yBorder
        else grid.xBorder
      }(rover.location))
        .filterNot(_.isObstacle)
        .map(potNextLoc =>
          (rover.copy(location = potNextLoc, facingPosition = nextFacing),
            rover.facingPosition.rotateToInstruction(nextFacing) :+ GoForward)
        )

    @tailrec
    def helper(newInterfaceState: PlateauInterface, commands: Seq[RoverCommand]): Seq[RoverCommand] = {
      val rover = newInterfaceState.rover
      val currentLocation = rover.location
      val (currentX, currentY) = (currentLocation.x, currentLocation.y)
      if(target._1 > rover.location.x)
        getRoverAndInstructionToNext(rover, RIGHT) match {
          case Some((newRover, newCommands)) =>
            helper(newInterfaceState.updateRover(newRover), commands ++ newCommands)
          case None => helper(newInterfaceState, commands)
        }
      else if(target._1 < rover.location.x)
        getRoverAndInstructionToNext(rover, LEFT) match {
          case Some((newRover, newCommands)) =>
            helper(newInterfaceState.updateRover(newRover), commands ++ newCommands)
          case None => helper(newInterfaceState, commands)
        }
      else if(target._2 > rover.location.y)
        getRoverAndInstructionToNext(rover, DOWN) match {
          case Some((newRover, newCommands)) =>
            helper(newInterfaceState.updateRover(newRover), commands ++ newCommands)
          case None => helper(newInterfaceState, commands)
        } else if(target._2 < rover.location.y)
        getRoverAndInstructionToNext(rover, UP) match {
          case Some((newRover, newCommands)) =>
            helper(newInterfaceState.updateRover(newRover), commands ++ newCommands)
          case None => helper(newInterfaceState, commands)
        } else commands
    }

    helper(this, Nil)
    //      def findShortestPath(queue: Queue[Position], distances: Map[Position, Int], explored: Set[Position]) = {
    //
    //      }
  }

  def printPlateauState: String =
    grid.squares.values.groupBy(_.y).map(_._2.toList.sortBy(_.x)).toList
      .map(t => t.map{
        case position if(position.isObstacle) => "!^/"
        case rover.location                   => " @ "
        case _                                => "[_]"
      }.mkString).mkString("\n")

  def updateRover(rover: Rover): PlateauInterface = new PlateauInterface(grid, rover)
}

object PlateauInterface {

  def apply(grid: Plateau, rover: Rover): PlateauInterface =
    new PlateauInterface(grid, rover)

  def initRover(x: Int, y: Int, initialDirection: String)(plateau: Plateau): PlateauInterface =
    plateau.squares.get((x, y)) match {
      case Some(square) if !square.isObstacle =>
        PlateauInterface(plateau, Rover(square, Facing.translate(initialDirection)))
      case Some(_) =>
        throw new IllegalArgumentException(s"Coordinates $x-$y are occupied.")
      case _ =>
        throw new IllegalArgumentException(s"Coordinates $x-$y are not applicable.")
    }
}
