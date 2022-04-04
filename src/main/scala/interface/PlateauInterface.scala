package interface

import com.typesafe.scalalogging.LazyLogging
import model.{Plateau, Position, Rover}

import scala.annotation.tailrec

class PlateauInterface(val grid: Plateau, val rover: Rover) extends LazyLogging {

  private def forward: Position =
    grid.getPositionAt {
      if (rover.facingDirection == UP || rover.facingDirection == DOWN)
        rover.facingDirection.calculateNextCoordinate(grid.yBorder)(rover.location)
      else
        rover.facingDirection.calculateNextCoordinate(grid.xBorder)(rover.location)
    }

  def issueCommand(command: RoverCommand): PlateauInterface =
    command match {
      case RotateLeft =>
        this.updateRover(rover = rover.copy(facingDirection = rover.facingDirection.rotateAntiClockWise))
      case RotateRight =>
        this.updateRover(rover = rover.copy(facingDirection = rover.facingDirection.rotateClockWise))
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
        if(!grid.getPositionAt((x, y)).isObstacle)
          issueCommandsSequence(getInstructionsToShortestPath((x,y)))
        else throw new IllegalArgumentException("")
    }

  def issueCommandsSequence(commands: Seq[RoverCommand]): PlateauInterface =
    commands.foldLeft(this) { (state, command) =>
      state.issueCommand(command)
    }

  def getInstructionsToShortestPath(target: (Int, Int)): Seq[RoverCommand] = {

    def getRoverAndInstructionToNext(rover: Rover, nextFacing: FacingDirection): Option[(Rover, Seq[RoverCommand])] =
      grid.getPositionOptAt(nextFacing.calculateNextCoordinate {
        if (nextFacing == DOWN || nextFacing == UP) grid.yBorder
        else grid.xBorder
      }(rover.location))
        .map(potNextLoc =>
          if(potNextLoc.isObstacle)
            (rover.copy(obstaclesDetected = rover.obstaclesDetected + potNextLoc), Nil)
          else
          (rover.copy(location = potNextLoc, facingDirection = nextFacing),
            rover.facingDirection.rotateToInstruction(nextFacing) :+ GoForward)
        )

    @tailrec
    def nextAttemptsIfObstacle(rover: Rover, directionSequence: List[FacingDirection]): (Rover, Seq[RoverCommand]) = {
      directionSequence match {
        case head :: otherDirectionsToTry => getRoverAndInstructionToNext(rover, head) match {
          case Some((oldRover, Nil)) =>
            nextAttemptsIfObstacle(oldRover, otherDirectionsToTry)
          case Some(result) =>
            result
        }
        case _ =>
          logger.warn("No path found")
          (rover, Nil)
      }
    }

    @tailrec
    def helper(newInterfaceState: PlateauInterface, commands: Seq[RoverCommand]): Seq[RoverCommand] = {
      val rover = newInterfaceState.rover
      if (target._1 > rover.location.x) {
        val (newRover, newCommands) = nextAttemptsIfObstacle(
          rover, RIGHT +: {
            if (target._2 <= rover.location.y) List(UP, DOWN) else List(DOWN, UP)
          } :+ LEFT)
        helper(newInterfaceState.updateRover(newRover), commands ++ newCommands)
      }
      else if (target._1 < rover.location.x) {
        val (newRover, newCommands) = nextAttemptsIfObstacle(
          rover, LEFT +: {
            if (target._2 < rover.location.y) List(UP, DOWN) else List(DOWN, UP)
          } :+ RIGHT)
        helper(newInterfaceState.updateRover(newRover), commands ++ newCommands)
      } else if (target._2 > rover.location.y) {
        val (newRover, newCommands) = nextAttemptsIfObstacle(
          rover, DOWN +: {
            if (target._1 <= rover.location.x) List(LEFT, RIGHT) else List(RIGHT, LEFT)
          } :+ UP)
        helper(newInterfaceState.updateRover(newRover), commands ++ newCommands)
      } else if (target._2 < rover.location.y) {
        val (newRover, newCommands) = nextAttemptsIfObstacle(
          rover, UP +: {
            if (target._1 <= rover.location.x) List(LEFT, RIGHT) else List(RIGHT, LEFT)
          } :+ DOWN)
        helper(newInterfaceState.updateRover(newRover), commands ++ newCommands)
      } else commands
    }

    helper(this, Nil)
  }

  def printPlateauState: String =

      grid.matrixRepr.map(_.map{
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
    plateau.getPositionAt(x, y) match {
      case square if !square.isObstacle =>
        PlateauInterface(plateau, Rover(square, FacingDirection.translate(initialDirection)))
      case _ =>
        throw new IllegalArgumentException(s"Coordinates $x-$y are occupied.")
    }
}
