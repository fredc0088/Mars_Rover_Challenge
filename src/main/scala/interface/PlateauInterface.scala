package interface

import cats.effect.Ref
import cats.effect.kernel.Async
import model.{Plateau, Position, Rover}

import scala.annotation.tailrec
import cats.implicits._
import org.typelevel.log4cats.Logger
import util.LoggerWrapper

class PlateauInterface[F[_]](val grid: Plateau, val rover: Rover)
                            (implicit F: Async[F], state: Ref[F, String], logger: LoggerWrapper[F]) {

  private def forward: Position =
    grid.getPositionAt {
      if (rover.facingDirection == UP || rover.facingDirection == DOWN)
        rover.facingDirection.calculateNextCoordinate(grid.yBorder)(rover.location)
      else
        rover.facingDirection.calculateNextCoordinate(grid.xBorder)(rover.location)
    }

  def issueCommand(command: RoverCommand): F[PlateauInterface[F]] =
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
            for {
              updatedPlateau <- this.updateRover(rover = rover.copy(location = nextPosition))
              currentState   <- updatedPlateau.printPlateauState
              _              <- F.delay(logger.info(s"${(updatedPlateau.rover.location.x, updatedPlateau.rover.location.y)}\n$currentState\n\n\n"))
            } yield updatedPlateau
        }
      case GetToLocation(x, y) =>
        if(!grid.getPositionAt((x, y)).isObstacle)
          issueCommandsSequence(getInstructionsToShortestPath((x,y)))
        else throw new IllegalArgumentException(s"Position at $x, $y is already occupied.")
    }

  def issueCommandsSequence(commandsF: F[Seq[RoverCommand]]): F[PlateauInterface[F]] =
    commandsF.flatMap(_.foldLeft(F.delay(this)) { (state, command) =>
      state.flatMap(_.issueCommand(command))
    })

  def getInstructionsToShortestPath(target: (Int, Int)): F[Seq[RoverCommand]] = {

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
    def nextAttemptsIfObstacle(rover: Rover, directionSequence: List[FacingDirection]): F[(Rover, Seq[RoverCommand])] =
      directionSequence match {
        case head :: otherDirectionsToTry =>
          getRoverAndInstructionToNext(rover, head) match {
          case Some((oldRover, Nil)) =>
            nextAttemptsIfObstacle(oldRover, otherDirectionsToTry)
          case Some(result)          =>
            F.delay(result)
        }
        case _                            =>
          F.delay(logger.warn("No path found"))
            .map(_ => (rover, Nil))
      }

    def getNewStateAndRecurse(
      rover: Rover, directionsSeq: List[FacingDirection],
      commands: Seq[RoverCommand]
    )(interface: PlateauInterface[F]) =
      nextAttemptsIfObstacle(rover, directionsSeq)
        .flatMap { case (newRover, newCommands) =>
          interface.updateRover(newRover).flatMap(
            helper(_, commands ++ newCommands)
          )
        }

    def helper(interfaceState: PlateauInterface[F], commands: Seq[RoverCommand]): F[Seq[RoverCommand]] = {
      val rover = interfaceState.rover
      if (target._1 > rover.location.x)
        getNewStateAndRecurse(rover, RIGHT +: {
          if (target._2 <= rover.location.y) List(UP, DOWN) else List(DOWN, UP)
        } :+ LEFT, commands)(interfaceState)
      else if (target._1 < rover.location.x)
        getNewStateAndRecurse(rover, LEFT +: {
          if (target._2 < rover.location.y) List(UP, DOWN) else List(DOWN, UP)
        } :+ RIGHT, commands)(interfaceState)
      else if (target._2 > rover.location.y)
        getNewStateAndRecurse(rover, DOWN +: {
          if (target._1 <= rover.location.x) List(LEFT, RIGHT) else List(RIGHT, LEFT)
        } :+ UP, commands)(interfaceState)
      else if (target._2 < rover.location.y)
        getNewStateAndRecurse(rover, UP +: {
          if (target._1 <= rover.location.x) List(LEFT, RIGHT) else List(RIGHT, LEFT)
        } :+ DOWN, commands)(interfaceState)
      else F.delay(commands)
    }

    helper(this, Nil)
  }

  def printPlateauState: F[String] =
    F.delay(
      grid.matrixRepr.map(_.map{
        case position if(position.isObstacle) => "!^/"
        case rover.location                   => " @ "
        case _                                => "[_]"
      }.mkString).mkString("\n")
    )

  def updateRover(rover: Rover): F[PlateauInterface[F]] = F.delay(PlateauInterface[F](grid, rover))
}

object PlateauInterface {

  def apply[F[_]: Async : LoggerWrapper](grid: Plateau, rover: Rover)(implicit state: Ref[F, String]): PlateauInterface[F] =
    new PlateauInterface[F](grid, rover)

  def initRover[F[_] : LoggerWrapper](x: Int, y: Int, initialDirection: String)(plateau: Plateau)(implicit F: Async[F], state: Ref[F, String]): F[PlateauInterface[F]] =
    F.delay(plateau.getPositionAt(x, y)) flatMap {
      case square if !square.isObstacle =>
        F.delay(PlateauInterface[F](plateau, Rover(square, FacingDirection.translate(initialDirection))))
      case _                            =>
        F.raiseError(new IllegalArgumentException(s"Coordinates $x-$y are occupied."))
    }
}
