import cats.effect.{IO, Ref}
import cats.effect.unsafe.IORuntime
import interface.{DOWN, GoForward, PlateauInterface, RotateLeft, RotateRight}
import model.Plateau
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import util.LoggerWrapper

class MarsRoverTest extends AnyFunSpec with Matchers {

  implicit lazy val runtime: IORuntime = cats.effect.unsafe.IORuntime.global
  implicit lazy val logger: LoggerWrapper[IO] = LoggerWrapper[IO](false).unsafeRunSync()

  describe("The Plateau-Rover Interface") {
    implicit def stateRef = Ref.of[IO, String]("").unsafeRunSync()
    describe("when given starting point x and y and an initial direction") {
      it("generates a rover on the grid if the given coordinates are valid") {
        val rover = PlateauInterface.initRover[IO](33, 22, "S")(new Plateau(55,60))
          .unsafeRunSync.rover
        rover.location.x should be (33)
        rover.location.y should be (22)
        rover.facingDirection should be (DOWN)
      }

      it("throws an exception if the given coordinates exists but they are occupied (e.g. by an obstacle)") {
         val error = intercept[IllegalArgumentException] {
          PlateauInterface.initRover[IO](33, 22, "S")(new Plateau(55,60, List((33,22))))
            .unsafeRunSync.rover
        }
        error.getMessage should be ("Coordinates 33-22 are occupied.")
      }

      it("throws an exception if the given coordinates do not exists") {
        val error = intercept[IllegalArgumentException] {
          PlateauInterface.initRover[IO](33, 22, "S")(new Plateau(10,22))
            .unsafeRunSync.rover
        }
        error.getMessage should be ("Coordinates 33-22 are not applicable.")
      }

    }

    describe("has a plateau") {
      it("which is rounded, where going over a border will make appear in the opposite direction") {
        val interfaceInitState = PlateauInterface.initRover[IO](2, 0, "N")(new Plateau(5,5))
        val newState = interfaceInitState.flatMap(_.issueCommand(GoForward))
          .unsafeRunSync()

        newState.rover.location.x should be (2)
        newState.rover.location.y should be (4)
      }
    }

    describe("when give a Rover") {
      describe("sent forward to the facing position") {
        describe("if the rover encounters an obstacle") {
          it("should not send forward the rover") {
            val interfaceInitState = PlateauInterface.initRover[IO](2, 3, "S")(new Plateau(6,5, List((2,4))))
            val newState = interfaceInitState.flatMap(_.issueCommand(GoForward))
              .unsafeRunSync()

            newState.rover.location.x should be (2)
            newState.rover.location.y should be (3)
          }

          it("has the rover memorizing the obstacle's position") {
            val interfaceInitState = PlateauInterface.initRover[IO](2, 3, "S")(new Plateau(5,5, List((2,4))))
            val newState = interfaceInitState.flatMap(_.issueCommand(GoForward))
              .unsafeRunSync()
            newState.rover.obstaclesDetected.map(pos => (pos.x, pos.y)) should be (Set((2,4)))
          }
        }
      }

    }

    describe("when given a target") {
      it("should be able to produce the commands for the rover to reach the target via  the most direct path (with no obstacles)") {
        val newPlanet = new Plateau(24,17)
        val startCoordinate = (3,12)
        val commands = PlateauInterface.initRover[IO](
          startCoordinate._1, startCoordinate._2, "N")(newPlanet)
          .flatMap(_.getInstructionsToShortestPath(18, 5))
          .unsafeRunSync()

        commands.size should be (24)
        commands should equal (Seq(
          RotateRight, GoForward, GoForward, GoForward, GoForward, GoForward, GoForward, GoForward, GoForward, GoForward, GoForward,
          GoForward, GoForward, GoForward, GoForward, GoForward, RotateLeft, GoForward, GoForward, GoForward, GoForward, GoForward,
          GoForward, GoForward
        ))
      }

      it("should be able to produce the commands for the rover to reach the target via the most direct path avoiding a set of obstacles") {
        val obstaclesCoordinates = List((1,1))
        val newPlanet = new Plateau(6,6, obstaclesCoordinates)
        val startCoordinate = (4,1)
        val commands = PlateauInterface.initRover[IO](
          startCoordinate._1, startCoordinate._2, "E")(newPlanet)
          .flatMap(_.getInstructionsToShortestPath(1, 5))
          .unsafeRunSync()

        commands.size should be (12)
        commands should equal (Seq(
          RotateRight, RotateRight, GoForward, GoForward, RotateLeft, GoForward, RotateRight,
          GoForward, RotateLeft, GoForward, GoForward, GoForward
        ))

      }
    }
  }

}
