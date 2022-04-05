import cats.data.EitherT
import cats.effect.implicits.genSpawnOps
import cats.effect.std.{Console, Queue}
import cats.effect.{Async, ExitCode, IO, Ref}
import cats.implicits._
import interface._
import model.Plateau
import util.LoggerWrapper

import scala.concurrent.duration._
import scala.language.postfixOps

class MarsRoverExploration[F[_]](implicit console: Console[F], F: Async[F]) {

  def startExploration: F[ExitCode] = {

    def process(implicit logger: LoggerWrapper[F]) = for {
      _         <- EitherT.right(console.println("Define environment"))
      plateau   <- generatePlateau
      _         <- EitherT.right(console.println("Define starting rover position on the planet"))
      _         <- EitherT.right(console.println("Insert latitude position of the rover:"))
      roverX    <- readIntegerFromConsole()
      _         <- EitherT.right(console.println("Insert longitude position of the rover:"))
      roverY    <- readIntegerFromConsole()
      _         <- EitherT.right(console.println("Insert facing direction of the rover (N, S, E or W):"))
      roverDir  <- EitherT.right(console.readLine)
      _         <- EitherT.right(console.println("Loading exploration interface..."))
      implicit0(interfaceStateRef: Ref[F, String]) <- EitherT.right(Ref.of[F, String](""))
      interface <- EitherT(PlateauInterface.initRover[F](roverX, roverY, roverDir)(plateau).attempt)
      queue     <- EitherT.right(Queue.unbounded[F, RoverCommand])
      completed <- EitherT.right(Ref.of[F, Boolean](false))
      initial   <- EitherT(interface.printPlateauState
        .flatMap(initialState => interfaceStateRef.updateAndGet(_ => initialState)).attempt)
      _         <- EitherT.right(console.println(s"($roverX $roverY)\n$initial"))
      _         <- EitherT.right(console.println("Insert commands for rover, typing `done` when finished"))
      _         <- processConcurrently(interface, queue, completed)
    } yield ()

    LoggerWrapper[F]() flatMap { logger =>
      process(logger).value.flatMap {
        case Right(_) =>
          F.pure(ExitCode.Success)
        case Left(error) =>
          logger.error("Failure due " + error.getMessage).map(_ => ExitCode.Error)
      }
    }

  }

  private def processConcurrently(
    interface: PlateauInterface[F],
    queue: Queue[F, RoverCommand],
    isCompleted: Ref[F, Boolean]
  )(implicit state: Ref[F, String]) =
    EitherT((
      for {
        x1 <- insertCommandsLoop(queue, isCompleted)(state).value.start
        x2 <- EitherT(useCommands(interface, queue, isCompleted)).map(_ => ()).value.start
        _ <- x1.join
        _ <- x2.join
      } yield ()
    ).attempt)

  private def generatePlateau =
    for {
      _           <- EitherT.right(console.println("Time to create the plateau"))
      _           <- EitherT.right(console.println("Is it a whole planet? y(default) or n"))
      (x, y)      <- EitherT.right(console.readLine).flatMap {
        case "n" =>
          for {
            _         <- EitherT.right(console.println("Insert plateau's max length:"))
            plateauX  <- readIntegerFromConsole()
            _         <- EitherT.right(console.println("Insert plateau's max height:"))
            plateauY  <- readIntegerFromConsole()
          } yield (plateauX, plateauY)
        case _   =>
          EitherT.right(console.println("Insert planet's size:")).flatMap { _ =>
            readIntegerFromConsole().map(size => (size, size))
          }
      }
      _         <- EitherT.right(console.println("Insert obstacles coordinates; type 'done' when finished."))
      obstacles <- insertObstacles(Set())
    } yield new Plateau(x, y, obstacles.toList)

  private def insertObstacles(obstacles: Set[(Int, Int)]): EitherT[F, Throwable, Set[(Int, Int)]] =
    EitherT.right(for {
      _ <- console.println("Insert coordinate x and y with a dividing space")
      input <- console.readLine
    } yield input.trim) flatMap {
      case "done" | ""       => EitherT.right(F.delay(obstacles))
      case coordinatesString =>
        EitherT(F.delay(coordinatesString.split(" ").take(2))
          .map(coordinates => (coordinates.head.toInt, coordinates.last.toInt)).attempt)
          .flatMap(coordinatesTp => insertObstacles(obstacles + coordinatesTp))
    }

  private def readIntegerFromConsole(errorMessage: String = "The value inserted was not an integer") =
    EitherT(console.readLine.map(_.toInt)
      .handleErrorWith(_ => F.raiseError(new IllegalArgumentException(errorMessage)))
      .attempt
    )

  private def useCommands(
    plateauInterface: PlateauInterface[F],
    queue: Queue[F, RoverCommand],
    completed: Ref[F, Boolean]
  )(implicit plateauState: Ref[F, String]): F[Either[Throwable, Unit]] =
    completed.get.flatMap { completion =>
      queue.tryTake.flatMap {
        case Some(command)   =>
          plateauInterface.issueCommand(command) >>
            useCommands(plateauInterface, queue, completed)
        case _ if completion => F.unit.attempt
        case _               => useCommands(plateauInterface, queue, completed)
      }
    }

  private def insertCommands(queue: Queue[F, RoverCommand])(command: String): F[Boolean] =
    command.toLowerCase match {
      case "go forward"   =>
        queue.tryOffer(GoForward)
      case "rotate right" =>
        queue.tryOffer(RotateRight)
      case "rotate left"  =>
        queue.tryOffer(RotateLeft)
      case "go to"        =>
        (for {
          _       <- EitherT.right(console.println("Insert target latitude:"))
          targetX <- readIntegerFromConsole()
          _       <- EitherT.right(console.println("Insert target longitude:"))
          targetY <- readIntegerFromConsole()
          res     <- EitherT(queue.tryOffer(GetToLocation(targetX, targetY)).attempt)
        } yield res).value.rethrow
      case other          =>
        F.raiseError(new IllegalArgumentException(s"$other is not a valid command"))
    }

  private def insertCommandsLoop(
    queue: Queue[F, RoverCommand],
    completed: Ref[F, Boolean]
  )(implicit interfaceRef: Ref[F, String]): EitherT[F, Throwable, Unit] = {

    val fn = insertCommands(queue) _

    def helper: F[Unit] =
      for {
        _ <- console.println("Give a command; type either 'done' or leave empty if you want to end the exploration.")
        _ <- console.println("Check the Readme.md for details on the commands available.")
        commandStd <- console.readLine
        _ <- commandStd.trim match {
          case "done" | "" => completed.set(true)
          case command => fn(command) >>
            F.sleep(1.seconds) >>
            interfaceRef.get.flatMap(interface => console.println(s"Current exploration status:\n$interface\n")) >>
            helper
          case _ => F.raiseError(new IllegalArgumentException(s"Invalid input"))
        }
      } yield ()

    EitherT(helper.attempt)
  }

}
