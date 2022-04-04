import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp, Ref}
import interface.{GetToLocation, GoForward, PlateauInterface, RotateLeft, RotateRight, RoverCommand}
import model.{Plateau, Rover}
import cats.effect.std.{Console, Queue}
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import cats.implicits._
import scala.util.{Success, Try}
import scala.concurrent.duration._

object Main extends IOApp with LazyLogging {

//  val obstacles = List((2,3))
//
//  val newPlateau = new Plateau(6, 6, obstacles)
//
//  val rover = Rover(newPlateau.getPositionAt(4,3))
//
//  PlateauInterface(newPlateau, rover)
//    .issueCommand(GetToLocation(1,1))
//    .issueCommand(GetToLocation(0, 5))

  override def run(args: List[String]): IO[ExitCode] = {
    val console = Console.make[IO]
    val process = for {
      _ <- EitherT.right(console.println("Define environment"))
      _ <- EitherT.right(console.println("Insert Planet max length:"))
      plateauX <- readIntegerFromConsole()
      _ <- EitherT.right(console.println("Insert Planet max height:"))
      plateauY <- readIntegerFromConsole()
      _ <- EitherT.right(console.println("Insert obstacles coordinates; type 'done' when finished."))
//      obstacles <- EitherT(insertObstacles(Set(), console))
      newPlateau = new Plateau(plateauX, plateauY)
      _ <- EitherT.right(console.println("Define starting rover position on the planet"))
      _ <- EitherT.right(console.println("Insert latitude position of the rover:"))
      roverX <- readIntegerFromConsole()
      _ <- EitherT.right(console.println("Insert latitude position of the rover:"))
      roverY <- readIntegerFromConsole()
      _ <- EitherT.right(console.println("Insert facing direction of the rover (N, S, E or W):"))
      roverDir <- EitherT.right(console.readLine)
      _ <- EitherT.right(console.println("Loading exploration interface..."))
//      (roverX, roverY, roverDir, newPlateau) = (2,3,"S", new Plateau(10, 10))
      interface <- EitherT(IO(PlateauInterface.initRover(roverX, roverY, roverDir)(newPlateau)).attempt)
      _ <- EitherT.right(console.println("Insert commands for rover, typing `done` when finished"))
      queue <- EitherT.right(Queue.unbounded[IO, RoverCommand])
      completed <- EitherT.right(Ref.of[IO, Boolean](false))
      x <- insertCommandsLoop(queue, console, completed)
      _ <- EitherT((for {
        x1 <- insertCommandsLoop(queue, console, completed).value.start
        x2 <-  useCommands(interface, queue, console, completed).start
        r1 <- x1.join
        r2 <- x2.join
      } yield  ()).attempt)
    } yield ()

    process.value.flatMap {
      case Right(_) => IO.pure(ExitCode.Success)
      case Left(error) =>
        IO(logger.error("Failure due " + error.getMessage)).map(_ => ExitCode.Error)
    }
  }

//  private def insertObstacles(obstacles: Set[(Int, Int)], console: Console[IO]): EitherT[IO, Throwable, Set[(Int, Int)]] =
//    (for {
//      _ <- console.println("Insert coordinate x and y with a dividing space")
//      input <- console.readLine
//    } yield input) flatMap {
//      case "done" => EitherT.right[Throwable](IO(obstacles))
//      case coordinatesString =>
//        EitherT(IO(Try(coordinatesString.split(" ").take(2))
//          .map(coordinates => (coordinates.head.toInt, coordinates.last.toInt)).toEither))
//          .flatMap(coordinatesTp => insertObstacles(obstacles + coordinatesTp, console))
//    }

  private def readIntegerFromConsole(errorMessage: String = "The value inserted was not an integer")(implicit console: Console[IO]) =
    EitherT(console.readLine.map(_.toInt)
      .handleErrorWith(_ => IO.raiseError(new IllegalArgumentException(errorMessage)))
      .attempt
    )

  private def useCommands(plateauInterface: PlateauInterface, queue: Queue[IO, RoverCommand], console: Console[IO], completed: Ref[IO, Boolean]): IO[Either[Throwable, Any]] = {
    completed.get.flatMap{ completion =>
      queue.tryTake.flatMap {
        case Some(command) =>
          IO(useCommands(plateauInterface.issueCommand(command), queue, console, completed))
        case _ =>
          if(completion)
            IO.unit.attempt
          else
            IO(useCommands(plateauInterface, queue, console, completed))
      }.attempt
    }
  }

  private def insertCommands(queue: Queue[IO, RoverCommand], console: Console[IO])(command: String): IO[Boolean] =
    command.toLowerCase match {
      case "go forward" =>
        queue.tryOffer(GoForward)
      case "rotate right" =>
        queue.tryOffer(RotateRight)
      case "rotate left" =>
        queue.tryOffer(RotateLeft)
      case "go to" =>
        (for {
          _ <- EitherT.right(console.println("Insert target latitude:"))
          targetX <- readIntegerFromConsole()
          _ <- EitherT.right(console.println("Insert target longitude:"))
          targetY <- readIntegerFromConsole()
          res <- EitherT(queue.tryOffer(GetToLocation(targetX, targetY)).attempt)
        } yield res).value.rethrow
      case other =>
        IO.raiseError(new IllegalArgumentException(s"$other is not a valid command"))
    }

  private def insertCommandsLoop(queue: Queue[IO, RoverCommand], console: Console[IO], completed: Ref[IO, Boolean]): EitherT[IO, Throwable, Unit] = {
    val fn = insertCommands(queue, console) _

    def helper(): IO[Unit] = {
      for {
        _ <- console.println("Give a command or type 'done' if you want to end the exploration.")
        commandStd <- console.readLine
        _ <- commandStd match {
          case "done" => completed.set(true)
          case command => fn(command) >> helper()
          case _ => IO.raiseError(new IllegalArgumentException(s"Invalid input"))
        }
      } yield ()
    }
    EitherT(helper().attempt)
  }

}
