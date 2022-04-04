import cats.data.EitherT
import cats.effect.std.{Console, Queue}
import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import interface._
import model.Plateau

import scala.concurrent.duration._
import scala.language.postfixOps

object Main extends IOApp with LazyLogging {

  override def run(args: List[String]): IO[ExitCode] = {

    implicit val console: Console[IO] = Console.make[IO]

    val process = for {
      _         <- EitherT.right(console.println("Define environment"))
      _         <- EitherT.right(console.println("Insert Planet max length:"))
      plateauX  <- readIntegerFromConsole()
      _         <- EitherT.right(console.println("Insert Planet max height:"))
      plateauY  <- readIntegerFromConsole()
      _         <- EitherT.right(console.println("Insert obstacles coordinates; type 'done' when finished."))
      obstacles <- insertObstacles(Set(), console)
      newPlateau = new Plateau(plateauX, plateauY, obstacles.toList)
      _         <- EitherT.right(console.println("Define starting rover position on the planet"))
      _         <- EitherT.right(console.println("Insert latitude position of the rover:"))
      roverX    <- readIntegerFromConsole()
      _         <- EitherT.right(console.println("Insert latitude position of the rover:"))
      roverY    <- readIntegerFromConsole()
      _         <- EitherT.right(console.println("Insert facing direction of the rover (N, S, E or W):"))
      roverDir  <- EitherT.right(console.readLine)
      _         <- EitherT.right(console.println("Loading exploration interface..."))
      implicit0(interfaceRef: Ref[IO, String]) <- EitherT.right(Ref.of[IO, String](""))
      interface <- EitherT(IO(PlateauInterface.initRover[IO](roverX, roverY, roverDir)(newPlateau)).attempt)
      queue     <- EitherT.right(Queue.unbounded[IO, RoverCommand])
      completed <- EitherT.right(Ref.of[IO, Boolean](false))
      _         <- EitherT(interface.printPlateauState.flatTap(interfaceRef.set).attempt)
      _         <- EitherT.right(console.println("Insert commands for rover, typing `done` when finished"))
      _         <- processConcurrently(interface, queue, completed)
    } yield ()

    process.value.flatMap {
      case Right(_) =>
        IO.pure(ExitCode.Success)
      case Left(error) =>
        IO(logger.error("Failure due " + error.getMessage)).map(_ => ExitCode.Error)
    }

  }

  private def processConcurrently(
    interface: PlateauInterface[IO],
    queue: Queue[IO, RoverCommand],
    isCompleted: Ref[IO, Boolean]
  )(implicit state: Ref[IO, String], console: Console[IO]) =
    EitherT((
      for {
        x1 <- insertCommandsLoop(queue, console, isCompleted)(state).value.start
        x2 <- EitherT(useCommands(interface, queue, console, isCompleted)).map(_ => ()).value.start
        _ <- x1.join
        _ <- x2.join
      } yield ()
    ).attempt)

  private def insertObstacles(obstacles: Set[(Int, Int)], console: Console[IO]): EitherT[IO, Throwable, Set[(Int, Int)]] =
    EitherT.right(for {
      _ <- console.println("Insert coordinate x and y with a dividing space")
      input <- console.readLine
    } yield input.trim) flatMap {
      case "done" | ""       => EitherT.right(IO(obstacles))
      case coordinatesString =>
        EitherT(IO(coordinatesString.split(" ").take(2))
          .map(coordinates => (coordinates.head.toInt, coordinates.last.toInt)).attempt)
          .flatMap(coordinatesTp => insertObstacles(obstacles + coordinatesTp, console))
    }

  private def readIntegerFromConsole(errorMessage: String = "The value inserted was not an integer")(implicit console: Console[IO]) =
    EitherT(console.readLine.map(_.toInt)
      .handleErrorWith(_ => IO.raiseError(new IllegalArgumentException(errorMessage)))
      .attempt
    )

  private def useCommands(
    plateauInterface: PlateauInterface[IO],
    queue: Queue[IO, RoverCommand],
    console: Console[IO],
    completed: Ref[IO, Boolean]
  )(implicit plateauState: Ref[IO, String]): IO[Either[Throwable, Any]] =
    completed.get.flatMap { completion =>
      queue.tryTake.flatMap {
        case Some(command)   =>
          plateauInterface.issueCommand(command) >> IO(useCommands(plateauInterface, queue, console, completed))
        case _ if completion => IO.unit.attempt
        case _               => useCommands(plateauInterface, queue, console, completed)
      } attempt
    }

  private def insertCommands(queue: Queue[IO, RoverCommand], console: Console[IO])(command: String): IO[Boolean] =
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
        IO.raiseError(new IllegalArgumentException(s"$other is not a valid command"))
    }

  private def insertCommandsLoop(
    queue: Queue[IO, RoverCommand],
    console: Console[IO],
    completed: Ref[IO, Boolean]
  )(implicit interfaceRef: Ref[IO, String]): EitherT[IO, Throwable, Unit] = {

    val fn = insertCommands(queue, console) _

    def helper: IO[Unit] =
      for {
        _ <- console.println("Give a command; type either 'done' or leave empty if you want to end the exploration.")
        _ <- console.println("Check the Readme.md for details on the commands available.")
        commandStd <- console.readLine
        _ <- commandStd.trim match {
          case "done" | "" => completed.set(true)
          case command => fn(command) >>
            IO.sleep(1.seconds) >>
            interfaceRef.get.flatMap(interface => console.println(s"Current exploration status:\n$interface\n")) >>
            helper()
          case _ => IO.raiseError(new IllegalArgumentException(s"Invalid input"))
        }
      } yield ()

    EitherT(helper.attempt)
  }

}
