import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}

import scala.language.postfixOps

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    implicit lazy val console: Console[IO] = Console.make[IO]

    new MarsRoverExploration()
      .startExploration
  }
}
