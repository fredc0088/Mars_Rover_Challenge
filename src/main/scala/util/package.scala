import cats.effect.Async
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.implicits._

package object util {

  class LoggerWrapper[F[_]]private(loggerOpt: Option[Logger[F]])(implicit F: Async[F]) {

    def info(message: String): F[Unit] =
      logOr(_.info(message))

    def warn(message: String): F[Unit] =
      logOr(_.warn(message))

    def error(message: String): F[Unit] =
      logOr(_.error(message))

    private def logOr(f: Logger[F] => F[Unit]): F[Unit] =
      loggerOpt.map(f).getOrElse(F.unit)
  }
  object LoggerWrapper {
    def apply[F[_]: Async](logging: Boolean = true): F[LoggerWrapper[F]] =
      Slf4jLogger.create[F].map(
        logger =>
          if(logging) new LoggerWrapper(logger.some)
          else new LoggerWrapper(None)
      )
  }
}
