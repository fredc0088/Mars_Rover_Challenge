import model.Position

package object interface {

  sealed trait Facing {
    def calculateNextCoordinate(hardLimit: Int): Position => (Int, Int)
    def rotateAntiClockWise: Facing
    def rotateClockWise: Facing
    def rotateToInstruction(target: Facing): Seq[RoverCommand]
  }
  object Facing {
    def translate(directionAsAString: String): Facing = {
      directionAsAString match {
        case "N" => UP
        case "W" => LEFT
        case "E" => RIGHT
        case "S" => DOWN
        case _   => throw new IllegalArgumentException("Invalid: only accepts W,N,S or E as values.")
      }
    }
  }
  final case object UP extends Facing {
    override def calculateNextCoordinate(hardLimit: Int): Position => (Int, Int) =
      current => (current.x, current.y - 1 match {
        case -1 => hardLimit
        case next => next
      })

    override def rotateAntiClockWise: Facing = LEFT

    override def rotateClockWise: Facing = RIGHT

    override def rotateToInstruction(target: Facing): Seq[RoverCommand] = target match {
      case UP => Nil
      case DOWN => Seq(RotateRight, RotateRight)
      case LEFT => Seq(RotateLeft)
      case RIGHT => Seq(RotateRight)
    }
  }
  final case object DOWN extends Facing {
    override def calculateNextCoordinate(hardLimit: Int): Position => (Int, Int) =
      current => (current.x,  current.y + 1 match {
        case next if next > hardLimit => 0
        case next => next
      })

    override def rotateAntiClockWise: Facing = RIGHT

    override def rotateClockWise: Facing = LEFT

    override def rotateToInstruction(target: Facing): Seq[RoverCommand] = target match {
      case UP => Seq(RotateLeft, RotateLeft)
      case DOWN => Nil
      case LEFT => Seq(RotateRight)
      case RIGHT => Seq(RotateLeft)
    }
  }
  final case object LEFT extends Facing {
    override def calculateNextCoordinate(hardLimit: Int): Position => (Int, Int) =
      current => (current.x - 1 match {
        case -1 => hardLimit
        case next => next
      }, current.y)

    override def rotateAntiClockWise: Facing = DOWN

    override def rotateClockWise: Facing = UP

    override def rotateToInstruction(target: Facing): Seq[RoverCommand] = target match {
      case UP => Seq(RotateRight)
      case DOWN => Seq(RotateLeft)
      case LEFT => Nil
      case RIGHT => Seq(RotateRight, RotateRight)
    }
  }
  final case object RIGHT extends Facing {
    override def calculateNextCoordinate(hardLimit: Int): Position => (Int, Int) =
      current => (current.x + 1 match {
        case next if next > hardLimit => 0
        case next => next
      }, current.y)

    override def rotateAntiClockWise: Facing = UP

    override def rotateClockWise: Facing = DOWN

    override def rotateToInstruction(target: Facing): Seq[RoverCommand] = target match {
      case UP => Seq(RotateLeft)
      case DOWN => Seq(RotateRight)
      case LEFT => Seq(RotateRight, RotateRight)
      case RIGHT => Nil
    }
  }

  sealed trait RoverCommand
  case object RotateLeft extends RoverCommand
  case object RotateRight extends RoverCommand
  case object GoForward extends RoverCommand
  case class GetToLocation(x: Int, y: Int) extends RoverCommand

}
