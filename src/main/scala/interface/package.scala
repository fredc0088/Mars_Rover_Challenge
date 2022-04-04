import model.Position

package object interface {

  sealed trait FacingDirection {
    def calculateNextCoordinate(hardLimit: Int): Position => (Int, Int)
    def rotateAntiClockWise: FacingDirection
    def rotateClockWise: FacingDirection
    def rotateToInstruction(target: FacingDirection): Seq[RoverCommand]
  }
  object FacingDirection {
    def translate(directionAsAString: String): FacingDirection = {
      directionAsAString match {
        case "N" => UP
        case "W" => LEFT
        case "E" => RIGHT
        case "S" => DOWN
        case _   => throw new IllegalArgumentException("Invalid: only accepts W,N,S or E as values.")
      }
    }
  }
  final case object UP extends FacingDirection {
    override def calculateNextCoordinate(hardLimit: Int): Position => (Int, Int) =
      current => (current.x, current.y - 1 match {
        case -1   => hardLimit
        case next => next
      })

    override def rotateAntiClockWise: FacingDirection = LEFT

    override def rotateClockWise: FacingDirection = RIGHT

    override def rotateToInstruction(target: FacingDirection): Seq[RoverCommand] = target match {
      case UP    => Nil
      case DOWN  => Seq(RotateRight, RotateRight)
      case LEFT  => Seq(RotateLeft)
      case RIGHT => Seq(RotateRight)
    }
  }
  final case object DOWN extends FacingDirection {
    override def calculateNextCoordinate(hardLimit: Int): Position => (Int, Int) =
      current => (current.x,  current.y + 1 match {
        case next if next > hardLimit => 0
        case next                     => next
      })

    override def rotateAntiClockWise: FacingDirection = RIGHT

    override def rotateClockWise: FacingDirection = LEFT

    override def rotateToInstruction(target: FacingDirection): Seq[RoverCommand] = target match {
      case UP    => Seq(RotateLeft, RotateLeft)
      case DOWN  => Nil
      case LEFT  => Seq(RotateRight)
      case RIGHT => Seq(RotateLeft)
    }
  }
  final case object LEFT extends FacingDirection {
    override def calculateNextCoordinate(hardLimit: Int): Position => (Int, Int) =
      current => (current.x - 1 match {
        case -1   => hardLimit
        case next => next
      }, current.y)

    override def rotateAntiClockWise: FacingDirection = DOWN

    override def rotateClockWise: FacingDirection = UP

    override def rotateToInstruction(target: FacingDirection): Seq[RoverCommand] = target match {
      case UP    => Seq(RotateRight)
      case DOWN  => Seq(RotateLeft)
      case LEFT  => Nil
      case RIGHT => Seq(RotateRight, RotateRight)
    }
  }
  final case object RIGHT extends FacingDirection {
    override def calculateNextCoordinate(hardLimit: Int): Position => (Int, Int) =
      current => (current.x + 1 match {
        case next if next > hardLimit => 0
        case next                     => next
      }, current.y)

    override def rotateAntiClockWise: FacingDirection = UP

    override def rotateClockWise: FacingDirection = DOWN

    override def rotateToInstruction(target: FacingDirection): Seq[RoverCommand] = target match {
      case UP    => Seq(RotateLeft)
      case DOWN  => Seq(RotateRight)
      case LEFT  => Seq(RotateRight, RotateRight)
      case RIGHT => Nil
    }
  }

  sealed trait RoverCommand
  case object RotateLeft extends RoverCommand
  case object RotateRight extends RoverCommand
  case object GoForward extends RoverCommand
  case class GetToLocation(x: Int, y: Int) extends RoverCommand

}
