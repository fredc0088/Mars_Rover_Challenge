import model.Position

package object interface {

  sealed trait Facing
  final case object UP extends Facing
  final case object DOWN extends Facing
  final case object RIGHT extends Facing

  sealed trait RoverCommand
  case object RotateLeft extends RoverCommand
  case object RotateRight extends RoverCommand
  case object GoForward extends RoverCommand
  case class GetToLocation(x: Int, y: Int) extends RoverCommand


}
