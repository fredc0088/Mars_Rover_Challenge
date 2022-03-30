import interface.{GetToLocation, PlateauInterface}
import model.{Grid, Rover}

object Main extends App {

  val d = new Grid(24, 24, Nil)

  val rover = Rover(d.squares(4,6))

  PlateauInterface(d, rover)
    .issueCommand(GetToLocation(22,17))
    .issueCommand(GetToLocation(0, 5))

}
