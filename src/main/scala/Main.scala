import interface.{GetToLocation, PlateauInterface}
import model.{Plateau, Rover}

object Main extends App {

  val d = new Plateau(24, 24, List((16,3), (17,4), (23,23), (8,2), (17,9)))

  val rover = Rover(d.getPositionAt(4,6))

  PlateauInterface(d, rover)
    .issueCommand(GetToLocation(22,17))
    .issueCommand(GetToLocation(0, 5))

}
