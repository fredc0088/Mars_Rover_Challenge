import interface.{GetToLocation, PlateauInterface}
import model.{Plateau, Rover}

object Main extends App {

  val d = new Plateau(6, 6, List((2,3)))

  val rover = Rover(d.getPositionAt(4,3))

  PlateauInterface(d, rover)
    .issueCommand(GetToLocation(1,1))
    .issueCommand(GetToLocation(0, 5))

}
