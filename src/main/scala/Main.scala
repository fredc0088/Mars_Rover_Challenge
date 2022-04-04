import interface.{GetToLocation, PlateauInterface}
import model.{Plateau, Rover}

object Main extends App {

  val obstacles = List((2,3))

  val newPlateau = new Plateau(6, 6, obstacles)

  val rover = Rover(newPlateau.getPositionAt(4,3))

  PlateauInterface(newPlateau, rover)
    .issueCommand(GetToLocation(1,1))
    .issueCommand(GetToLocation(0, 5))

}
