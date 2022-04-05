# Mars Rover Challenge

## Brief
The next Mars Rover is being developed, and we need you to come up with a simple way of issuing navigation instructions to it from back on Earth!
We would like you to model the following.
### Part 1: Basic Movement
1. The Mars Rover operates on a grid of arbitrary size.  
2. You can only issue three commands: Move forward, rotate clockwise, and rotate
   anticlockwise. 
3. If the rover moves off the grid, it reappears on the opposite side of the grid. 
### Part 2: Autopilot
1. Devise a simple process for determining the shortest possible path from one position on the grid to another. 
2. Improve the solution so that it can avoid mountain ranges that occupy a number of inconvenient grid squares scattered around the map.

## Run program
You can run with
```
sbt clean run
```
You can enter the parameters when asked.
Follow the instructions, after finishing and typing done when entering the commands, press 'ENTER' button on the keyboard
two to four times consequently, until the program exits.

## Control the rover
The rover can be initialised with a longitude and latitude parameters, and a facing position indicated as a cardinal point letter (N, S, E, W).
The commands available are:
- `go forward`: advance the rover to the current direction by one square
- `rotate left`: rotate the rover in an anticlock fashion
- `rotate right`: rotate the rover in a clock fashion
- `go to`: it takes a x and y parameters (latitude and longitude) to represent a place in the grid to reach.

## Output
The output will be logs in the `exploration` folder. The log name will include the time the exploration was run.
The representation will be the plateau as a grid. `@` indicates the current rover's position, `!^/` represent an obstacles
Other logging will be placed in the `logs` folder.

## Testing
You can run test with 
```
sbt clean test
```
## Current Issues WIP
- As the grid is a wrapping plateau, sometimes the shortest path can be achieved by crossing an edge and appearing at the opposite side. I am devising a method that will take that into account when trying to reach a position in the plateau. (branch `wrapping_autopilot_logic`).
