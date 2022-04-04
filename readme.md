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

The parameters can be defined in Main.scala as per the existing example.

## Output

The output will be logs in the `exploration` folder. The log name will include the time the exploration was run.
The representation will be the plateau as a grid. `@` indicates the current rover's position, `!^/` represent an obstacles
Other logging will be placed in the `logs` folder.

## Testing

You can run test with 
```
sbt clean test
```
