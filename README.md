# maze navigating robot ocaml

You must write a program for a robot that navigates a closed maze in search for letters that were printed on the floor of the maze. The solution consists of the list of all letters, ordered alphabetically. The letters are capitals (A-Z) and the same letter may exist in multiple locations.

## Example
The solution for the following maze is AABQ:

```
#############
#   A   #   #
# ## ## # # #
# Q# #   B  #
#  #A# # ####
#############
```

`#` symbols indicate walls, which are illegal positions. Your robot can navigate on all other cells vertically or horizontally but not diagonally. You can see the 4 neighbors cells (up, down, left, right) around your current cell.

The initial position of the robot can be any non-wall cell (empty or letter), inside the maze.

## Navigation API
Your program will read cell information from stdin, then write a command on stdout, and loop.

Cell information is given as one line of 5 characters indicating the type of the current cell and the type of each of the neighbor cells in the following order: current, up, down, left, right. In the example, if the current position is the position of the letter Q, cell information will be given as:

```
Q#  #
```
A command is either one of the 4 navigation commands indicating which neighbor cell to move to (U: up, D: down, L: left, R: right) or the key for escaping the maze starting with the K prefix, e.g. KAABQ which terminates the game. Commands are terminated by a newline character (LF).

An attempt to move into a wall or giving a wrong key terminates the life of the poor robot.

Here is a very simple maze:

```
####
#AB#
####
```

A successful session for this maze, assuming a start position on cell marked with A is:

```
 input: A###B
output: R
 input: B##A#
output: KAB
```

## Restrictions
The number of moves is limited to at most twice the number of moves required by our reference solution. This limit as well as the size of the maze remain undisclosed (edited 2015-03-17).

Your program may not consult files or network resources other than stdin and stdout as specified above. It must consist in human-readable source code written by yourself. We will build it and run it on Ubuntu 14.04 (64 bits). Please provide:

the list of packages needed to build and run your solution or your best guess
a shell script to build your solution in one shot
the command to run your solution
Recommendations
Make sure your solution is successful on the sample mazes on this page, and make sure to flush stdout after printing each command when your program runs non-interactively.
