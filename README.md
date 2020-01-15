# BearMaze

This lab uses the concept of assembly, a custom made pcb board called the CSULB Shield, and an Arduino Uno to replicate a bear
going thrugh a maze collection up to 15 bees and reaching the end of the maze.

The CSULB Shield has 2 dip switchs, a 7 segment LED display, 2 timers, and 2 push buttons.  The 7 segment LED display acts as a directional guide for our bear in the type of room and direction the bear is facing.  The maze is already given, but there is a specific spot that the bear must travel to before leaveing the maze with up to 15 bees.  This number was determined by the last 4 digits of my school id, 91 78, converted into hexadecimal value.  The bear must also reach a dead end and turn around before leaving the maze.

Finite state machines were used for the bear when it would enter a room and check the surrounding area and turn in certain directions through the maze.  From check if certain regsters are set to a high value and comparing specific register and position values in the loaded maze, the bear can successfully navigate a pre set course through the maze.
