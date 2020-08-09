# SudokuFX

A project which uses OpenCV to solve Sudoku puzzles with a live display of the computed solution.

![a screen recording for SudokuFX](/demo.gif "A screen recording for SudokuFX")

Just show your unsolved Sudoku puzzle to the camera, on the display a solution should appear in a short timeframe. If a Sudoku was not recognized properly the videostream flickers for an instant.

This project only serves educational purposes and shows how to use JavaCV and - under the hood- OpenCV features. Furthermore it showcases a basic usage for a JavaFX UI.

Target audience are Java / Scala developers who want to play around with image processing. This project needs still a lot of polishing for an end user, but it could serve for other experiments in this fascinating field. 

# Building

One feature of this project is that it should run out of the box. Given you have installed JDK11 and Maven in a recent version, all you have to do to build it would be to execute

    mvn package
    
in the root directory.

For executing SudokuFX on the command line, change to the sudoku-javafx directory and enter

    mvn javafx:run

# Documentation

This project is mainly un - documented, but some blog posts exist, check them out at [http://ladstatt.blogspot.com/](http://ladstatt.blogspot.com/).

SudokuFX was developped on OsX and was tested on Windows as well, chances are that a Linux build would run as well. 

Attention: This project contains an android version, but it is unmaintained at the moment. 

# License

Apache License 2.0





