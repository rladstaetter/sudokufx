# SudokuFX

A project which uses JavaCV (OpenCV) to solve Sudoku puzzles with a live display of the computed solution.

![a screen recording for SudokuFX](/demo.gif "A screen recording for SudokuFX")

Just show your unsolved Sudoku puzzle to the camera, on the display a solution should appear in a short timeframe. If a Sudoku was not recognized properly the videostream flickers for an instant.

This project only serves educational purposes and shows how to use JavaCV and - under the hood- OpenCV features and JavaFX GUI programming.

Target audience are Scala/Java developers who want to play around with image processing. 

# Building

One feature of this project is that it should run out of the box with minimal setup. Given you have installed Jdk11 and Maven in a recent version, all you have to do to build it would be to execute

    mvn package
    
in the root directory.

For executing SudokuFX on the command line, change to the sudoku-javafx subdirectory and enter

    mvn javafx:run

# Documentation

This project is mainly un - documented, but some blog posts exist, check them out at [http://ladstatt.blogspot.com/](http://ladstatt.blogspot.com/).

SudokuFX was developed on OsX and was tested on Windows, but there should be no reason why it wouldn't run on Linux.

Attention: This project contains also an android (legacy) version, but it is unmaintained at the moment and won't compile. 

# License

Apache License 2.0
