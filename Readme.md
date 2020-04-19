# SudokuFX

A project which uses OpenCV to solve Sudoku puzzles with a live display of the computed solution.

![a screenshot of the sudokuFX application](/screenshot.png "A screenshot from the project")

Just show your unsolved Sudoku puzzle to the camera, on the display the solution should appear.

This project only serves educational purposes and shows how to use the OpenCV Java API, for the UI it uses JavaFX. 

This project contains an android version as well, but it is unmaintained at the moment. 

Target audience are Scala developers who want to play around with Image processing, it would need some polishing
to get a deployable and usable version.

# Building

In order to get the project to run you have either install prebuild binaries of OpenCV or build it yourself. There
are some blog posts about this project on my [blog](http://ladstatt.blogspot.com/). I just recently (time of writing: 
early 2020) updated dependencies to the latest versions, as such it should be 'no problem' to get it to run on your 
machine.

You have to provide paths and version of OpenCV, either download a binary version from [their site])(https://opencv.org)
or build it from scratch. After successfully doing so, you have to invoke maven like this

     mvn initialize -Pinstall-opencv -N

which installs the java wrapper to your local maven repository with the correct groupId/artifactId and version coordinates.

I've developed it on OsX, I tested it on Windows as well, at most it should run with small modifications. 

Use at your own risk.

# License

Apache License 2.0





