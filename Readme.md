# SudokuFX

![a screenshot of the sudokuFX application](/screenshot.png "A screenshot from the project")

This is a project demonstrating what is necessary implement a Sudoku grabber and solver using the OpenCV Java API.

Maybe you want to check out a blog post series about this project at http://ladstatt.blogspot.com/ for a more detailled
discussion of the used concepts.

# Building

SudokuFX uses maven as build tool. One dependency is opencv 3.1, which you will
have to deploy yourself in your local repository. See INSTALL.txt.

Afterwards the project should build with

    mvn package

You should be able to run the JavaFX application by executing:


    java -jar sudoku-javafx/target/
# License

Apache License 2.0





