# SudokuFX

![a screenshot of the sudokuFX application](/screenshot.png "A screenshot from the project")

This is a project demonstrating what is necessary implement a Sudoku grabber and solver using the OpenCV Java API.

Maybe you want to check out a blog post series about this project at http://ladstatt.blogspot.com/ for a more detailled
discussion of the used concepts.

## Building and running

First, you have to make sure that openCV jar is in your local maven repository:

    mvn install:install-file -Dfile=<path-to-file> -DgroupId=<group-id> -DartifactId=<artifact-id> -Dversion=<version> -Dpackaging=<packaging>
    mvn install:install-file -Dfile=/opt/local/share/OpenCV/java/opencv-246.jar -DgroupId=org.opencv -DartifactId=opencv-java -Dversion=2.4.6 -Dpackaging=jar

Then, you have to make sure that the call to loadNativeLib points to the dll / dylib on your system. The newest
opencv dll's can be downloaded from http://opencv.org/.

Finally, you should be able to run the app by issuing

    mvn jfx:run



