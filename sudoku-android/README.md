# Sudoku Capturer (for Android)

## Prerequisites

If you want to deploy the app to the appstore, you have to make sure you have a keystore
created. A keystore is necessary for signing the application. There are several tutorials for this on the web.



## Build instructions

To install the application on your device, type:

    mvn clean install android:deploy

For signing and releasing it for the app store:

    mvn clean install -Pandroid,sign
    
Make sure you've defined the necessary properties in your settings.xml, namely
    
    android.keystore
    android.keypass
    android.storepass
    
Of course, you have to create a corresponding keystore and use your own storepass.
    


## TODOS

- create a HUD with a frame so the user knows where to place the sudoku
- show ads only when solution is shown
- translate to chinese
- improvement of detection algorithm for the sudoku corners
- parameter dialog / preferences
  - change minimal number of cells to start solving algorithm
  - change minimal number of hits for a cell
- fix bug that contour lines show in black, not in green (only on samsung2)
- progress bar implementieren: entlang dem bild unten einen bar der von rot auf grün wechselt,
und je nach anzahl der erkannten nummern sich langsam füllt
- mini thumb matrix links oben anzeigen von erkannten zahlen
- integrate opencv correctly (maybe even without opencv manager)


