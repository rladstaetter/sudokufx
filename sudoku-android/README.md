# SudokuFX (for Android)

## Prerequisites

If you want to deploy the app to the appstore, you have to make sure you have a keystore
created. A keystore is necessary for signing the application. There are several tutorials for this on the web.

## Build instructions

To install the application on your device, type:

    mvn clean install
    mvn android:deploy

## For App Store upload

For signing and releasing it for the app store:

    mvn package -Psign,SETTINGS.android

Make sure you've defined the necessary properties in your settings.xml, namely
    
    android.keystore
    android.keypass
    android.storepass
    
Of course, you have to create a corresponding keystore and use your own storepass.

For example, you could place those values in your settings.xml in the SETTINGS.android profile.
 
For upload to app store use 

    target/sudoku-android-aligned.apk.    

via the google play store web interface.



