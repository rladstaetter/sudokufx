package net.ladstatt.apps.sudoku.android

import android.os.Bundle
import android.preference.PreferenceFragment


object SudokuPreferences extends PreferenceFragment {
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)

    // Load the preferences from an XML resource
    addPreferencesFromResource(R.xml.preferences)
  }
}
