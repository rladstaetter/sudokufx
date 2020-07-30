package net.ladstatt.sudoku

sealed trait ImageInput

case object FromFile extends ImageInput

case object FromVideo extends ImageInput
