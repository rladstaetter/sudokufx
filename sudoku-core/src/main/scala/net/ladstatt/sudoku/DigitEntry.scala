package net.ladstatt.sudoku

import org.bytedeco.opencv.opencv_core.Mat

case class DigitEntry(value: Int
                      , timestamp: Long
                      , quality: Double
                      , optMat: Option[Mat])
