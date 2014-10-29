package net.ladstatt.apps.sudoku

import org.opencv.core.Mat

/**
 * the result for one frame. a frame is a image from the image stream
 */

case class ImageIOChain(working: Mat,
                        grayed: Mat,
                        blurred: Mat,
                        thresholded: Mat,
                        inverted: Mat,
                        dilated: Mat,
                        eroded: Mat)
