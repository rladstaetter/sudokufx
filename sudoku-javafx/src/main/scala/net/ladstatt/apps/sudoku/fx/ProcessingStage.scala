package net.ladstatt.apps.sudoku.fx

/**
 * Created by lad on 10.02.15.
 */
sealed trait ProcessingStage

case object InputStage extends ProcessingStage

case object GrayedStage extends ProcessingStage

case object BlurredStage extends ProcessingStage

case object ThresholdedStage extends ProcessingStage

case object InvertedStage extends ProcessingStage

case object DilatedStage extends ProcessingStage

case object ErodedStage extends ProcessingStage

case object SolutionStage extends ProcessingStage
