package net.ladstatt.jfx

import java.util.Timer

/**
 * Created by lad on 09.02.15.
 */
class FrameTimer extends Timer {

  def schedule(task: FrameGrabberTask, delay: Long = 0, period: Long = 1) = {
    super.schedule(task, delay, period)
  }
}
