import com.typesafe.scalalogging
import com.typesafe.scalalogging.Logger
import org.apache.commons.lang3.time.StopWatch
class Logger {
  val logger: scalalogging.Logger = Logger("log")
  val stopWatch = new StopWatch("")

  /**
   * starts the stopwatch
   *
   */
  def startTime():Unit={
    stopWatch.start()
  }

  /**
   * stops the stopwatch
   *
   */
  def stopTime():Unit={
    stopWatch.stop()
  }

  /**
   * get the time between start and stop
   * @return get the time between start and stop in long(milliseconds)
   */
  def getTime:Long={
    stopWatch.getTime()
  }
}