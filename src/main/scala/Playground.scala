import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object Playground extends App{
  val rnd = ThreadLocalRandom.current()

  private def rndNumber(min: Int, max: Int): Int = {
    rnd.nextInt(max - min) + min
  }

  println(rndNumber(5, 18))
}