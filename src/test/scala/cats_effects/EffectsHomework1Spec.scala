package cats_effects

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

class EffectsHomework1Spec extends AnyFreeSpec with Matchers {

  import EffectsHomework1.IO

  val rnd    = scala.util.Random
  val x: Int = rnd.nextInt(Int.MaxValue / 2)
  val y      = rnd.nextInt(Int.MaxValue / 2)
  val z      = rnd.nextString(5)
  val io     = IO(x)

  "IO monad" - {
    "map works correctly" in {
      val res = io.map(_ + y)
      res.unsafeRunSync() shouldEqual x + y
    }

    "flatMap works correctly" in {
      val res = io.flatMap(x => IO((x + y).toString))
      res.unsafeRunSync() shouldEqual (x + y).toString
    }

    "*> works correctly" in {
      val counter = new AtomicInteger(x)
      val res = new IO(() => counter.incrementAndGet()) *> IO(z)
      res.unsafeRunSync() shouldEqual z
      counter.get shouldEqual x + 1
    }

    "as works correctly" in {
      val counter = new AtomicInteger(x)

      val res = new IO(() => counter.incrementAndGet()).as(z).unsafeRunSync()
      res shouldEqual z
      counter.get shouldEqual x + 1
    }

    "void works correctly" in {
      val counter = new AtomicInteger(x)
      val res = new IO(() => counter.incrementAndGet()).void
      res.unsafeRunSync()
      counter.get shouldEqual x + 1
    }

    "attempt works correctly" in {
      io.attempt.unsafeRunSync() shouldEqual Right(x)

      val ex = new Exception("Test error")
      new IO(() => throw ex).attempt.unsafeRunSync() shouldEqual Left(ex)
    }

    "option works correctly" in {
      io.option.unsafeRunSync() shouldEqual Some(x)
      new IO(() => ???).option.unsafeRunSync() shouldEqual None
    }

    "handleErrorWith works correctly" in {
      io.handleErrorWith(ex => IO(ex.toString)).unsafeRunSync() shouldEqual x
      new IO(() => ???).handleErrorWith(ex => IO(ex.getMessage)).unsafeRunSync() shouldEqual "an implementation is missing"
    }

    "redeem works correctly" in {
      io.redeem(ex => z, x => (x + y).toString).unsafeRunSync() shouldEqual (x + y).toString
      new IO[String](() => ???).redeem(ex => z, x => (x + y).toString).unsafeRunSync() shouldEqual z
    }

    "redeemWith works correctly" in {
      io.redeemWith(ex => IO(z), x => IO((x + y).toString)).unsafeRunSync() shouldEqual (x + y).toString
      new IO[Int](() => ???).redeemWith(ex => IO(z), x => IO((x + y).toString)).unsafeRunSync() shouldEqual z
    }

    "unsafeRunSync works correctly" in {
      val counter = new AtomicInteger(0)
      val res = new IO(() => counter.incrementAndGet()).unsafeRunSync()
      res shouldEqual 1
      counter.get shouldEqual 1
    }

    "unsafeToFuture works correctly" in {
      val counter = new AtomicInteger(x)
      val res = new IO(() => counter.incrementAndGet()).unsafeToFuture()
      Await.result(res, Duration.Inf) shouldEqual x + 1
      counter.get shouldEqual x + 1
    }
  }
}
