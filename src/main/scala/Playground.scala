import cats.data.State

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import cats.syntax.all
import cats.effect.syntax.all

object Playground {
  final case class Robot(id: Long, sentient: Boolean, name: String, model: String)

  final case class Seed(long: Long) {
    def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
  }

  val nextLong: State[Seed, Long] = State(seed =>
    (seed.next, seed.long))

  val nextBoolean: State[Seed, Boolean] = nextLong.map(long =>
    long >= 0)

  val createRobot: State[Seed, Robot] =
    for {
      id <- nextLong
      sentient <- nextBoolean
      isCatherine <- nextBoolean
      name = if (isCatherine) "Catherine" else "Carlos"
      isReplicant <- nextBoolean
      model = if (isReplicant) "replicant" else "borg"
    } yield Robot(id, sentient, name, model)

  val createRobot2: State[Seed, Robot] = {
    nextLong
      .flatMap(id =>
        nextBoolean
          .map(sentient => Robot(id, sentient, "name", "model"))
      )
  }
}
