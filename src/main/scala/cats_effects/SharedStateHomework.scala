package cats_effects

import cats.Monad
import cats.implicits._
import cats.effect.concurrent.Ref
import cats.effect.{Async, Clock, Concurrent, ExitCode, IO, IOApp, Sync, Timer}
import cats.effect.implicits._
import typeclass.QAndAExamples.Applicative

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{FiniteDuration, _}

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]
    def put(key: K, value: V): F[Unit]
    def close: F[Unit]
  }

  abstract class RefCache[F[_]: Clock: Monad, K, V](
      state: Ref[F, Map[K, (Long, V)]],
      running: Ref[F, Boolean],
      expiresIn: FiniteDuration
  ) extends Cache[F, K, V] {
    def get(key: K): F[Option[V]] = {
      for {
        nowMs <- Clock[F].realTime(MILLISECONDS)
        map <- state.getAndUpdate(m => {
          m.get(key) match {
            case Some((_, v)) =>
              m + (key -> (nowMs + expiresIn.toMillis, v)) // Extend TTL on access
            case None => m
          }
        })
      } yield map.get(key).map({ case (_, v) => v })
    }

    def put(key: K, value: V): F[Unit] = {
      for {
        nowMs <- Clock[F].realTime(MILLISECONDS)
        _     <- state.getAndUpdate(m => m + (key -> (nowMs + expiresIn.toMillis, value)))
      } yield ()
    }

    def close: F[Unit] = running.set(false)

    def runMaintenanceLoop(
        runInterval: FiniteDuration
    )(implicit T: Timer[F], C: Concurrent[F]): F[Unit] = {
      for {
        _   <- T.sleep(runInterval)
        _   <- evictCache()
        run <- running.get
        _   <- if (run) runMaintenanceLoop(runInterval) else Monad[F].pure(())
      } yield ()
    }

    private def evictCache(): F[Unit] = {
      for {
        nowMs <- Clock[F].realTime(MILLISECONDS)
        _ <- state.update(_.filter {
          case (k, (expMs, v)) => nowMs < expMs
        })
      } yield ()
    }
  }

  object Cache {
    def of[F[_]: Clock: Monad, K, V](
        expiresIn: FiniteDuration,
        checkOnExpirationsEvery: FiniteDuration
    )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
      for {
        state   <- Ref.of[F, Map[K, (Long, V)]](Map.empty)
        running <- Ref.of(true)
        cache   <- new RefCache[F, K, V](state, running, expiresIn) {}.pure[F]
        _       <- C.start(cache.runMaintenanceLoop(checkOnExpirationsEvery))
      } yield cache
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](5.seconds, 4.seconds)
      _     <- cache.put(1, "Hello")
      _     <- cache.put(2, "World")
      _     <- cache.get(1).flatMap(s => printIO(s"first key $s"))
      _     <- cache.get(2).flatMap(s => printIO(s"second key $s"))
      _     <- delay(4.seconds)
      _     <- cache.get(1).flatMap(s => printIO(s"first key $s"))
      _     <- cache.get(2).flatMap(s => printIO(s"second key $s"))
      _     <- delay(6.seconds)
      _     <- cache.get(1).flatMap(s => printIO(s"first key $s"))
      _     <- cache.get(2).flatMap(s => printIO(s"second key $s"))
      _     <- delay(10.seconds)
      _     <- cache.get(1).flatMap(s => printIO(s"first key $s"))
      _     <- cache.get(2).flatMap(s => printIO(s"second key $s"))
      _     <- cache.close
    } yield ExitCode.Success
  }

  private def delay(duration: FiniteDuration): IO[Unit] = {
    IO { println(s">> sleep $duration") } *>
      IO.sleep(duration)
  }

  private def printIO(msg: String): IO[Unit] = IO { println(s"first key $msg") }
}
