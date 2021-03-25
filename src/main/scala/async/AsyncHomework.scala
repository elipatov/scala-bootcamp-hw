package async

import async.AsyncHomework.parallelism
import cats.Monads.Identity

import java.net.URL
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success}
import cats._
import cats.effect.{Async, IO}
import cats.effect.concurrent.Semaphore
import cats.effect.kernel.{Fiber, Outcome, Sync}
import cats.effect.unsafe.implicits.global
import cats.implicits.{catsSyntaxParallelTraverse, toTraverseFilterOps, toTraverseOps}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.impl.Promise

/**
  * Application:
  * - takes a web-page URL from arguments (args array)
  * - loads the web-page body, extracts HTTP links from it
  * - for all the found links, tries to fetch a server name header if there is one
  * - prints all the encountered unique server name values in alphabetical order
  *
  * Each link processing should be done in parallel.
  * Validation of arguments is not needed.
  *
  * Try to test it on http://google.com!
  */
object AsyncHomework extends App {
  private implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  //put your code there
  val parallelism    = 2
  println(s"Start: ${args.mkString(" ")}")

  val program =
    for {
      throttling <- Semaphore[IO](parallelism)
      _          <- args.toList.parTraverse(processUrlWithThrottling(throttling))
    } yield ()

  program.unsafeRunSync()
  println("Exit")

  def processUrlWithThrottling(semaphore: Semaphore[IO])(url: String) = {
    for {
      _      <- semaphore.acquire
      result <- IO.fromFuture(IO(processUrl(url)))
      _      <- Sync[IO].delay(println(result))
      _      <- semaphore.release
    } yield ()
  }

  private def processUrl(url: String): Future[String] = {
    val result = Promise[String]

    val f =
      for {
        body <- fetchPageBody(url)
        urls <- findLinkUrls(body)
        srvs <- urls.traverseFilter(fetchServerName)
      } yield srvs.sorted.mkString(", ")

    f.onComplete({
      case Failure(ex) => result.success(s"Error: ${ex.getMessage}")
      case Success(value) => result.success(value)
    })

    result.future
  }
  //end of my code

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] =
    Future {
      val linkPattern = """href="(http[^"]+)"""".r
      linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
    }
}
