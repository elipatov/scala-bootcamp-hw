package http

import cats.data.EitherT
import cats.{Applicative, Monad}
import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ContextShift, Effect, ExitCode, IO, IOApp, Sync}
import cats.syntax.all._
import http.GuessClient.uri
import http.GuessGame.ValidationError.{MinMax, SmallRange}
import http.GuessGame.{ValidationError, maxAttempts, minRange}
import http.Protocol.{
  GuessGame,
  GuessResult,
  MoveRequest,
  MoveResponse,
  NewGameRequest,
  NewGameResponse
}
import org.http4s.Status.Successful
import org.http4s.circe.CirceEntityCodec._
import org.http4s.client.Client
import org.http4s.{HttpRoutes, _}
import org.http4s.dsl.io.{->, /, Ok, POST, Root, _}
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.multipart.{Multipart, Part}
import org.http4s.server.blaze.BlazeServerBuilder

import java.time.Instant
import java.util.concurrent.ThreadLocalRandom
import scala.concurrent.ExecutionContext
import scala.util.Random

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.

// Models that are shared between `HttpServer` and `HttpClient` below.
object Protocol {
  trait GuessGame[F[_]] {
    def create(gameParams: NewGameRequest): F[Either[ValidationError, String]]
    def guess(move: MoveRequest): F[Option[GuessResult]]
  }

  final case class NewGameRequest(min: Int, max: Int)
  final case class NewGameResponse(id: String)

  final case class MoveRequest(gameId: String, guess: Int)
  final case class MoveResponse(result: GuessResult)

  sealed trait GuessResult
  object GuessResult {
    final case object Equals           extends GuessResult
    final case object Greater          extends GuessResult
    final case object Less             extends GuessResult
    final case object AttemptsExceeded extends GuessResult
  }
}

/*****************-Server-*********************/
object GuessServer extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      app <- GuessGame.of[IO]
      routes = new HttpGuessGame(app).routes
      _ <- httpServer(routes.orNotFound)
    } yield ExitCode.Success
  }

  private def httpServer(httpApp: HttpApp[IO]): IO[Unit] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 8080, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
}

class HttpGuessGame(game: GuessGame[IO]) {
  import io.circe.Codec
  import io.circe.generic.auto._
  import io.circe.generic.extras.semiauto.deriveEnumerationCodec
  implicit val validationErrorCodec: Codec[ValidationError] =
    deriveEnumerationCodec[ValidationError]
  implicit val guessResultCodec: Codec[GuessResult] = deriveEnumerationCodec[GuessResult]

  def routes: HttpRoutes[IO] = {
    newGame <+> move
  }

  private val newGame = {
    HttpRoutes.of[IO] {
      // curl -XPOST "localhost:8080/game" -i -d '{"min": 5, "max": 18}' -H "Content-Type: application/json"
      case req @ POST -> Root / "game" =>
        val response =
          for {
            ngr <- req.as[NewGameRequest]
            ng  <- game.create(ngr)
            res = ng match {
              case Right(id) => Ok(NewGameResponse(id))
              case Left(err) => BadRequest(err)
            }
          } yield res

        response.flatten
    }
  }

  private val move = {
    HttpRoutes.of[IO] {
      // curl -XPOST "localhost:8080/move" -i -d '{"gameId": "1", "guess": 5}' -H "Content-Type: application/json"
      case req @ POST -> Root / "move" =>
        val response =
          for {
            mv <- req.as[MoveRequest]
            r  <- game.guess(mv)
            res = r match {
              case Some(value) => Ok(MoveResponse(value))
              case None        => NotFound(s"Game with id ${mv.gameId} not found")
            }
          } yield res

        response.flatten
    }
  }
}

object GuessGame {
  val minRange    = 10
  val maxAttempts = 5

  abstract sealed class ValidationError(val message: String) extends Throwable
  object ValidationError {
    final case object MinMax extends ValidationError("min have to be less than max")
    final case object SmallRange
        extends ValidationError(s"Range smaller than $minRange not allowed")
  }

  def of[F[_]: Sync]: F[GuessGame[F]] =
    for {
      idGen <- Ref.of[F, Long](0)
      games <- Ref.of[F, Map[String, GameState]](Map.empty)
      rnd = Sync[F].delay(ThreadLocalRandom.current())
    } yield new GuessGameImpl(idGen, games, rnd)
}

private final case class GameState(number: Int, attempt: Int)
private final class GuessGameImpl[F[_]: Monad](
    idGen: Ref[F, Long],
    games: Ref[F, Map[String, GameState]],
    rnd: F[ThreadLocalRandom]
) extends GuessGame[F] {
  override def create(gameParams: NewGameRequest): F[Either[ValidationError, String]] = {
    validate(gameParams).traverse { game =>
      for {
        id  <- idGen.modify(x => (x + 1, (x + 1).toString))
        num <- rndNumber(game)
        _   <- games.update(_.updated(id, GameState(num, 0)))
      } yield id
    }
  }

  override def guess(move: MoveRequest): F[Option[GuessResult]] = {
    for {
      gs <- games.updateAndGet { map =>
        map
          .get(move.gameId)
          .map(s => GameState(s.number, s.attempt + 1))
          .map(map.updated(move.gameId, _))
          .getOrElse(map)
      }
    } yield gs
      .get(move.gameId)
      .map(s =>
        if (s.attempt > maxAttempts) GuessResult.AttemptsExceeded
        else if (move.guess < s.number) GuessResult.Greater
        else if (move.guess > s.number) GuessResult.Less
        else GuessResult.Equals
      )
  }

  private def rndNumber(gameParams: NewGameRequest): F[Int] = {
    rnd.map(r => r.nextInt(gameParams.min, gameParams.max))
  }

  private def validate(gameParams: NewGameRequest): Either[ValidationError, NewGameRequest] =
    for {
      _ <- Either.cond(gameParams.min < gameParams.max, gameParams, MinMax)
      _ <- Either.cond(gameParams.max - gameParams.min >= minRange, gameParams, SmallRange)
    } yield gameParams
}

/*****************-Client-*********************/
object GuessClient extends IOApp {
  private val uri = uri"http://localhost:8080"

  def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO])
      .use {
        case (httpClient, blocker) =>
          for {
            client <- IO(new GuessGameClient(httpClient, uri))
            min = 5
            max = 60
            ngResp <- client.create(NewGameRequest(min, max))
            _      <- printLine(s"New game: $ngResp")
            num    <- ngResp.traverse(id => play(client, id, min, max))
            _      <- printLine(s"Finish Game($ngResp) => $num")
          } yield ()
      }
      .as(ExitCode.Success)
  }

  private def play(client: GuessGameClient, gameId: String, min: Int, max: Int): IO[Option[Int]] = {
    val mid = min + (max - min) / 2
    for {
      resp <- client.guess(MoveRequest(gameId, mid))
      _    <- printLine(s"Guess($mid) => $resp")
      num <- resp.traverse {
        case GuessResult.AttemptsExceeded => IO.pure(None)
        case GuessResult.Equals           => IO.pure(Option(mid))
        case GuessResult.Greater          => play(client, gameId, mid + 1, max)
        case GuessResult.Less             => play(client, gameId, min, mid - 1)
      }
    } yield num.flatten
  }

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))
}

private final class GuessGameClient(client: Client[IO], uri: Uri) extends GuessGame[IO] {
  import io.circe.Codec
  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._
  import io.circe.generic.extras.semiauto.deriveEnumerationCodec
  implicit val validationErrorCodec: Codec[ValidationError] =
    deriveEnumerationCodec[ValidationError]
  implicit val guessResultCodec: Codec[GuessResult] = deriveEnumerationCodec[GuessResult]

  override def create(gameParams: NewGameRequest): IO[Either[ValidationError, String]] = {
    for {
      req <- Method.POST.apply(gameParams, uri / "game")
      resp <- client.run(req).use {
        case Successful(r) => r.as[NewGameResponse].map(Right(_))
        case resp          => resp.as[ValidationError].map(Left(_))
      }
    } yield resp.map(_.id)
  }

  override def guess(move: MoveRequest): IO[Option[GuessResult]] = {
    for {
      req  <- Method.POST.apply(move, uri / "move")
      resp <- client.expectOption[MoveResponse](req)
    } yield resp.map(_.result)
  }
}
