import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.matching.Regex

object Playground extends App{
  implicit class RegexOps(sc: StringContext) {
    def r = {
      val regex = sc.parts.mkString
      val map = sc.parts.tail.map(_ => "x")
      new Regex(regex, map: _*)
    }
  }

  val testing = Some("newGame(1,3)\n")

  testing match {
    case Some(r"newGame\((\d{1,9})$min,[ ]?(\d{1,9})$max\)\W*") => println(s"$min $max")
    case _ => println("NA")
  }

//  "10+15" match {
//    case r"(\d{1,9})${first}\+[ ]?(\d\d)${second}" => println(first.toInt+second.toInt)
//    case _ => println("0")
//  }
}