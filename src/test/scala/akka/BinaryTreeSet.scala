package akka

import akka.BinaryTreeSet.Operation._
import akka.BinaryTreeSet.OperationReply.{ContainsResult, OperationFinished}
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.util.concurrent.atomic.AtomicInteger

class BinaryTreeSetSpec
    extends TestKit(ActorSystem("BinaryTreeSetSpec"))
    with ScalaCheckDrivenPropertyChecks
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  "BinaryTreeSet actor" must {
    "Inserts values" in {
      val actor = system.actorOf(Props(new BinaryTreeSet()))
      val idGen = new AtomicInteger(0)

      forAll { xs: List[Int] =>
        xs.foreach(x => {
          val id = idGen.incrementAndGet()
          actor ! Insert(testActor, id, x)
          expectMsg(OperationFinished(id))

          val id2 = idGen.incrementAndGet()
          actor ! Contains(testActor, id2, x)
          expectMsg(ContainsResult(id2, true))
        })
      }
    }

    "Removes values" in {
      val actor = system.actorOf(Props(new BinaryTreeSet()))
      val idGen = new AtomicInteger(0)
      val max = 100000

      val intBinom = Gen.binomial(Gen.prob(0.5), max).map(_ - max / 2)
      val gen = for {
        i <- Gen.listOf(intBinom)
        r <- Gen.listOf(intBinom)
      } yield (i, r)

      forAll(gen) {
        case (is: List[Int], rs: List[Int]) => {
          is.foreach(x => {
            val id = idGen.incrementAndGet()
            actor ! Insert(testActor, id, x)
            expectMsg(OperationFinished(id))
          })

          rs.foreach(x => {
            val id = idGen.incrementAndGet()
            actor ! Remove(testActor, id, x)
            expectMsg(OperationFinished(id))
          })

          is.filterNot(rs.toSet).foreach(x => {
            val id = idGen.incrementAndGet()
            actor ! Contains(testActor, id, x)
            expectMsg(ContainsResult(id, true))
          })

          rs.foreach(x => {
            val id = idGen.incrementAndGet()
            actor ! Contains(testActor, id, x)
            expectMsg(ContainsResult(id, false))
          })
        }
      }
    }
  }

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }
}
