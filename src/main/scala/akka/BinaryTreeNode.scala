package akka

import akka.BinaryTreeNode.Position
import akka.BinaryTreeSet.OperationReply
import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive

object BinaryTreeNode {
  private sealed trait Position

  private case object Left  extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props =
    Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  private final case class State(subtrees: Map[Position, ActorRef], removed: Boolean)
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  override def receive: Receive = awaitCommand(State(Map[Position, ActorRef](), initiallyRemoved))

  private def awaitCommand(state: State): Receive =
    LoggingReceive {
      case m: Insert   => doInsert(state, m)
      case m: Contains => doContains(state, m)
      case m: Remove   => doRemove(state, m)
    }

//  private def awaitReply(client: ActorRef, state: State): Receive =
//    LoggingReceive {
//      case m: OperationReply => client ! m
////    case m: OperationFinished => client ! m
////    case m: ContainsResult =>
//    }

  private def doInsert(state: State, m: Insert): Unit = {
    if (m.elem == elem) {
      context.become(awaitCommand(state.copy(removed = false)))
      m.requester ! OperationFinished(m.id)
      return
    }

    val position = if (m.elem < elem) Left else Right
    if (state.subtrees.contains(position)) {
      state.subtrees(position) ! m
    } else {
      val actorRef   = context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false))
      val newSubtree = state.subtrees + (position -> actorRef)
      context.become(awaitCommand(state.copy(subtrees = newSubtree)))
      m.requester ! OperationFinished(m.id)
    }
  }

  private def doContains(state: State, m: Contains): Unit = {
    if (m.elem == elem) {
      m.requester ! ContainsResult(m.id, !state.removed)
      return
    }

    val position = if (m.elem < elem) Left else Right
    if (state.subtrees.contains(position)) {
      state.subtrees(position) ! m
    } else {
      m.requester ! ContainsResult(m.id, false)
    }
  }

  private def doRemove(state: State, m: Remove): Unit = {
    if (m.elem == elem) {
      context.become(awaitCommand(state.copy(removed = true)))
      m.requester ! OperationFinished(m.id)
      return
    }

    val position = if (m.elem < elem) Left else Right
    if (state.subtrees.contains(position)) {
      state.subtrees(position) ! m
    } else {
      m.requester ! OperationFinished(m.id)
    }
  }
}
