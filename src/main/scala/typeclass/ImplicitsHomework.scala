package typeclass

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.util.control.Breaks.break

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {
  /**
    * Lo and behold! Brand new super-useful collection library for Scala!
    *
    * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
    * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
    * of the data stored.
    *
    * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
    * a thing called size score. Its calculation rules:
    * - size score of a Byte is 1
    * - Int - 4 (as primitive JVM int consists of 4 bytes)
    * - Long - 8
    * - Char - 2 (one UTF-16 symbol is 2 bytes)
    * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
    * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
    * the fields
    * - score for any sequence (Array[T], List[T], Vector[T]) is
    * 12 (our old friend object header) + sum of scores of all elements
    * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
    */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {
      object GetSizeScore {
        def apply[T](implicit instance: GetSizeScore[T]): GetSizeScore[T] =
          instance
      }

      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = GetSizeScore[T].apply(inner)
      }

      object Iterate {
        def apply[F[_]](implicit instance: Iterate[F]): Iterate[F] = instance
      }

      implicit class IterateOps[F[_]: Iterate, T: GetSizeScore](inner: F[T]) {
        def iterator: Iterator[T] = Iterate[F].iterator(inner)
      }

      object Iterate2 {
        def apply[F[_, _]](implicit instance: Iterate2[F]): Iterate2[F] = instance
      }

      implicit class Iterate2Ops[F[_, _]: Iterate2, K: GetSizeScore, V: GetSizeScore](inner: F[K, V]) {
        def iterator1: Iterator[K] = Iterate2[F].iterator1(inner)
        def iterator2: Iterator[V] = Iterate2[F].iterator2(inner)
      }
    }

    /**
      * Mutable key-value cache which limits the size score of the data scored.
      *
      * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
      * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
      * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
      * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
      *
      * @param maxSizeScore max size score for the stored data
      * @tparam K key type
      * @tparam V value type
      */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](
        maxSizeScore: SizeScore
    ) {
      //with this you can use .sizeScore syntax on keys and values
      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]
      private var totalScore: SizeScore = 0

      def put(key: K, value: V): Unit = {
        totalScore += key.sizeScore + value.sizeScore
        while (maxSizeScore < totalScore) {
          map.headOption match {
            case Some((k, v)) => {
              map -= k
              totalScore -= k.sizeScore + v.sizeScore
            }
            case _ => break
          }
        }

        map(key) = value
      }

      def get(key: K): Option[V] = {
        Option.when(map.contains(key))(map(key))
      }
    }

    /**
      * Cool custom immutable multi-map collection - does not extend the standard library collection types
      * (yes, this is a feature)
      */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] =
        PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
      * Type-class allowing us to iterate over different "collection-like" types with one type arg
      */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    /**
      * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
      */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {
      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] =
        new Iterate[Iterable] {
          override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
        }

      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      implicit val mapIterate2: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] =
          f.keys.iterator
        override def iterator2[T, S](f: Map[T, S]): Iterator[S] =
          f.values.iterator
      }

      implicit val multiMapIterate2: Iterate2[PackedMultiMap] =
        new Iterate2[PackedMultiMap] {
          override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] =
            f.inner.map({ case (k, _) => k }).iterator
          override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] =
            f.inner.map({ case (_, v) => v }).iterator
        }

      implicit def byteSizeScore: GetSizeScore[Byte] = _ => 1
      implicit def charSizeScore: GetSizeScore[Char] = _ => 2
      implicit def intSizeScore: GetSizeScore[Int] = _ => 4
      implicit def longSizeScore: GetSizeScore[Long] = _ => 8
      implicit def strSizeScore: GetSizeScore[String] = s => 12 + s.length * 2

      implicit def iterateSizeScore[F[_]: Iterate, T: GetSizeScore]: GetSizeScore[F[T]] =
          12 + _.iterator.map(_.sizeScore).sum

      implicit def iterate2SizeScore[F[_, _]: Iterate2, K: GetSizeScore, V: GetSizeScore]: GetSizeScore[F[K, V]] = x =>
        12 + x.iterator1.map(_.sizeScore).sum + x.iterator2.map(_.sizeScore).sum
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {

    import SuperVipCollections4s._
    import instances._
    import syntax._

    final case class Twit(
        id: Long,
        userId: Int,
        hashTags: Vector[String],
        attributes: PackedMultiMap[String, String],
        fbiNotes: List[FbiNote]
    )

    final case class FbiNote(
        month: String,
        favouriteChar: Char,
        watchedPewDiePieTimes: Long
    )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    implicit def fbiSizeScore: GetSizeScore[FbiNote] =
      n => {
        12 + n.month.sizeScore + n.favouriteChar.sizeScore + n.watchedPewDiePieTimes.sizeScore
      }

    implicit def twitSizeScore: GetSizeScore[Twit] =
      t => {
        12 +
          t.id.sizeScore +
          t.userId.sizeScore +
          t.hashTags.sizeScore +
          t.attributes.sizeScore +
          t.fbiNotes.sizeScore
      }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache =
      new TwitCache {
        private val _cache = new MutableBoundedCache[Long, Twit](maxSizeScore)

        override def put(twit: Twit): Unit = _cache.put(twit.id, twit)
        override def get(id: Long): Option[Twit] = _cache.get(id)
      }
  }
}

/**************Exercises************/
object HktExercises {

  // Exercise 12. Implement Functor for `Disjunction`
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  sealed trait Disjunction[+L, +R]
  object Disjunction {
    case class Left[L](left: L) extends Disjunction[L, Nothing]
    case class Right[R](value: R) extends Disjunction[Nothing, R]
  }

  implicit def disjunctionFunctor[T]: Functor[Disjunction[T, *]] = new Functor[Disjunction[T, *]] {
    override def fmap[A, B](fa: Disjunction[T, A])(f: A => B): Disjunction[T, B] = {
      fa match {
        case Disjunction.Right(r)  => Disjunction.Right(f(r))
        case l@Disjunction.Left(_) => l
      }
    }
  }
}
