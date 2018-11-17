object Monads {
  def main(args: Array[String]): Unit = {
    val result = for {
      x <- computeX()
      y <- computeY()
    } yield x + y

    println(result)
    assert(result == Try(579))
  }

  def computeX(): Try[Int] = Success(123)

  def computeY(): Try[Int] = Success(456)

  /*
  Is try a monad?

  where unit(x) == Try(x)

  Does it satisfy the associativity law?
  m flatMap f flatMap g == m flatMap (x => f(x) flatMap g)

  m flatMap f flatMap g
  = m match {
    case Success(x) => {
      val u = (try f(x) catch { case ex: Exception => Failure(ex) })
      u flatMap g
    }
    case fail: Failure => fail
  }

  Does it satisfy the left unit law?
  unit(x) flatMap f == f(x)
  Try(x) flatMap f != f(x)

  An exception might be thrown from f or x but Try(x) flatMap f will not throw an exception.
  Therefore Try is not a monad. But an expression composed from Try, flatMap, or Map will never
  throw an exception.

  Does it satisfy the right unit law?
  m flatMap unit == m


   */
}

abstract class Try[+T] {
  def flatMap[U](f: T => Try[U]): Try[U] = {
    this match {
      case Success(x) => try f(x) catch { case ex: Exception => Failure(ex) }
      case fail: Failure => fail
    }
  }

  def map[U](f: T => U): Try[U] = {
    this match {
      case Success(x) => Try(f(x))
      case fail: Failure => fail
    }
  }

  def get: T = {
    this match {
      case Success(x) => x
      case Failure(ex) => throw ex
    }
  }
}

case class Success[T](s: T) extends Try[T]
case class Failure(ex: Exception) extends Try[Nothing]

object Try {
  def apply[V](f: => V): Try[V] = {
    try Success(f)
    catch {
      case ex: Exception => Failure(ex)
    }
  }
}
