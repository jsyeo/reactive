import scala.util.Random

trait Generator[+T] {
  self =>

  def generate(): T

  def map[S](f: T => S): Generator[S] = () => f(self.generate())

  def flatMap[S](f: T => Generator[S]): Generator[S] = () => f(self.generate()).generate()
}

trait Tree

case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

object Main {
  def main(args: Array[String]) = {
    val integers: Generator[Int] = new Generator[Int] {
      val rand = new Random()
      override def generate(): Int = rand.nextInt()
    }

    val booleans = for {
      x <- integers
    } yield x > 0

    val pairs = for {
      x <- integers
      y <- integers
    } yield (x, y)

    def single[T](x: T): Generator[T] = () => x

    val emptyList = single(Nil)

    def nonEmptyList = for {
      head <- integers
      tail <- lists
    } yield head :: tail

    def lists: Generator[List[Int]] = for {
      isEmpty <- booleans
      l <- if (isEmpty) emptyList else nonEmptyList
    } yield l

    val leaf = for (x <- integers) yield Leaf(x)

    def innerTree = for {
      l <- trees
      r <- trees
    } yield Inner(l, r)

    def trees: Generator[Tree] = for {
      isLeaf <- booleans
      t <- if (isLeaf) leaf else innerTree
    } yield t

    println(trees.generate())
  }
}
