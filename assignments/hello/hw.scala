trait Generator[+T] {
  self =>

  def generate: T
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }
}

trait M[T] {
  def flatMap[T] (f: T => M[U]): M[U]
}

def unit[T](x: T): M[T]

m map f == m flatMap (x => unit(f(x)))
        == m flatMap (f andThen unit)

object Hi {
  def main(args: Array[String]) {

    def integers = new Generator[Int] {
      def generate = scala.util.Random.nextInt()
    }

    def booleans = integers.map(_ >= 0)

    trait Tree
    case class Inner(left: Tree, right: Tree) extends Tree
    case class Leaf(x: Int) extends Tree


    def trees: Generator[Tree] = for {
      isLeaf <- booleans
      tree <- if(isLeaf) leafs else inners
    } yield tree

    def leafs  = integers.map(Leaf)
    def inners = for {
      x <- trees
      y <- trees
    } yield Inner(x, y)
    println(trees.generate)
  }
}
