package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def size[A](tree: Tree[A]): Int = tree match {

    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {

    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {

    case Leaf(_) => 0
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {

    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {

    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(a => 1)((l, r) => 1 + l + r)

  def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(a => a)((l, r) => l max r)

  def depthViaFold(tree: Tree[Int]): Int = fold(tree)(a => 0)((l, r) => 1 + (l max r))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {

    val tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    val tree2 = Branch(Branch(Leaf(2), Leaf(8)),
      Branch(Branch(Leaf(11),
        Branch(Leaf(12),
          Leaf(16))),
        Leaf(7)))

    println(size(tree))
    println(maximum(tree2))
    println(depth(tree2))
    println(map(tree2)(_.toDouble))

    println(sizeViaFold(tree))
    println(depthViaFold(tree2))
    println(maximumViaFold(tree2))
    println(mapViaFold(tree2)(_.toDouble))
  }
}