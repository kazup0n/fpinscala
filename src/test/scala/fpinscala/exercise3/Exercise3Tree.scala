package fpinscala.exercise3

import org.scalatest.FunSuite

class Exercise3Tree extends FunSuite {

  test("exercise 3.25 - size") {
    assert(Tree.size(Leaf(1)) == 1)
    assert(Tree.size(Branch(Leaf(1), Leaf(1))) == 3)
    assert(Tree.size(
      Branch(
        Branch(
          Leaf(1),
          Leaf(1)),
        Branch(
          Leaf(1),
          Leaf(1)
        )
      ))
      == 7)
  }

  test("execise 3.26 - max") {
    assert(Tree.max(
      Branch(
        Branch(
          Leaf(1),
          Leaf(2)),
        Branch(
          Leaf(3),
          Leaf(4)
        )
      ))
      == 4)
  }

  test("execise 3.27 - depth") {
    assert(Tree.depth(
      Branch(
        Branch(
          Leaf(1),
          Leaf(2)),
        Branch(
          Leaf(3),
          Leaf(4)
        )
      ), Leaf(4))
      == 3)

    assert(Tree.depth(
      Branch(
        Branch(
          Leaf(1),
          Leaf(2)
        ),
        Branch(
          Leaf(4),
          Branch(
            Leaf(1),
            Leaf(4)
          )
        )
      ), Leaf(4))
      == 4)


    assert(Tree.depth(
      Branch(
        Branch(
          Leaf(1),
          Leaf(2)
        ),
        Branch(
          Leaf(4),
          Branch(
            Leaf(1),
            Leaf(4)
          )
        )
      ), Leaf(5))
      == 0)

    assert(Tree.depth(Leaf(1), Leaf(1)) == 1)
  }

  test("exercise 3.28 - map") {
    assert(Tree.map(Branch(Leaf(1), Leaf(2)))(_ + 1) == Branch(Leaf(2), Leaf(3)))
    assert(Tree.map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ + 1) == Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
  }

  test("exercise 3.29 - fold"){
    val t:Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    //max
    assert(Tree.fold(t)(v=>v)(_ max _) == 3)

    //size
    assert(
      Tree.fold(t)(_=>1)((l:Int, r:Int)=> l+r+1)
      ==
      5
    )

    //depth
    def depth[A](_t: Tree[A], s:Leaf[A]): Int =
      Tree.fold(_t)(v => if(s.value==v) 1 else 0)((l, r)=> {
        val d = l max r
        if(d > 0) d+1 else 0
      })

    assert(depth(Leaf(1), Leaf(1)) == 1)
    assert(depth(t, Leaf(3)) == 3)


    //map
    def map[A, B](_t:Tree[A])(f: A=>B): Tree[B] = Tree.fold(_t)(v=>Leaf(f(v)):Tree[B])((l, r)=> Branch(l, r))
    assert(map(t)(_+1) == Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))

  }





}
