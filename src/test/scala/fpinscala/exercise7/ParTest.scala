package fpinscala.exercise7

import java.util.concurrent.Executors

import fpinscala.exercise7.nb.NB
import org.scalatest.{FunSuite}

class ParTest extends FunSuite {


  test("デッドロックしちゃうやつ"){
//    val a = Par.lazyUnit(42+1)
//    val S = Executors.newFixedThreadPool(1)
//
//    println(Par.equal(S)(a, Par.fork(a)))
  }

  test("NonBlockingな実装"){
    val p = NB.parMap(List.range(1, 100000))(math.sqrt(_))
    val x= NB.run(Executors.newFixedThreadPool(2))(p)
    println(x)
  }

  test("chooser"){
    val p = NB.choice(NB.unit(true))(NB.unit(1), NB.unit(0))

    val a = NB.run(Executors.newFixedThreadPool(1))(p)

    assert(a == 1)
  }

  test("join") {
    val p = NB.unit(NB.unit(1))
    val a = NB.run(Executors.newFixedThreadPool(1))(NB.join(p))
    assert(a == 1)
  }


  test("joinFlatMap") {
    val p = NB.unit(NB.unit(1))
    val a = NB.run(Executors.newFixedThreadPool(1))(NB.joinFlatMap(p))
    assert(a == 1)
  }



}
