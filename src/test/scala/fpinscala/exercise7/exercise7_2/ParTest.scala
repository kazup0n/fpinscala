package fpinscala.exercise7.exercise7_2

import java.util.concurrent.Executors

import fpinscala.parallelism.Par
import org.scalatest.{FunSuite, Ignore}

class ParTest extends FunSuite {


  @Ignore
  test("デッドロックしちゃうやつ"){
    val a = Par.lazyUnit(42+1)
    val S = Executors.newFixedThreadPool(1)

    println(Par.equal(S)(a, Par.fork(a)))
  }



}
