package fpinscala.exercise6

sealed trait Input

case object Coin extends Input

case object Turn extends Input


case class Machine(locked: Boolean, candies: Int, coins: Int) {

//  def update(input: Input): State[Machine, (Int, Int)] =
//    State((machine:Machine) => {
//      (machine, (0,0))
//    })

  /**
    * ロックされた状態の自販機に硬貨を投入すると、スナックが残っている場合はロックが解除される
    * ロックが解除された時状態の自動販売機のハンドルを回すと、スナックが出てきてロックがかかる
    * ロックされた状態でハンドルを回したり、ロックが解除された状態で硬貨を投入したりしてもなにも起こらない
    * スナックが売り切れた自動販売機は入力を全て無視する
    *
    * @param inputs
    * @return 最後に自動販売機の中にある硬貨とスナックの数; (硬貨, スナック)
    */
  //def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)]


}
