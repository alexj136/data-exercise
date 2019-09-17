object Solutions {

  /**
   * QUESTION 1
   * The function transactionDaySums solves question 1 by folding the input list
   * of transactions into a Map[Int, Double] - the Int key is the date and the
   * Double value is the sum of transactions for that day.
   *     The fold function takes a transaction, extracting only the date and
   * amount (the rest is irrelevant to the question), and if the date already
   * exists as a key in the map, adds the current amount to the amount already
   * contained in the map. If the date isn't a key in the map, just add the
   * mapping (day -> amount) to the map.
   *     The complexity will by O(n log n) where n is the length of the input
   * list. This is because we pass over the input list once (hence n *
   * something), and for each list element we perform a map lookup and
   * insertion, each of which is log n (assuming a reasonable implementation
   * behind Scala's Map class, based on e.g. red-black trees). So it's doing 2
   * log n work n times for O(n log n).
   */
  def transactionDaySums(transactions: List[Transaction]): Map[Int, Double] =
    transactions.foldLeft(Map[Int, Double]())({
      case (map, Transaction(_, _, day, _, amount)) => map.updatedWith(day)({
        case Some(runningAmount) => Some(runningAmount + amount)
        case None                => Some(amount)
      })
    })
}
