object Solutions {

  /**
   * QUESTION 1 ENTRY POINT
   */
  def dayTransactions(transactions: List[Transaction]): Map[Int, Double] =
    transactions.foldLeft(Map())({
      case (map, Transaction(_, _, day, _, amount)) =>
        map.updatedWith(day)({
          case Some(runningAmount) => Some(runningAmount + amount)
          case None                => Some(amount)
        })
    })
}
