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

  /**
   * QUESTION 2
   * The function averagePerAccountAndType solves question 2 in a similar manner
   * to the solution to question 1, and indeed is in the same complexity class
   * (O(n log n)). The difference is that instead of keeping a single running
   * total as we fold over the input, we keep 14 running totals - 7 pairs of
   * (Int, Double) - for each transaction type (category) we keep a total of how
   * many of that type we've seen (the Int) and the running total (the Double).
   * Once all this is computed, we traverse the resulting Map and combine every
   * pair (amountSeen: Int, totalValue: Double) into an average by dividing the
   * latter by the former. Thus we do (n log n) + n work which entails the
   * complexity class O(n log n).
   *     The function totalAndCountPerAccountAndType does the folding described
   * above, and averagePerAccountAndType takes that result and combines to get
   * averages. We use a helper type to manage tuples of length 7 in which all
   * contained values are of the same type. The solution makes use of some
   * helper functions over this type, Seven[T].
   *     We could use List[T] instead of Seven[T], and indeed the code would be
   * a fair bit neater, but we would lose the guarantee that the compiler gives
   * us, that all the lists have length 7. If we used a List[T] instead, and
   * somewhere in our code omitted one of the seven elements, the compiler
   * wouldn't warn us. Using Seven[T] ensures that the compiler checks and warns
   * us if we forget an element (or add an extra one).
   */

  def averagePerAccountAndType(transactions: List[Transaction]):
    Map[String, Seven[Double]] = totalAndCountPerAccountAndType(transactions)
      .map({ case (account, totalAndCountPerType) =>
        (account, mapSeven[(Int, Double), Double]({ case (count, total) =>
          if (count == 0) 0.0 else total / count }, totalAndCountPerType)) })

  def totalAndCountPerAccountAndType(transactions: List[Transaction]):
    Map[String, Seven[(Int, Double)]] =
      transactions.foldLeft(Map[String, Seven[(Int, Double)]]())({
        case (map, Transaction(_, account, _, category, amount)) =>
          map.updatedWith(account)({
            case Some(categoryAmounts) =>
              Some(addToCategory(category, amount, categoryAmounts))
            case None => Some(addToCategory(category, amount, zeroed7IntDouble))
          })
      })

  // Add an amount to a 7x2 tuple containing counts and running totals for all
  // transaction categories. Simply add the supplied amount to the running total
  // and increment the count by 1.
  def addToCategory(category: String, amount: Double, categoryAmounts:
    Seven[(Int, Double)]): Seven[(Int, Double)] =
      applyToSevenByCategory(category, categoryAmounts, {
        case (count, runningTotal) => (count + 1, amount + runningTotal)
      })

  type Seven[T] = (T, T, T, T, T, T, T)

  // Generate a Seven[T] of 7 copies of a given element x
  def sevenOf[T](x: T): Seven[T] = (x, x, x, x, x, x, x)

  // Map a function T => U over all elements of a Seven[T] yielding a Seven[U]
  def mapSeven[T, U](f: T => U, xs: Seven[T]): Seven[U] = xs match {
    case (x1, x2, x3, x4, x5, x6, x7) =>
      (f(x1), f(x2), f(x3), f(x4), f(x5), f(x6), f(x7))
  }

  // Initial running totals for the fold over the transaction list - 0
  // transactions seen for a total value of 0.0, for each category
  val zeroed7IntDouble: Seven[(Int, Double)] = sevenOf((0, 0.0))

  // Apply a function T => T to a single element of a Seven[T], selecting the
  // element to apply the function to using a category String like the ones in
  // the input data, of form "AA", "BB", ..., "GG".
  def applyToSevenByCategory[T](category: String, s: Seven[T], f: T => T):
    Seven[T] = category match {
        case "AA" => (f(s._1), s._2, s._3, s._4, s._5, s._6, s._7)
        case "BB" => (s._1, f(s._2), s._3, s._4, s._5, s._6, s._7)
        case "CC" => (s._1, s._2, f(s._3), s._4, s._5, s._6, s._7)
        case "DD" => (s._1, s._2, s._3, f(s._4), s._5, s._6, s._7)
        case "EE" => (s._1, s._2, s._3, s._4, f(s._5), s._6, s._7)
        case "FF" => (s._1, s._2, s._3, s._4, s._5, f(s._6), s._7)
        case "GG" => (s._1, s._2, s._3, s._4, s._5, s._6, f(s._7))
        case _    => throw new RuntimeException("invalid category")
    }

  /**
   * QUESTION 3
   */

  val maximum: Int = 0
  val average: Int = 1
  val aaTotal: Int = 2
  val ccTotal: Int = 3
  val ffTotal: Int = 4

  def q3Statistics(transactions: List[Transaction]):
    Map[(Int, String), List[Double]] = {

	  // Group all rows by date and account using groupByDayAndAccount
	  val dayAndAccountGrouping: Map[(Int, String), List[(String, Double)]] =
		groupByDayAndAccount(transactions)

	  // Get all unique pairs (day, account) - we will map over this to get out
	  // final results
	  val allDayAndAccountPairs: List[(Int, String)] =
		dayAndAccountGrouping.keys.toList

	  // Map over the (day, account) pairings - for each pairing...
	  allDayAndAccountPairs.map({
		case (day, account) => {

		  // Query the (day, account) grouping for all records on each relevant
		  // day. Each record is a pair (category, amount)
		  val relevantDaysData: List[(String, Double)] =
			(List(1, day - 6).max to List(1, day - 1).max).toList.map(dayN =>
			  dayAndAccountGrouping((dayN, account)))
				.foldLeft(List[(String, Double)]())(_ ++ _)

		  // Calculate the max by dropping the category info and using List.max
		  val max: Double = relevantDaysData.map(_._2).max

		  // Calculate the average by summing the amounts and dividing by the
		  // total number of transactions in the relevant period
		  val avg: Double =
			relevantDaysData.map(_._2).sum / relevantDaysData.length

		  // Calculate totals for the relevant categories by first filtering the
	      // relevant transactions by that category and then summing
		  val aaT: Double = relevantDaysData.filter(_._1 == "AA").map(_._2).sum
		  val ccT: Double = relevantDaysData.filter(_._1 == "CC").map(_._2).sum
		  val ffT: Double = relevantDaysData.filter(_._1 == "FF").map(_._2).sum

		  // Return the calculated data for conversion into a map
		  ((day, account), List(max, avg, aaT, ccT, ffT))
		}
	  }).toMap
	}

  def groupByDayAndAccount(transactions: List[Transaction]):
    Map[(Int, String), List[(String, Double)]] =
      transactions.foldLeft(Map[(Int, String), List[(String, Double)]]())({
        case (map, Transaction(_, account, day, category, amount)) =>
          map.updatedWith((day, account))({
            case Some(l) => Some((category, amount) :: l)
            case None    => Some(List((category, amount)))
          })
      })
}
