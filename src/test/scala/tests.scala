import org.scalacheck.{Properties, Gen}
import org.scalacheck.Prop.forAll
import org.scalatest.FunSuite

object ExerciseProperties extends Properties("Exercise") {

  /**
   * Randomly generate a transaction
   */
  val genTransaction: Gen[Transaction] = for {
    transactionId <- for { num <- Gen.choose[Int](1, 991) } yield "T000" + num
    account <- for { num <- Gen.choose[Int](1, 49) } yield "A" + num
    day <- Gen.choose[Int](1, 29)
    category <- Gen.oneOf(List("AA", "BB", "CC", "DD", "EE", "FF", "GG"))
    amount <- for { num <- Gen.choose[Double](0.01, 999.99) } yield
      num - num % 0.01 // round the amount to two decimal places
  } yield Transaction(transactionId, account, day, category, amount)

  /**
   * Randomly generate 0 to 1000 transactions
   */
  val gen0To1000Transactions: Gen[List[Transaction]] =
    Gen.containerOfN[List, Transaction](1000, genTransaction)

  /**
   * Randomly generate one or more transactions
   */
  val genSomeTransactionsNonEmpty: Gen[List[Transaction]] =
    Gen.nonEmptyContainerOf[List, Transaction](genTransaction)

  /**
   * Test that the task 1 solution yields as many rows as there are unique dates
   * in the input dataset. This ensures that no days are left out.
   */
  property("transactionDaySums length equals number of unique days") =
    forAll(gen0To1000Transactions) { transactions =>
      Solutions.transactionDaySums(transactions).size == (transactions map
        { case Transaction(_, _, day, _, _) => day }).toSet.size
    }

  /**
   * Test that the task 1 solution yields, for a specific day, an amount equal
   * to filtering the input dataset by that day, and then summing the amounts.
   * This ensures that days are summed correctly.
   */
  property("""transactionDaySums for a given day equals first filtering by that
      day and then summing""") =
    forAll(genSomeTransactionsNonEmpty) { transactions =>
      val dayOfFirstTransaction: Int = transactions.head.transactionDay
      Solutions.transactionDaySums(transactions)(dayOfFirstTransaction) ==
        ((transactions filter {
          case Transaction(_, _, day, _, _) => day == dayOfFirstTransaction
        }) map (_.transactionAmount)).sum
    }

  /**
   * Test that the task 2 solution yields as many rows as there are unique
   * accounts in the input dataset. This ensures that no accounts are left out.
   */
  property("averagePerAccountAndType length equals number of unique accounts") =
    forAll(gen0To1000Transactions) { transactions =>
      Solutions.averagePerAccountAndType(transactions).size == (transactions map
        { case Transaction(_, account, _, _, _) => account }).toSet.size
    }

  /**
   * Test that the task 2 solution yields, for a specific account, amounts equal
   * to filtering the input dataset by that account, and then summing the
   * amounts for each transaction category. This ensures that account categories
   * are summed correctly.
   */
  property("""averagePerAccountAndType for a given account equals first
      filtering by that account and then summing for all categories""") =
    forAll(genSomeTransactionsNonEmpty) { transactions =>
      val accountOfFirstTransaction: String = transactions.head.accountId
      Solutions.averagePerAccountAndType(transactions)(
        accountOfFirstTransaction) ==
          Solutions.mapSeven[(Int, Double), Double]({ case (count, amount) =>
            if(count == 0) 0.0 else amount / count },
            (transactions filter { case Transaction(_, account, _, _, _) =>
                account == accountOfFirstTransaction
            }).foldLeft(Solutions.sevenOf((0, 0.0)))({
              case (categoryAmounts, Transaction(_, _, _, category, amount)) =>
                Solutions.addToCategory(category, amount, categoryAmounts)
            })
          )
    }
}

class ExerciseSuite extends FunSuite {

  test("q3Statistics empty list test case") {
    assert(Solutions.q3Statistics(List()) == Map())
  }

  test("q3Statistics singleton list test case") {
    val testDay: Int = 8
    val results: Map[(Int, String), List[Double]] =
      Solutions.q3Statistics(List(Transaction("T0005", "A1", 7, "FF", 3.58)))
    assert(results((testDay, "A1"))(Solutions.maximum) == 3.58)
  }

  test("q3Statistics short example") {
    val transactions: List[Transaction] = List(
      Transaction("T0001", "A1", 3, "AA", 16.0),
      Transaction("T0002", "A1", 4, "AA", 4.06),
      Transaction("T0003", "A1", 5, "CC", 1.34),
      Transaction("T0004", "A1", 6, "CC", 1.32),
      Transaction("T0005", "A1", 7, "FF", 3.58),
      Transaction("T0006", "A1", 8, "FF", 1.12)
    )
    val testDay: Int = 8
    val results: Map[(Int, String), List[Double]] =
      Solutions.q3Statistics(transactions)
    assert(results((testDay, "A1"))(Solutions.maximum) == 16.0)
  }
}
