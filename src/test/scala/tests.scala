import org.scalacheck.{Properties, Gen}
import org.scalacheck.Prop.forAll

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
}
