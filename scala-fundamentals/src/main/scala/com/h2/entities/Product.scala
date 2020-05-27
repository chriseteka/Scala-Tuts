package main.scala.com.h2.entities

abstract class Product {
  val name: String

  override def toString: String = "Product=" + name
}

abstract class Deposits extends Product {
  val interestRatePerYear: Double
  val minimumBalancePerMonth: Int
}

abstract class Checkings extends Deposits

abstract class Savings extends Deposits {
  val transactionsAllowedPerMonth: Int
}

class CoreChecking(bal: Int, rate: Double) extends Checkings {
  println("Created core checking product")
  override val interestRatePerYear: Double = rate
  override val minimumBalancePerMonth: Int = bal
  override val name: String = "Core Checking"
}

class StudentChecking(bal: Int, rate: Double) extends Checkings {
  println("Created student checking product")
  override val interestRatePerYear: Double = rate
  override val minimumBalancePerMonth: Int = bal
  override val name: String = "Student Checking"
}

class RewardsSavings(bal: Int, rate: Double, trans: Int) extends Savings {
  override val transactionsAllowedPerMonth: Int = trans
  override val interestRatePerYear: Double = rate
  override val minimumBalancePerMonth: Int = bal
  override val name: String = "Rewards Savings"
}

abstract class Lending extends Product {
  val annualFee: Double
  val apr: Double
  val rewardsPercent: Double
}

class CreditCard(fee: Double, rate: Double, pct: Double) extends Lending {
  override val annualFee: Double = fee
  override val apr: Double = rate
  override val rewardsPercent: Double = pct
  override val name: String = "Credit Card"
}
