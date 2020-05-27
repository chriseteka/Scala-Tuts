package main.scala

import java.time.LocalDate

import main.scala.com.h2.entities.{Account, Bank, CoreChecking, CreditCard, Customer, RewardsSavings, StudentChecking}

object BankOfScala {
  def main(args: Array[String]): Unit = {

    println("Instantiating Bank")

    val coreChecking = new CoreChecking(100, 0.025)
    val studentChecking = new StudentChecking(0, 0.010)
    val rewardsSaving = new RewardsSavings(10000, 0.10, 1)
    val creditCard = new CreditCard(99.00, 14.23, 20.00)
    val products = Set(coreChecking, studentChecking, rewardsSaving, creditCard)

    val bobMartin = new Customer("Bob", "Martin", "bob@martin.com",
      LocalDate.of(1983, 8, 22))
    val bobCheckingAccount = new Account(bobMartin, coreChecking, 1000)
    val bobSavingAccount = new Account(bobMartin, rewardsSaving, 20000)
    val bobCreditAccount = new Account(bobMartin, creditCard, 4500)
    val accounts = Set(bobCheckingAccount, bobSavingAccount, bobCreditAccount)

    val bank = new Bank("Bank of Scala", "Auckland", "New Zealand",
      "bank@scala.com", products, Set(bobMartin), accounts)

    println(bobCheckingAccount)

    bobCheckingAccount.deposit(100)
    println(bobCheckingAccount)

    bobCheckingAccount.withdraw(200)
    println(bobCheckingAccount)

    println(bank)

    println(bank.accounts)
  }
}
