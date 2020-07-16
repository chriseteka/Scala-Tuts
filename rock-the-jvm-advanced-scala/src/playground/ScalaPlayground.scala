package playground

object ScalaPlayground extends App{

  println("Hello Scala")

  def staircase(n: Int) {

      (1 to n).foreach(num => {
        (1 to num).foreach(li => print(" "*(num - li)+"#"))
        println()
      })
  }

  staircase(4)
}
