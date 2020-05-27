class Employee(f: String, l: String, s: Int){
  private val first = f
  private val last = l
  private var stocks = s

  def getFirst = first
  def getLast = last
  def getStocks = stocks

  // Known as procedure or void (causes side effect in FPL)
  //Only used when necessary
  def awardMoreStock(num: Int) = stocks += num

  override def toString = first + " " + last + " " + stocks
}

val bobMartin = new Employee("Bob", "Martin", 10)
bobMartin.getFirst
bobMartin.getLast
bobMartin.getStocks
bobMartin.awardMoreStock(19)
bobMartin