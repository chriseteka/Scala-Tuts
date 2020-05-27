package lectures

class Companion(val name: String, age: Int, nickName: String) {
  override def toString: String = s"Companion[name=$name, age=$age, nickName=$nickName]"
}

object Companion{

  def apply(name: String, age: Int, nickName: String): Companion =
    new Companion(name, age, nickName)
}
