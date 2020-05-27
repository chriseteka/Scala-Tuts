// Making class immutable by using class parameters
//Also known as constructor fields
class Employee(val first: String, val last: String){
  override def toString = first + " " + last
}

val bobMartin = new Employee("bob", "martin")
bobMartin

//When we try to access members of the class,
//the compiler complains that there is nothing
//of such "first" or "last", we need to introduce
//the val keyword to make it accessible but not
//modifiable.
bobMartin.first
bobMartin.last

//We can therefore say that the value in the class
//below is a public var member
class Shape(var name: String)
//This ensures that the name of the shape even after
//instantiation can be changed
val triangle = new Shape("Triangle") //Instantiation
triangle.name // accessed the name field
triangle.name = "Square" // modified the name field
triangle.name // accessed the name field again

//Below is a public val member of a class
class Color(val colName: String)
//After instantiation, the field 'colName' can only be
//accessed but not modifiable.
val red = new Color("Red") //Instantiation
red.colName // Access the colName field
//red.colName = "Blue" // Cannot modify the colName field