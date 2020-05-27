class Employee{
  var first:String = ""
  var last:String = ""
}

val amyJones = new Employee
amyJones.first = "Amy"
amyJones.last = "Jones"

val bobMartin = new Employee
bobMartin.first = "Bob"
bobMartin.last = "Martin"

// The val keywords are like finals and
// cannot be reassigned, helping us achieve
//immutability in scala