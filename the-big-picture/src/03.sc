class Employee{
  var first:String = ""
  var last:String = ""

  override def toString = first + " " + last
}

val amyJones = new Employee
amyJones.first = "Amy"
amyJones.last = "Jones"

val bobMartin = new Employee
bobMartin.first = "Bob"
bobMartin.last = "Martin"

bobMartin

bobMartin.first = "Marina"
bobMartin.last = "frida"

bobMartin

//Notice that although bobMartin is a val that cannot
//point to another instance of Employee, Employee on its
//own has variables that are changing, and hence can be
//reassigned/changed if need be
//We can however make a class fields immutable