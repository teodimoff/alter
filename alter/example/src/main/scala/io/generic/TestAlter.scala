package io.generic

case class Nested3(xname3: String,xage3: Int,xgo3: Boolean)

case class Nested2(xname2: String,xage2: Int,xgo2: Boolean, nes3: Nested3)

case class Nested1(name1: String,age1: Int,go1: Boolean, nes2: Nested2)

case class TestNestedCase(name: String,age: Int,go: Boolean,txx: Nested1,x: Int)

case class TestRename(name: String,age: Int, activity: Boolean)


object TestAlter{

  val simple = Alter[TestRename].gen.rename("name","othername").rename("activity","running")

  val nested = io.generic.Alter[TestNestedCase].gen
    .rename("name1","namex1")
    .rename("x","xx")
    .rename("age","ages")
    .rename("xage2","wrt22")
    .rename("xage3","wewe333")


  val nestedHList = nested.to(
    TestNestedCase("weq",21321,false,
      Nested1("aw",1123,true,
	Nested2("awer",23,false,
	  Nested3("arqwq",323,true))),123))

}