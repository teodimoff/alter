package io.generic.circe

import io.generic.Alter 


case class TestRename(name: String,age: Int, activity: Boolean)


object SimpleTest{

val alterTest = Alter[TestRename].gen.rename("name","othername").rename("activity","running")
  
  val tdecoder = alterTest.decoder
  val tencoder = alterTest.encoder

}