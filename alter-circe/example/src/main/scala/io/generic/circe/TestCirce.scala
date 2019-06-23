package io.generic.circe

import io.generic.Alter

case class Level(advance: String,duration: Int,shockIntensive: Boolean,sets: Int)

case class Routine(name: String,creator: String,exercises: Int,intense: Boolean, level: Level)

case class Diet(source: String,calories: Int,gain: Boolean, routine: Routine)

case class Human(name: String,age: Int, weight: Double, height: Double,diet: Diet,training: Training)

sealed trait Training
case class Gym(name: String, isCrowded: Boolean, minMembership: Int, miles: Int) extends Training
case class StreetFitness(park: String, equipment: Int, peakHour: Int) extends Training


object TestCirce{


val alterHuman = Alter[Human].gen
  .rename("height","feets")
  .rename("weight","lbs")
  .rename("training","training_type") //can change the name of adt though
  .rename("gain","gainOriented")
  .rename("creator","trainer_creator")
  .rename("duration","minutes")

  val humanDecoder = alterHuman.decoder

  val humanEncoder = alterHuman.encoder

  val human =
    Human("leonardo",32,176.36, 5.6,
      Diet("meat",2500,true,
        Routine("blackout","Tim",24,true,
          Level("expert",45,true,3))),
      StreetFitness("south park",12,1900))

   
   val ejson = humanEncoder(human)

   val djson = humanDecoder.decodeJson(ejson)

  val check = require(human == djson.right.get)
}
