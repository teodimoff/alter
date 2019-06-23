# Alter: rename your case class fields at compile time.

# What
Alter will allow you to rename your case class fileds at compile time. 
The result is a HList, which has the renamed fields. Later this HList can be passed to the
standard derivation type class pipeline eg. to JSON derivation HTTP derivation etc. with respected fields renamed.
After you extract the relevant fields from your source (json,http params etc.) you will have a HList which cannot be converted back to original case class.
Alter has a method .from (equivalent to shapeless synthesized LabelledGenric[T].from method), which will 
rename all the fields back to their original names, and then convert the HList to original case class instance.

# Why 
Alter aims to provide simple way to rename fields without a lookup from intermediate structure like a Map[String,String].
Alter will attempt to provide more uniform way for renaming hlists and leave only
the derivation to the implementations.

# Dependencies
[shapeless](https://github.com/milessabin/shapeless)

# Usage:
We have hypotetical class Test which we want the field 'name' to be renamed to 'othername' and field 'activity' to 'running': 
```scala

case class Test(name: String,age: Int, activity: Boolean)
  

val alter = Alter[Test].gen
	    .rename("name","othername")
	    .rename("activity","running")
	    //.rename("atcivity","running") a typo - does not compile
//alter.Out will have HList with the field names changed.
  
//hypotetical derivation typeclass  
//val derive = Derive[T /*source*/,alter.Out]  
  
//val extracted: alter.Out = derive.apply(t: T)
  
//val instanceTest: Test = alter.from(extracted)

```
# Implementations
  * [circe](https://github.com/circe/circe)(depends on circe-shapes and circe-generic)
  * more to come
```scala
import io.generic.Alter
import io.generic.circe._
  
  
case class Level(advance: String,duration: Int,shockIntensive: Boolean,sets: Int)
  
case class Routine(name: String,creator: String,exercises: Int,intense: Boolean, level: Level)
  
case class Diet(source: String,calories: Int,gain: Boolean, routine: Routine)
  
case class Human(name: String,age: Int, weight: Double, height: Double,diet: Diet,training: Training)
  
sealed trait Training
case class Gym(name: String, isCrowded: Boolean, minMembership: Int, miles: Int) extends Training
case class StreetFitness(park: String, equipment: Int, peakHour: Int) extends Training


val alterHuman = Alter[Human].gen
  .rename("height","feets")
  .rename("weight","lbs")
  .rename("training","training_type") //can change the name of adt though
  .rename("gain","gainOriented")
  .rename("creator","trainer_creator")
  .rename("duration","minutes")
  
  
val decoder: io.circe.Decoder[Human] = alterHuman.decoder
val encoder: io.circe.Encoder[Human] = alterHuman.encoder

  
  val human =
    Human("leonardo",32,176.36, 5.6,
      Diet("meat",2500,true,
        Routine("blackout","Tim",24,true,
          Level("expert",45,true,3))),
      StreetFitness("south park",12,1900))
      
val json = encoder(human) 
/*
val json: io.circe.Json =
    {
      "name" : "leonardo",
      "age" : 32,
      "lbs" : 176.36,
      "feets" : 5.6,
      "diet" : {
        "source" : "meat",
        "calories" : 2500,
        "gainOriented" : true,
        "routine" : {
          "name" : "blackout",
          "trainer_creator" : "Tim",
          "exercises" : 24,
          "intense" : true,
          "level" : {
            "advance" : "expert",
            "minutes" : 45,
            "shockIntensive" : true,
            "sets" : 3
          }
        }
      },
      "training_type" : {
        "StreetFitness" : {
          "park" : "south park",
          "equipment" : 12,
          "peakHour" : 1900
        }
      }
    }
*/
  
val fromJsonHuman = decoder.decodeJson(json) 
/*

io.circe.Decoder.Result[io.generic.circe.Human] =Right(
    Human(leonardo,32,176.36,5.6,
      Diet(meat,2500,true,Routine(blackout,24,true,
        Level(expert,45,true,3))),
    StreetFitness(south park,12,1900)))

 */

```

##### It is important to notice change order of nested case calsses or you might get implicit not found. In the Human example, the order of changes are from outer to inner:
##### Human(fields - height, weight, training) ->  Diet(field - gain) -> Routine(field - creator) -> Level(field - duration)

# Limitations
For the moment Alter supports only renaming HList fields, as this is the most common use case.
There is no direct support (read function to convert them all) for snake case or hyphens names (kebab names),
because that would require runtime knowledge of the strings.
This can be done explicitly:

```testHuman.rename("shockIntensive", "shock_intensive")```

or  

```testHuman.rename("shockIntensive", "shock-intensive")```

Future version might provide macro based automatic (camel case , kebab case) renaming.
