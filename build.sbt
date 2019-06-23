
val scala =  "2.12.8"
scalaVersion := scala
 
val shapelessVersion = "2.3.3"
val circeVersion = "0.9.3"

lazy val alter = (project in file("alter/core"))
  .settings(
    libraryDependencies += "com.chuusai" %% "shapeless" % shapelessVersion,
    scalaVersion := scala
  )

lazy val alterExample = (project in file("alter/example"))
  .settings(
    libraryDependencies += "com.chuusai" %% "shapeless" % shapelessVersion,
    scalaVersion := scala
  )
  .dependsOn(alter)
  
lazy val circe = (project in file("alter-circe/circe"))
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % shapelessVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-shapes" % circeVersion
    ),
    scalaVersion := scala
  )
  .dependsOn(alter)  

lazy val circeExample = (project in file("alter-circe/example"))
  .settings(
     libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % shapelessVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-shapes" % circeVersion
    ),
    scalaVersion := scala
  )
  .dependsOn(alter,circe)  