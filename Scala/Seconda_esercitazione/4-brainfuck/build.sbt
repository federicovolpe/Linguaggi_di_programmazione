ThisBuild / scalaVersion := "2.13.10"                                                      
                                                                                           
lazy val brainfuck = (project in file("."))                                                
  .settings(                                                                               
    scalacOptions += "-deprecation",                                                       
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2", 
)                                                                                          