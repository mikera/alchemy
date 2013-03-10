alchemy
=======

A 7 day roguelike in Clojure


## Build instructions

Alchemy requires Maven to build.

The following command should produce a working build:

    mvn assembly:single

This will create an executable jar file (in the /target directory) that can be run with a command like:

    java -jar alchemy-0.0.1-SNAPSHOT-jar-with-dependencies.jar
