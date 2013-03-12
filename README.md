alchemy
=======

A 7 day Roguelike in Clojure

by Mike Anderson

## Story

    You are a talented young alchemist. You discover that the fabled Philosopher's 
    Stone is hidden at the bottom of an old dungeon. What luck! All you have to do 
    is descend and grab the stone before your rivals catch the scent. 
    
    What could possibly go wrong?

## Build instructions

Alchemy requires Maven to build (though you can probably get a simple project.clj to make it work with leiningen pretty easily)

The following command should produce a working build:

    mvn assembly:single

This will create an executable jar file (in the /target directory) that can be run with a command like:

    java -jar alchemy-0.0.1-SNAPSHOT-jar-with-dependencies.jar
