alchemy
=======

A 7 day Roguelike in Clojure

by Mike Anderson

## Story

    You are a talented young alchemist. You discover that the fabled Philosopher's 
    Stone is hidden at the bottom of an old dungeon. What luck! All you have to do 
    is descend and grab the stone before your rivals catch the scent. 
    
    What could possibly go wrong?
    
## Download

If you just want to play the game, you can get the compiled .jar file here:

 - https://drive.google.com/file/d/1rWkCiant3PEVkitP4AmNUph9JsBvAtg5  (latest version with fixes)
 - https://drive.google.com/file/d/1TbcY_Msby2NYW44aqB6zFvtgIaQ7NCku  (original 7DRL version)
    
You should be able to run the file either by double clicking it or using the command `java -jar alchemy.jar`. You will need to have Java 1.6 or above installed.

## More infomation

Blog posts about the development / design of alchmey can be found on the Creative Clojure blog:

 - http://clojurefun.wordpress.com/2013/03/


## Build instructions

Alchemy requires Maven to build (though you can probably get a simple project.clj to make it work with leiningen pretty easily)

The following command should produce a working build:

    mvn install assembly:single

This will create an executable jar file (in the /target directory) that can be run with a command like:

    java -jar alchemy-0.0.1-SNAPSHOT-jar-with-dependencies.jar
