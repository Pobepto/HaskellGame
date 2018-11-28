<p align="center">
<img src="https://user-images.githubusercontent.com/16295803/49148113-879fe300-f317-11e8-955c-b22e80633b08.png"/>
</p>
<p align="center">
<img src="https://user-images.githubusercontent.com/16295803/49148140-95556880-f317-11e8-869b-9adfc559977d.png"/>
</p>

# Programming in Haskell Course project

## Description
Repository contains basic implementation of Doodle Jump game for the "Programming in Haskell" course.

<p align="center">
<img src="https://media.giphy.com/media/7Jw6V8ZpDy9lt9JUqr/giphy.gif"/>
</p>

## Team
Robert Sayakhov \
Timur Khazhiev

## How to run
As a prerequisite you need [Haskell Stack](https://docs.haskellstack.org/en/stable/README/ "Stack's doc") to be installed. \
Then you can simply run it with 
``` bash
stack build && stack exec game-exe
``` 
Or through repl
``` bash
>: main
``` 

## Works done
- [x] Implemented movement of player
- [x] Implemented gravity
- [x] Added start and defeat screens
- [x] Added different platforms types
- [x] Added score points to game
- [x] Added platform patterns
- [x] Added level generation
- [x] Added several monsters 