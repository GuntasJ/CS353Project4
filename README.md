# CS353Project4

## Running the Game

To run the game, download the rkt file and run it in Dr Racket. 

## Game Overview

### Setting

The setting of the game is that the player is trapped in a dungeon, and they are trying to escape.

### Locations

<img width="540" alt="Screenshot 2024-05-05 at 6 26 32 PM" src="https://github.com/GuntasJ/CS353Project4/assets/90429329/c393a9e3-4084-482d-a0ec-fbb2902e9c6f">

>A rudimentary diagram of the map is shown above


There are a total of 12 locations in the game. These locations are conected to one another, and they can be accessed by the four cardinal directions

### Inventory

The player inventory starts off as empty, however, as the game progresses, there will be items that they can collect and put in their own inventory. Additionally, they may also drop them on the ground.

### Goal State

The goal state is to reach the exit, and then to drop the three keys scattered throughout the map in order to escape. 

## Sources Used

I used the Racket docs extensively. 

>https://docs.racket-lang.org/reference/index.html

## Comments

A winning path would be to go South, South, West, South and grab the first key. Then go North, East, North, East and grab the second key. Then go South, South, East to grab the third and final key. Then go West, North, East, North to reach the exit. At this point, drop all three of the keys. Game over.


I found this project to be fun and exciting. It was interesting to see how to make a game and manage the states without mutating them. It was frustrating, as I felt that if I could just mutate the state, I would be able to complete the project much faster. Yet it was also rewarding, and once the pieces started to fall into place, it became trivial!
