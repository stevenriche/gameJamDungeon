#DUNGEON CRAWL

This is a game I made for the 2018 Q4 IP sprint Game Jam. It is my first experience writing anything
using Haskell (so it probably isn't written very well).

##How to Run

1. First, [Install Haskell](https://www.haskell.org/platform/).

2. Next, you will need to compile the game. From the root directory of this repo:

```
$ ghc --make dungeon
```

3. Once the code is compiled, you can run it from the root directory of this repo:

```
$ ./dungeon
```

##How to Play

This game is a simple dungeon crawler, similar to old Rogue-like games (but with fewer features).
You see the level depicted as an ASCII map, and you must move your character to the exit of each
level.

###Character Key

`.` is an open area in the dungeon room.

`#` is a wall that you (and most monsters) cannot cross.

`@` shows your position in the room.

`x` is the exit for that level.

`*` is a landmine you have placed. Landmines will kill monsters (and yourself) if they come into
contact with it. You have a limited number of landmines (and only starting on Level 4).

`M` is a standard, aggressive monster. It moves 2 spaces every turn, directly towards the player.

`B` is a blocking monster. It moves 1 space every turn, trying to get to the exit to block it from
the player.

`S` is a sneaky monster. It will move 2 spaces towards the midpoint between the player and the exit,
trying to anticipate your moves.

`G` is a ghost monster. It will move 1 space every turn directly towards the player. It can pass
directly through walls, leaving gaps behind it that the player or other monsters can pass through.

###Movement

Each turn, the player will enter `w`, `a`, `s`, or `d` to move one space up, left, down, or right
respectively. If you have landmines available (shown at the bottom of the level), you can instead
enter `*w`, `*a`, `*s`, or `*d` to leave a landmine behind you when you move.

You can also enter `q` to quit the game.

##Development

This was my first Haskell project, so I still haven't had much of a chance to really get a feel for
the language and use its strengths to my advantage - I imagine a lot of the code I have now is an
attempt to force imperative designs in a declarative way.

I have set up the code in such a way that it is easy to add new levels. Each level is basically a
list of strings that draw out the level in text. The game engine can read and understand the logic
based on the characters listed above. Monsters placed in the map will behave as expected, and
you can enable landmines for the character by listing the landmines as `*`'s sandwiched between
`[` and `]` somewhere on the map. Created levels can then be added into the list of levels in
`Levels/LevelList.hs`.
