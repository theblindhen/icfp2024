# > get index
Hello and welcome to the School of the Bound Variable!

Before taking a course, we suggest that you have a look around. You're now looking at the [index]. To practice your communication skills, you can use our [echo] service. Furthermore, to know how you and other students are doing, you can look at the [scoreboard].

After looking around, you may be admitted to your first courses, so make sure to check this page from time to time. In the meantime, if you want to practice more advanced communication skills, you may also take our [language_test].

# > echo We come in peace
We come in peace

You scored some points for using the echo service!

# > get scoreboard
| # | team | hello | lambdaman | spaceship |
| --- | --- | --- | --- | --- |
| 1 | RGBTeam | 2 | 2 | 2 |
| 1 | manarimo | 2 | 2 | 2 |
...

# > get language_test
tbd

# > get lambdaman
Welcome to the Lambda-Man course.

It was the year 2014, and many members of our community worked hard to control Lambda-Man. Now, ten years later, this wonderful event is still memorized by holding a small Lambda-Man competition.

This course will teach you how to optimally control Lambda-Man to eat all pills. There is no fruit involved (neither low-hanging nor high-hanging), and even better: no ghosts! The input to each problem is a simple rectangular grid such as the following:

```
###.#...
...L..##
.#######
```

The grid contains exactly one `L` character, which is the starting position of Lambda-Man. There will be one or more `.` characters indicating the locations of pills to be eaten, and `#` characters are walls. The outside boundary of the grid is considered to consist of walls as well.

A solution should be a string of `U`, `R`, `D` and `L` characters (up, right, down, left, respectively) indicating the path to take. For example, a possible solution to the above example grid is the following path:
```
LLLDURRRUDRRURR
```
When Lambda-Man is instructed to move into a square containing a wall, nothing happens and the instruction is skipped. Your solution may consist of at most `1,000,000` characters.

The following levels are available:
* [lambdaman1] Best score: 33.
* [lambdaman2] Best score: 44.
* [lambdaman3] Best score: 58.
* [lambdaman4] Best score: 402.
* [lambdaman5] Best score: 215.
* [lambdaman6]
* [lambdaman7] Best score: 475.
* [lambdaman8] Best score: 321.
* [lambdaman9]
* [lambdaman10]
* [lambdaman11] Best score: 9997.
* [lambdaman12] Best score: 10003.
* [lambdaman13] Best score: 9993.
* [lambdaman14] Best score: 10011.
* [lambdaman15] Best score: 9993.
* [lambdaman16] Best score: 8209.
* [lambdaman17] Best score: 2922.
* [lambdaman18] Best score: 14607.
* [lambdaman19] Best score: 16364.
* [lambdaman20] Best score: 21934.
* [lambdaman21]

To submit a solution, send an ICFP expression that evaluates to:

```
solve lambdamanX path
```

Your score is number of bytes that the ICFP expressions consists of (i.e. the size of the POST body), so a lower score is better.

# > get spaceship
Welcome to the Spaceship course!

In 2020, most of us have learned how to operate a spaceship. In this course we'll play a small chess-like game featuring the spaceship! The game operates on an infinite 2D chess board, with the spaceship initially located on `(0,0)`. The spaceship has a velocity `vx` and `vy`, which are initially both set to `0`. In each turn the player can increase/decrease each of those numbers by at most one, and then the piece moves `vx` steps to the right and `vy` steps up.

Moves are represented with a single digit, inspired by the old numeric pad on a computer keyboard that we used to have in the old days on Earth. For example, `7` means decreasing `vx` and increasing `vy` by `1`, while `6` means increasing `vx` by `1` and keeping `vy` the same. A path can then be represented by a sequence of digits, e.g. the path `236659` visits, in this order, the following squares: `(0,0) (0,-1) (1,-3) (3,-5) (6,-7) (9,-9) (13,-10)`.

Now the challenge is the following: given a list of squares to be visited, find a sequence of moves that visits all those squares. Your solution may consist of at most `10,000,000` moves.

The following levels are available:
* [spaceship1] Best score: 5.
* [spaceship2] Best score: 50.
* [spaceship3] Best score: 10.
* [spaceship4] Best score: 1707.
* [spaceship5] Best score: 431.
* [spaceship6] Best score: 1990.
* [spaceship7] Best score: 502.
* [spaceship8] Best score: 458.
* [spaceship9] Best score: 19182.
* [spaceship10] Best score: 99750.
* [spaceship11] Best score: 1037710.
* [spaceship12]
* [spaceship13]
* [spaceship14]
* [spaceship15]
* [spaceship16]
* [spaceship17]
* [spaceship18]
* [spaceship19]
* [spaceship20]
* [spaceship21]
* [spaceship22]
* [spaceship23]
* [spaceship24]
* [spaceship25]

To submit a solution, send an ICFP expression that evaluates to:

```
solve spaceshipX moves
```

Your score is the number of moves, so a lower score is better.

# > get 3d