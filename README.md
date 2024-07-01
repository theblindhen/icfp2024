The Blind Hen 2024 ICFP Contest submission
==========================================

Team
----

Jonas B. Jensen,
Christoffer R. Nielsen,
Johan S. H. Rosenkilde, and
Kasper Svendsen

Jonas, Johan and Kasper in Aarhus, Denmark. Christoffer in Dubai, UAE

We wrote most of our code in OCaml, plus a sprinkle of Python, shell script,
and domain-specific tools like Gnuplot and SAT/LP/IP solvers.

## Lambdaman problems

At first we dived straight into path-finding strategies for solving the problem
with a minimal number of moves. When we saw that the encoding mattered, we
tried to find efficient encodings of the paths we found, like run-length
encoding and packing 2-bit move values into large integers.

Our early ICFP-language interpreter wasn't efficient enough to interpret map
21, which wasn't a string literal, so we hand-translated the code into a
roughly equivalent OCaml program and got the map that way.

By Sunday evening it was clear from the scores of other teams that we had the
wrong strategy, and only at this point did we realise that it was valid to walk
into walls. We switched strategies completely and instead arranged for our
solutions to be a random seed plus an ICFP-language program to interpret that
random seed into a string of moves. For bigger maps we had multiple random
seeds packed into the same integer, with each random seed covering a section of
the map. This meant that we could search for the best seed in stages, keeping
the partial solution while exploring the rest. This worked well enough for
almost all maps and could have probably worked for map 19 with a bit of tuning
of the goal function (or exploiting symmetries in the map).

## Spaceship problems

Most of our submissions were made with a solver we wrote without even looking
at the problems (a bad idea in hindsight). The solver will search in the
4-dimensional statespace (x,y,vx,vy) to try and reach some a waypoint in the
fewest possible timesteps. If no waypoint can be reached in 7 timesteps, the
solver determines the nearest waypoint (in x,y chessboard distance) and
moves there in the fastest way that ends with speed at most 1. Then it goes
back to the search strategy.

At the end of the lightning round we looked at the actual maps (with gnuplot)
and saw that most of them were just curves. Our ranking on Spaceship was
acceptable at that point, so we moved on to other problems and only revisited
Spaceship in the final hours of the contest, where our score had greatly fallen
behind.

In our second go at the problem, we quickly built a way of visualising
solutions: write out a trace that could be consumed by Gnuplot. We also built a
solver that was intended to work well on curve-like problems:
1. Sort the points to be visited, with the point nearest the spaceship coming
   first, then the nearest point to the first coming second, etc.
2. Write out the ordering to a file and allow reading back this file. We could
   generate optimal solutions for the first few maps by reordering lines in
   this file manually, but unfortunately we only added this functionality very
   late, so we only had time to make use of it on 2-3 maps.
3. Repeatedly use A* search to find a route through the next _two_ points, then
   use that route to visit only the first _one_ point, and repeat. Having a good
   heuristic here turned out to be critical for performance.

## 3D problems

Our solutions were hand written, with the assistance of either Vim or Excel.
The harder problems were first implemented and tested in Python, where we
mostly restricted ourselves to the operations available in the 3D language.
Graphs on pen and paper were also indispensable.

We wrote a simulator that produced ASCII output similar to the input format.
This was crucial for shaking out subtle bugs and getting the timing just right.

## Efficiency problems

We solved the efficiency problems by manually inspecting ICFP terms. Problems involving fibonacci numbers, primes, 2-powers, etc. where solved by identifying the corresponding filters. Bit-encoding and Sudoku problems were solved by translation into SAT and integer linear programming problems (solved using GLPK).

We had first thought that the hints about call-by-value and call-by-need were
indications that our ICFP-language interpreter ought to be extended with those
evaluation strategies. We implemented a light-weight version of call-by-need
that was good enough to evaluate the first efficiency problem, but we found no
use for it beyond that. Only near the end of the contest did we remember that
the task description had a section about unknown operators, and we made some of
our lambdaman solutions potentially more efficient to evaluate on the server
side by making use of call-by-need in a few places.

## Building and developing

See [HOWTO.md](HOWTO.md).
