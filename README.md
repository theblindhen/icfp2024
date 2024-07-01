The Blind Hen 2024 ICFP Contest submission
==========================================

Team
----

Jonas B. Jensen,
Christoffer R. Nielsen,
Johan S. H. Rosenkilde, and
Kasper Svendsen

Jonas, Johan and Kasper in Aarhus, Denmark. Christoffer in Dubai, UAE

## Lambdaman problems

## Spaceship problems

## 3D problems

Our solutions were hand written, with the assistance of either Vim or Excel.
The harder problems were first implemented and tested in Python, where we
mostly restricted ourselves to the operations available in the 3D language.
Graphs on pen and paper were also indispensable.

We wrote a simulator that produced ASCII output similar to the input format.
This was crucial for shaking out subtle bugs and getting the timing just right.

## Efficiency problems

We solved the efficiency problems by manually inspecting ICFP terms. Problems involving fibonacci numbers, primes, 2-powers, etc. where solved by identifying the corresponding filters. Bit-encoding and Sudoku problems were solved by translation into SAT and integer linear programming problems (solved using GLPK). 

## Building and developing

See [HOWTO.md](HOWTO.md).
