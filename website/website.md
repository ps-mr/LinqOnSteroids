% SQuOpt project page
% Paolo G. Giarrusso
% 2012-04-20
# Introduction

This is the homepage for the Scala Query Optimizer research project, _SQuOpt_.

Often developers need to manually optimize computations on collections at the expense of
readability. In the SQuOpt project we reuse and extend optimizations known in
the database community to apply them over such collections and enjoy the
modularity advantages.

Concretely, SQuOpt allows to write queries on Scala collections and run
optimizations on them.
Queries are expressed through a quite natural syntax, almost identical to the
syntax for Scala for comprehensions; however, executing such for comprehensions
yields a representation of the query, which can then be optimized.

This website is a work in progress, which will be available on 23 April.

Here you will find:
<!-- Talk of versions. Use dates. -->

- the last version of the source code, with updated evaluation results.
- the evaluation data from our paper submission, including the raw dataset and scripts to regenerate the graphics and the tables.
- the source code as of when we ran the evaluation (not fully up-to-date with the paper), v0.1.
<!--the elaboration scripts and statistics-->

# Credits
This project benefitted from code and ideas of many different people.

- Paolo G. Giarrusso (project leader)
- Klaus Ostermann
- Michael Eichberg
- Katharina Haselhorst
- Tillmann Rendel
- Christian Kaestner

# Releases

- v0.2 - 2012 04 21
    - Renamed a few functions to match the paper.
    - Fixed a few bugs
    - Updated the evaluation
- v0.1 - 2012 04 09 First released version

Note: User documentation is currently lacking.

# Evaluation
The evaluation included in the paper was done by running version 0.1.
We provide the raw data collected, together with the script used to produce the
tables included in the paper, based on R and runnable on Mac OS X
(and probably Linux).
