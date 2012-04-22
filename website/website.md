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

This website is a work in progress; it contains project source releases,
together with raw evaluation data and the scripts used to produce the tables and
diagrams in our paper.


<!-- Talk of versions. Use dates. -->
We provide two source releases:

- the source release on which we ran the evaluation in the paper (0.1, from 2012-04-09);
- the current source release (0.2 from 2012-04-22), which corresponds more closely to the code
  presented in the paper (with mostly cosmetic changes and some bugfixes).

- the evaluation data from our paper submission, including the raw dataset and scripts to regenerate the graphics and the tables.
- updated evaluation results from version 0.2, showing essentially the same
  results.
<!--the elaboration scripts and statistics-->

# Credits
This project benefitted from code and ideas of many different people:

- Paolo G. Giarrusso (project leader)
- Klaus Ostermann
- Michael Eichberg
- Tillmann Rendel
- Christian Kaestner
- Katharina Haselhorst

# Releases

- 0.2 - 2012-04-22 - [.tar.gz](tarballs/squopt-v0.2.tar.gz)
    - Renames and refactorings to match the paper.
    - Updated the evaluation code (`COVARIANT_EQUALS`,
      `RUN_FINALIZERS_ON_EXIT` were modified slightly to match FindBugs)
    - Updated PaperTutorial and resynced with the paper
    - Fixed a few bugs.
    - Less type annotations are needed in the optimizer.
- 0.1 - 2012-04-09 - [.tar.gz](tarballs/squopt-v0.1.tar.gz)
    - First release

Note: User documentation is currently missing.

# Code from our paper
Examples from our paper are implemented and tested in class `ivm.tests.PaperTutorial`.

# Evaluation
The evaluation included in the paper was done on release 0.1.
The code is located in class `performancetests.opaltests.FindBugsAnalyses`.

We provide the raw data collected, together with the script used to produce the
tables included in the paper, based on R and runnable on Unix systems such as
Mac OS X and probably Linux. [Here](tarballs/evaluation-v0.2.tar.gz) they are.
