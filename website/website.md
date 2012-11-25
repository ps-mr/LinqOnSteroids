% SQuOpt---The Scala Query Optimizer
% Paolo G. Giarrusso
% 2012-10-10
# Introduction

This is the homepage for the Scala Query Optimizer research project, _SQuOpt_.

Often developers need to manually optimize computations on collections at the expense of
readability and modularity. In the SQuOpt project we reuse and extend optimizations known in
the database community to apply them over such collections and enjoy the
modularity advantages.

Concretely, SQuOpt allows to write queries on Scala collections and run
optimizations on them.
Queries are expressed through a quite natural syntax, almost identical to the
syntax for Scala for comprehensions; however, executing such for comprehensions
yields a representation of the query, which can then be optimized.

This website will continue being updated.

# What we provide
<!--
We provide two source releases:

- the source release on which we ran the evaluation in the paper (0.1, from 2012-04-09);
- the current source release (0.2 from 2012-04-22), which corresponds more closely to the code
  presented in the paper (with mostly cosmetic changes and some bugfixes).
-->

- Source code of our implementation, on [GitHub](https://github.com/ps-mr/LinqOnSteroids).
- the [version of FindBugs](https://github.com/ps-mr/FindBugsBenchmark) we compared our results with.
- a [technical report](giarrusso-techrep.pdf) accompanying our paper submission,
  also available on [arXiv](http://arxiv.org/abs/1210.6284).

Additionally, we will provide the evaluation data from our new paper submission, including the
raw dataset and scripts to regenerate the graphics and the tables.
The raw evaluation data from a previous evaluation is already available.

<!--
- updated evaluation results from version 0.2, which lead to essentially the same
  conclusions.
-->
<!--the elaboration scripts and statistics-->

# Credits
This project benefited from code and ideas of many different people:

- Paolo G. Giarrusso (project leader)
- Klaus Ostermann
- Michael Eichberg
- Ralf Mitschke
- Tillmann Rendel
- Christian Kästner
- Sebastian Erdweg
- Katharina Haselhorst

# News
- 2012-10-25: link to technical report on arXiv.
- 2012-10-23: technical report published.
- 2012-10-12: began to update website, with link to GitHub sources.
- 2012-04-26: added FindBugs source code and some details on reproducing the
  evaluation
- 2012-04-23: website online

<!--
# Releases

- 0.2 --- 2012-04-22 --- [.tar.gz](SQuOpt/tarballs/squopt-v0.2.tar.gz)
    - Renames and refactorings to match the paper.
    - Updated the evaluation code (`COVARIANT_EQUALS`,
      `RUN_FINALIZERS_ON_EXIT` were modified slightly to match FindBugs)
    - Updated PaperTutorial and resynced with the paper
    - Fixed a few bugs.
    - Less type annotations are needed in the optimizer.
- 0.1 --- 2012-04-09 --- [.tar.gz](SQuOpt/tarballs/squopt-v0.1.tar.gz)
    - First release
Note: User documentation is currently missing.

-->

# Code from our paper
Examples from our paper are implemented and tested in class [`ivm.tests.PaperTutorial`](https://github.com/ps-mr/LinqOnSteroids/blob/master/src/test/scala/ivm/tests/PaperTutorial.scala).

# Old evaluation
<!--
The evaluation included in the paper was done on release 0.1; the new release
gives similar results.
-->
The code is located in class `performancetests.opaltests.FindBugsAnalyses`.

We provide the raw data collected, together with the script used to produce the
tables included in the paper, based on R and runnable on Unix systems such as
Mac OS X and probably Linux. [Here](SQuOpt/tarballs/evaluation-v0.2.tar.gz) they are.

For those interested only in the resulting graphs,
[here](SQuOpt/EvalRed-new.pdf) are the results.

In our evaluation, we compare our source code analyses with a reference
implementation, namely FindBugs. We altered its source code to disable unrelated
analyses, and provide [the altered code](https://github.com/ps-mr/FindBugsBenchmark).

To ensure reproducibility, we plan to simplify and document the exact steps needed to
reproduce our results.

For the determined impatient, here is the current procedure, which will probably
have a few rough edges and is still untested.

- select a idle and sufficiently powerful machine with a *x operating system -
the evaluation takes currently 1 day and was run on a Linux machine; the
statistics were computed on a Mac OS X machine. The scripts _should_ work
(with minor changes) on any *x system with the needed software.
- download both our source release, our evaluation code, and
[R](http://www.r-project.org/).
- choose the test data to use---we provide a test harness to run the evaluation on
ScalaTest, that is `profileLos.sh`, or on the JDK, that is `profileLosJDK.sh`.
- To reproduce the original evaluation, you need to use the JDK. The binary
  library is in fact available in `data/rt.jar`.
    - Download the [version which was installed on our machine][JDKLink]
and extract it, using for instance `rpm2cpio` (we plan to simplify this step).
    - Adjust `profileLosJDK.sh` to find the copy of `classes.jar` from the extracted
  package.
- Run the chosen test harness. You might need to comment out any invocations to
  `git`; the software assumes to be run on a `git` working copy.
- After the end, move and rename the resulting `LOSTestLog-JDK.csv` or
`LOSTestLog-ScalaTest.csv` as `LOSTestLog-JDK-new-eval.csv` 
inside `squoptEval-v0.2` (the folder of the extracted evaluation).
- In the same directory, run `make newgraph`, which will save the reproduced
evaluation results by overwriting `EvalRed-new.pdf`.

# Contacts
For any question or suggestion, feel free to contact me, Paolo G. Giarrusso, at
pgiarrusso (at) informatik !dot! uni-marburg !dot! de.

[JDKLink]: http://rpmfind.net/linux/RPM/centos/updates/6.2/x86_64/Packages/java-1.6.0-openjdk-1.6.0.0-1.43.1.10.6.el6_2.x86_64.html
