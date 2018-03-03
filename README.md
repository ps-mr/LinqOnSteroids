# Intro

This repository contains a proof-of-concept implementation and examples for the [SQuOpt research project](http://www.informatik.uni-marburg.de/~pgiarrusso/SQuOpt/) and the relevant paper:

P. G. Giarrusso, K. Ostermann, M. Eichberg, R. Mitschke, T. Rendel, and C. Kästner. Reify your collection queries for modularity and speed! In *Proceedings of AOSD*, pages 1–12. ACM, 2013. Extended version available from http://www.informatik.uni-marburg.de/~pgiarrusso/SQuOpt/giarrusso-techrep.pdf, Official Version from https://dl.acm.org/citation.cfm?id=2451438.

# Code examples
An example of the optimization abilities of the framework is available in
[`src/test/scala/ivm/tests/PaperTutorial.scala`](src/test/scala/ivm/tests/PaperTutorial.scala). When run as a test, it verifies
that the optimizer on the given query produces the same code as one gets by
manual optimization.

# Compilation instructions
To compile this project, you can use SBT.

## Using SBT
The project uses SBT 0.12.0; later versions of SBT will download the right one.

1. To install SBT, follow instructions on this page:
  
  <https://github.com/harrah/xsbt/wiki/Getting-Started-Setup>
  
  Modify the SBT setup to use up to 2G of RAM by adding `-Xmx2048M` to the `java`
  command line. Otherwise, compilation might be slower or run out of memory.
2. After installing SBT, cd into this folder and launch `sbt`. It will find the build instructions in `build.sbt`. During the first execution SBT will auto-download various dependencies needed for itself (including the Scala compiler).
3. Then, at the SBT prompt, give the `package` command (simply `package` followed by newline) to compile and package the library as a JAR file under the `target` folder. Compilation will require around 5-10 minutes on a recent machine.
4. You can similarly run commands such as `compile` (for compilation), `test:compile` (which also compiles tests) and `test` (which additionally _runs_ the tests - they include performance tests and take some time). I usually run tests using IntelliJ IDEA to specify which one I am interested in, even if I do not use it for compilation.
In particular, tests in the `performancetests` package take considerable time.

Note: this project incorporates `lib/bat-1.6.0.RC1.jar`, a compiled snapshot of
BAT from <https://github.com/Delors/BAT>.

## Supported IDEs
You can edit the project using IntelliJ IDEA 11 with the Scala and SBT plugins
installed, beyond SBT itself. We supply IDEA project files, but it is probably
best to regenerate them locally via SBT through the `gen-idea` command.

Integrated compilation with IDEA is not supported; it is anyway too slow
to be useful, therefore I would not recommend it.

There are also Eclipse project files (requiring the Scala plugin), but no active
development uses Eclipse, therefore they are likely to be out-of-date.

We stopped using Eclipse because the Scala plugin was too buggy when we last
tried (as I last tested it).


23 Apr 2012
