# Compilation instructions
To compile this project, you can use SBT.

## Using SBT
The project uses SBT 0.10.1 - you need to install that or a later version of SBT; a pre-installed SBT 0.7.x will not work.

1) To install SBT, follow instructions on this page:
  <https://github.com/harrah/xsbt/wiki/Getting-Started-Setup>
2) After installing SBT, cd into this folder and launch `sbt`. It will find the build instructions in `build.sbt`. During the first execution it will auto-download various dependencies needed for itself (including the Scala compiler).
3) Then, at the SBT prompt, give the `update` command (simply `update` followed by newline). This step downloads external libraries and must only be repeated, when new external libraries are added.
4) Finally, you can similarly run any command between `compile`, `test:compile` (which also compiles tests) and `test` (which additionally _runs_ the tests - they include performance tests and take some time). I usually run tests using IntelliJ IDEA to specify which one I am interested in, even if I do not use it for compilation.
In particular, tests in the `ivm.performancetests` package take considerable time.

Note: this project incorporates `lib/bat-1.6.ALPHA.jar`, a compiled snapshot of
BAT from <https://github.com/Delors/BAT>, commit
`307df72accd1c0ea48b6d4e98fe5b19c78d082ef`.

## Supported IDEs
You can edit the project using IntelliJ IDEA 11 with the Scala and SBT plugins
installed, beyond SBT itself.
Since IDEA 11 was recently released you will probably need to update it.
Integrated compilation with IDEA might or might not work; it is anyway too slow
to be useful, therefore I wouldn't recommend it.

There are also Eclipse project files (requiring the Scala plugin), but no active
development uses Eclipse, therefore they are likely to be out-of-date.

We stopped using Eclipse because the Scala plugin was too buggy when we last
tried (as I last tested it).


11 Dec 2011
