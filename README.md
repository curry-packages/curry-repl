curry-repl
==========

This package contains the implementation of a
universal REPL (Read-Eval-Print-Loop) which can be used
on top of a Curry compiler. Thus, if the Curry compiler
supports some standard options (see below), it can
be extended to a simple interactive programming environment.

The specification of the concrete compiler is provided
as an element of type `REPL.Compiler.CCDescription`.
The entry point of the REPL is the operation `mainREPL`
defined in module `REPL.Main`.
The directory `examples` contains example specifications.


Requirements for a Curry Compiler used by a REPL
------------------------------------------------

Basically, the REPL translates each expression to be evaluated
into a program with a `main` operation. This program is
compiled and then executed. In order to implement this
kind of REPL, it should be possible to invoke the Curry compiler `cc`
as follows:

    cc [-vn] [-iDIR] ... [-iDIR] [--parse-options="PO"] [-c|--compile] MOD

* `-vn`: verbosity with 0<=n<=4
* `-iDIR`: use `DIR` to search for imported modules
* `-c`: compile only
* `PO`: options passed to the Curry front end
* `MOD`: the module to be compiled containing an operation `main`

This command should compile module `MOD` (and, if necessary,
all imported modules) and create an executable `MOD` which
executes the operation `main` in this module. `MOD` might
be a hierarchical module name, e.g., `Dir.Mod`. In this case,
the executable `Dir.Mod` is generated where the source code
of the module is stored in `Dir/Mod.curry`.

If the option `-c` or `--compile` is provided,
the executable is not generated.
This might be reasonable to compile all modules in order
to check for errors and speed up later compilations
without re-compiling all imports.

If the module name is prefixed by a path, e.g., `dir1/dir2/Mod`,
then we change into the directory of the path (`dir1/dir2`)
and compile the main module there.


Further options:
----------------

Options beyond this general usage might depend on the compiler.
For instance, each compiler might implement a different set
of search strategies.
Here are some examples of options and their values:

* interactive (yes/no): should the user be asked to print the next result?
* first (yes/no): print only one result and stop?
* results (int n): number of results to be printed (n=0: infinite)
* time (yes/no): print execution time?
* strategy: search strategy to be used, e.g., dfs/bfs/ids/fs
* debug (yes/no): interactive debugging
* trace (yes/no): trace evaluation
* printdepth (int d): print resulting terms up to depth d (d=0: no limit)

The actual options are specified by data of type `CCOption`
(see module `REPL.Compiler`).


Requirements for Curry Compilers used by CPM
============================================

In order to use a Curry compiler together with CPM,
it has to support some options so that CPM can interact
with the compiler. The Curry REPL implements these options
provided that the Curry compiler itself provides options
about its version. If `cc` is the executable of the compiler,
the following options must exist:

* `cc --compiler-name`: Shows that name of the compiler (which occurs
  in compiler dependencies of package specifications) and quits.
* `cc --numeric-version`: Shows the compiler version quits.
* `cc --base-version`: Shows the version of the base libraries implemented
  by the compiler and quits.

These options can also be combined. In this case the information
is shown in subsequent lines, as shown in this example
for [PAKCS](http://www.informatik.uni-kiel.de/~pakcs/):

    > pakcs --compiler-name --numeric-version --base-version
    pakcs
    3.3.0
    3.0.0

The intermediate files produced by the compiler should be
stored in the directory

    .curry/<compiler-name>-<numeric-version>

relative to the directory of the source file.
If the source file is a hierarchical module,
the same hierarchy is used relative to `.curry`.


Running programs
----------------

When CPM starts a Curry system (via `cypm curry`), it sets
the environment variable `CURRYPATH` to the load path of all
included packages and passes the option `--nocypm` to the
executable of the Curry system. Usually, when a Curry system
is started, it should query CPM (by `cypm deps -p`) to get
the value of `CURRYPATH` to load modules. The option `--nocypm`
is intended to turn off this behavior.


Installing executables
----------------------

If CPM installs an executable, it passes the following options
(REPL commands) to the compiler:

   > cc :set v0 :load MAINMOD :save :quit

(where `v0` is replaced by `v1` in debug mode).
The execution of this command should install the executable `MAINMOD`.