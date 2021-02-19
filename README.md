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
