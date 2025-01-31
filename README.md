curry-repl: A universal interactive environment for Curry compilers
===================================================================

This package contains the implementation of a
universal REPL (Read-Eval-Print-Loop) which can be used
on top of a Curry compiler. Thus, if the Curry compiler
supports some standard options (see below), it can
be extended to a simple interactive programming environment.

The specification of the concrete compiler is provided
as an element of type `REPL.Compiler.CCDescription`.
The entry point of the REPL is the operation `mainREPL`
defined in module `REPL.Main`.

The directory `examples` contains example specifications
for various Curry systems.

The directory `scripts` contains a simple shell script
to invoke a Curry REPL with appropriate path settings.
This script needs to be adapted to the actual Curry compiler.


Requirements for a Curry Compiler used by a REPL
------------------------------------------------

Basically, the REPL translates each expression to be evaluated
into a program with a `main` operation. This program is
compiled and then executed. In order to implement this
kind of REPL, it should be possible to invoke the Curry
compiler `curryc` as follows:

    curryc [-vn] [-iDIR] ... [-iDIR] [--parse-options="PO"] [-c|--compile] MOD

where the possible arguments are:

* `-vn`: verbosity with `0 <= n <= 4`
* `-iDIR`: use `DIR` to search for imported modules
* `-c`: compile only
* `PO`: additional options passed to the Curry front end
* `MOD`: the module to be compiled containing an operation `main`

This command should compile module `MOD` (and, if necessary,
all imported modules) and create an executable `MOD` which
executes the operation `main` in this module. `MOD` might
be a hierarchical module name, e.g., `Dir.Mod`. In this case,
the executable `Dir.Mod` is generated relative to the source directory,
e.g., if the source code of the module is stored in `Dir/Mod.curry`,
the executable is stored in the file `Dir.Mod`.

If the option `-c` or `--compile` is provided,
the executable is not generated.
This might be reasonable to compile all modules in order
to check for errors and speed up later compilations
without re-compiling all imports.

If the module name is prefixed by a path, e.g., `dir1/dir2/Mod`,
then the REPL changes into the directory of the path (`dir1/dir2`)
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

Print Bindings:
---------------

When `+bindings` is on, the values of variables that are free in the
to-be-evaluated expression will be shown.  If the compiler supports
the flags `-B` to enable it and`-V` for passing variable names via
command line, the compiler is responsible for showing these bindings.
For compilers that do not support this, the REPL will handle it in a
limited way.

RC file:
--------

The REPL reads on startup a compiler-specific configuration file
which contains definitions for some options and commands,
like a search path for additional libraries, commands to
show or edit source files, etc. A template for such a
configuration file is contained as `curryrc.default` in this package.
The REPL assumes that the home directory of the Curry compiler
contains a configuration file `CYCrc.default` if `CYC` is
the name of the compiler. If there is none, the configuration
file `curryrc.default` of this package is used.
On startup, the REPL copies this file (without the suffic `.default`)
into the user's home directory, if there is not already one, otherwise
the file is updated if the default file contains new fields.


Requirements for Curry Compilers used by CPM
============================================

The Curry REPL is the basic interface for an interactive Curry system.
Thus, many Curry systems implement the commands and options provided
by this universal REPL. This property is exploited by the
[Curry Package Manager (CPM)](http://www.curry-lang.org/tools/cpm)
in order to support the use of packages
for various Curry systems. The actual requirements of CPM
are described in the following.

In order to use a Curry system together with the Curry Package
Manager CPM, the Curry system has to support some options
so that CPM can interact with it.
The universal Curry REPL implements these options provided
that the Curry compiler itself provides options about its version. If
`curryc` is the executable of the compiler, the following options must
exist:

* `curryc --compiler-name`: Shows that name of the compiler (which occurs
  in compiler dependencies of package specifications) and quits.
* `curryc --numeric-version`: Shows the compiler version quits.
* `curryc --base-version`: Shows the version of the base libraries
  implemented by the compiler and quits.

Note that the universal Curry REPL provides the same options,
but they are implemented by passing them to the compiler.

These options can also be combined. In this case the information
is shown in subsequent lines, as shown in this example
for [PAKCS](http://www.curry-lang.org/pakcs/):

    > pakcs --compiler-name --numeric-version --base-version
    pakcs
    3.3.0
    3.0.0

In order to support the use of different Curry compilers in parallel,
the compiler should store its generated intermediate files
in the directory

    .curry/<compiler-name>-<numeric-version>

relative to the directory of the source file.
If the source file is a hierarchical module,
the same hierarchy is used relative to `.curry`.
For instance, if the PAKCS compiler version 3.3.0 is used
to compile a hierarchical module `Dir.Mod` stored in the
file

    /home/curry/base/Dir/Mod.curry

the Prolog target file is stored in

    /home/curry/base/.curry/pakcs-3.3.0/Dir/Mod.pl


Running programs
----------------

In order to use a Curry system (and its REPL) conveniently,
the Curry system should query CPM (by `cypm deps -p`) to get
the value of `CURRYPATH` to load modules at startup time.
This is usually done by a separate shell script which invokes
the actual executable of the REPL.

When CPM starts a Curry system (via the command `cypm curry`),
it sets the environment variable `CURRYPATH` to the load path of all
included packages and passes the option `--nocypm` to the
Curry system. This option should have the effect that CPM is not invoked
at startup time (to avoid a cyclic invocation of both systems).
The implementation of the universal Curry REPL just ignores
the option `--nocypm` (as mentioned above, the querying of CPM
is usually done by a separate shell script which invokes the REPL).


Installing executables
----------------------

If CPM installs an executable (by the command `cypm install`),
it passes the following options to the Curry system
(where `curry` denotes the main executable of the Curry system,
i.e., the REPL):

    > curry :set v0 :load MAINMOD :save :quit

(where `v0` is replaced by `v1` in debug mode).
The execution of this command should install the executable `MAINMOD`.
Hence, these options are also implemented by the universal Curry REPL.
