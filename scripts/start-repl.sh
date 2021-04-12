#!/bin/sh
#
# A simple shell script to start interactive read-eval-print loop of
# a Curry system together with CPM to compute the actual load path
# and rlwrap to support input line editing.

###########################################################################
# Adapt the following definitions to the actual Curry system:

CURRYHOME=`echo CURRYHOME must be defined here!`
export CURRYHOME

# The directory where CPM installs the binaries:
CPMBIN="$HOME/.cpm/bin"

# The executable of the Curry REPL installed by CPM:
REPL="$CPMBIN/curry-repl"

###########################################################################
# Invoke the Curry REPL with CPM to compute CURRYPATH:

# Check whether we should call CPM to compute the correct load path:
WHICHCPM=`which cypm`
if [ ! -d "$HOME" ] ; then
  USECPM=no   # do not use CPM without a home directory
elif [ -x $CPMBIN/cypm ] ; then
  CYPMBIN=$CPMBIN/cypm
  USECPM=yes
elif [ -x "$WHICHCPM" ] ; then
  CYPMBIN=$WHICHCPM
  USECPM=yes
else
  USECPM=no
fi

# Use readline wrapper `rlwrap` for REPL if `rlwrap` exists,
# we have tty as stdin, and we have a home dir to store rlwrap's history:
USERLWRAP=no
if tty -s ; then
  RLWRAP=`which rlwrap`
  if [ -x "$RLWRAP" -a -d "$HOME" ] ; then
    USERLWRAP=yes
  fi
fi

# check arguments for appropriate settings:
for i in $* ; do
  case $i in
    --help | -h | -\? ) USECPM=no ;;
    --version | -V    ) USECPM=no ;;
    --numeric-version | --compiler-name | --base-version ) USECPM=no ;;
    --nocypm ) USECPM=no ;;
    --noreadline ) USERLWRAP=no
  esac
done

if [ ! -x "$REPL" ] ; then
  echo "ERROR: executable '$REPL' not found!" >&2
  echo "Run: cd $CURRYHOME && make" >&2
  exit 1
fi

if [ $USECPM = yes ] ; then
  # set CURRYPATH with 'deps' command of CPM
  echo "Compute CURRYPATH with '$CYPMBIN'..."
  CPMPATH=`"$CYPMBIN" -v quiet -d CURRYBIN="$REPL" deps -p`
  if [ $? -gt 0 ] ; then
    echo $CPMPATH
    exit 1
  fi
  if [ -n "$CURRYPATH" ] ; then
    CURRYPATH=$CURRYPATH:$CPMPATH # keep existing CURRYPATH setting
  else
    CURRYPATH=$CPMPATH
  fi
  export CURRYPATH
fi

# do not use rlwrap inside emacs:
if [ "$TERM" = dumb ] ; then
  USERLWRAP=no
fi

if [ $USERLWRAP = yes ] ; then
  exec rlwrap -c "$REPL" ${1+"$@"}
else
  exec "$REPL" ${1+"$@"}
fi
