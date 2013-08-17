#!/bin/sh

PEARC="./pear"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0
testc=1

Usage() {
    echo "Usage: testall.sh [options] [.pear files]"
    echo "-c    Test C output files"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.pear//'`
    reffile=`echo $1 | sed 's/.pear$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""
    if [ $testc -eq 0 ] ; then
        generatedfiles="$generatedfiles ${basename}.i.out" &&
        Run "$PEARC -i" "<" $1 ">" ${basename}.i.out &&
        Compare ${basename}.i.out ${reffile}.out ${basename}.i.diff
    else
        generatedfiles="$generatedfiles ${basename}.i.out" &&
        Run "$PEARC" "<" $1 ";" "cat prog.c >" ${basename}.i.out &&
        Compare prog.c ${reffile}.out ${basename}.i.diff
    fi

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

while getopts kdpshi c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage
	    ;;
        i) # Test interpreted
            testc=0
            ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    if [ $testc -eq 0 ] ; then 
        # Basic tests
        files="testsi/fail-*.pear testsi/test-*.pear"
    else
        # Test Gtk
        files="tests/fail-*.pear tests/test-*.pear"
    fi
fi

for file in $files
do
    case $file in
	*test-*)
	    Check $file 2>> $globallog
	    ;;
	*fail-*)
	    CheckFail $file 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

exit $globalerror
