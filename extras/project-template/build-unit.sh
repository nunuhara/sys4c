#!/bin/bash

# The purpose of this script is to check if a unit needs recompilation and
# then compile it if necessary. It does this by using the -m option to sys4c
# to check if any of the external declarations it uses have changed in its
# dependencies. (It's assumed that the dependencies are up-to-date.)

# $1 - output file name (.ain)
# $2 - hll library file name (.ain)
# $3 - input file name (.jaf)
# $4... - dependency file names (.ain)

OUTFILE="$1"
LIBFILE="$2"
INFILE="$3"

if [ $# -gt 3 ]; then
	shift 3
	# FIXME: breaks if space in filename; need to loop over args and quote?
	M_ARGS=$(printf -- "-m %s " $@)
	I_ARGS=$(printf -- "-i %s " $@)
fi

if [ ! -f $OUTFILE ] || [ $INFILE -nt $OUTFILE ] || sys4c $M_ARGS $OUTFILE; then
	printf -- "sys4c -c $I_ARGS -o $OUTFILE $LIBFILE $INFILE\n"
	sys4c -c $I_ARGS -o $OUTFILE $LIBFILE $INFILE
fi
