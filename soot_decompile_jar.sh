#!/bin/bash

set -x

# TODO check soot jar exists
SOOTJAR=/home/jbalint/Downloads/soot-2.5.0.jar

unset GREP_OPTIONS

JAR=$1

IFS=:
SOOTCP=$*
unset IFS

DIR=`basename $JAR`

mkdir $DIR

#jar -tf $1 | grep '\.class$' | perl -lpe's/\.class$//;s/\//./g' |  xargs java -jar $SOOTJAR -cp $JAVA_HOME/jre/lib/rt.jar:"$SOOTCP" -f g -d $DIR -v

jar -tf $1 | grep '\.class$' | perl -lpe's/\.class$//;s/\//./g' | while read i ; do
	java -jar $SOOTJAR -cp $JAVA_HOME/jre/lib/rt.jar:"$SOOTCP" -f g -d $DIR $i
done

