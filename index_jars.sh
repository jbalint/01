#!/bin/bash

# Build an index of class/resource -> JAR_1[,JAR_n,...]
# Using Kyoto Cabinet

# To search:
# kchashmgr list jarindex.kch | grep ObjectLogicProjectFactory | xargs kchashmgr get jarindex.kch

# ~/sw/OntoStudio% ~/sw/01/index_jars.sh
# /home/jbalint/sw/01/index_jars.sh: line 37: unexpected EOF while looking for matching `"'
# /home/jbalint/sw/01/index_jars.sh: line 45: syntax error: unexpected end of file

# name of the index file, must be created before running
JARINDEX=jarindex.kch

# add a jar file's contents to the index
index_jarfile()
{
	jarfile=$1
	jar -tf $jarfile | \
		while read resource ; do
			add_resource $resource $jarfile
		done
}

# add a resource in the given jar file to the index
add_resource()
{
	resource=$1
	jarfile=$2
	cur=
	# if it exists, we get the current value and then append ours
	if [[ `kchashmgr get "$JARINDEX" "$resource" 2> /dev/null` ]] ; then
		cur=`kchashmgr get "$JARINDEX" "$resource"`
		cur="${cur},"
	fi
	kchashmgr set "$JARINDEX" "$resource" "$cur$jarfile"
}

# outer loop over jar files
find plugins -name '*.jar' | \
	while read jarfile ; do
		index_jarfile "$jarfile"
	done
