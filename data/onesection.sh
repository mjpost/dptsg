#!/bin/bash

# Creates source files from sections of the Treebank
# usage: [PENNWSJTREEBANK=/path/to/treebank] [force=1] section=N onesection.sh

set -u

# overwrite existing files if set to true
: ${force=0}
# the location of the Penn Treebank source trees (from the LDC)
: ${PENNWSJTREEBANK=/p/mt/corpora/wsj}

o=wsj.trees.$section
if test ! -e $o || $force; then
    echo "creating $o"
    cat $PENNWSJTREEBANK/$section/*mrg | ~/code/dpinfer/scripts/oneline_treebank.pl > $o
else 
    echo "$o already created"
fi

o=wsj.trees.$section.clean
if test ! -e $o || $force; then
    echo "creating $o"
    cat wsj.trees.$section | ~/code/dpinfer/scripts/clean.pl > $o
else 
    echo "$o already created"
fi

# extract the sentences
o=wsj.$section.words
if test ! -e $o || $force; then
    echo "creating $o"
    cat wsj.trees.$section.clean | ~/code/dpinfer/scripts/print_leaves.pl > $o
else 
    echo "$o already created"
fi

# extract the words with max40
o=wsj.$section.words.max40
if test ! -e $o || $force; then
    echo "creating $o"
    awk '{if (NF <= 40) print}' wsj.$section.words > $o
else 
    echo "$o already created"
fi

o=wsj.$section.words.lines-max40
if test ! -e $o || $force; then
    echo "creating $o"

    awk '{ nlines++; if (NF <= 40) print nlines; }' wsj.$section.words > $o
else 
    echo "$o already created"
fi

o=wsj.trees.$section.clean.max40
if test ! -e $o || $force; then
    echo "creating $o"

    ~/code/dpinfer/scripts/extract_lines.pl -l wsj.$section.words.lines-max40 -f wsj.trees.$section.clean > $o
else 
    echo "$o already created"
fi

o=wsj.$section.words.max40.unked
if test ! -e $o || $force; then
    echo "creating $o"

    cat wsj.trees.$section.clean.max40 | ~/code/dpinfer/scripts/print_leaves.pl -thresh 2 > $o
else 
    echo "$o already created"
fi

