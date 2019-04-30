#!/bin/bash


usage()
{
    echo "usage: sysinfo_page [[-rt] | [-h]]"
}

success='### SUCCESS: Files Are Identical! ###'
fail='### WARNING: Files Are Different! ###'
tmp=false
help=false
while [ "$1" != "" ]; do
    case $1 in
        -t | --tmp )     				shift
                                tmp=true
                                ;;
        -h | --help )           help=true
																usage
                                exit
                                ;;
        * )                     usage
                                exit 1
    esac
    shift
done

make clean
make


find . -name '*.sched' | while IFS= read -r line ; do
    echo "$line"
		./schedch.native -a "$line" > "$line".aout.tmp
		cmp --silent "$line".aout.tmp "$line".aout && echo "    $success" || echo "    $fail"
		./schedch.native -s "$line" > "$line".sout.tmp
		cmp --silent "$line".sout.tmp "$line".sout && echo "    $success" || echo "    $fail"
		./schedch.native -l "$line" > "$line".lout.tmp
		cmp --silent "$line".lout.tmp "$line".lout && echo "    $success" || echo "    $fail"
done

if [ $tmp = false ] ; then
	find . -type f -name '*.tmp' -exec rm {} +
fi
