#!/bin/bash


usage()
{
    echo "usage: sysinfo_page [[-rt] | [-h]]"
}

success='### SUCCESS: Files Are Identical! ###'
fail='### WARNING: Files Are Different! ###'
tmp=false
help=false
replace=false
while [ "$1" != "" ]; do
    case $1 in
        -t | --tmp )     				shift
                                tmp=true
                                ;;
        -h | --help )           help=true
																usage
                                exit
                                ;;
				-r | --replace )				replace=true
																;;
        * )                     usage
                                exit 1
    esac
    shift
done

make clean
make

if [ $replace = true ] ; then
	find . -name '*.sched' | while IFS= read -r line ; do
		echo "$line"
		./schedch.native -a "$line" > "$line".aout
		./schedch.native -s "$line" > "$line".sout
		./schedch.native -l "$line" > "$line".lout
	done
fi


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
	find . -type f -name '*.ll' -exec rm {} +
	find . -type f -name '*.s' -exec rm {} +
	find . -type f -name '*.exe' -exec rm {} +
fi

make clean
