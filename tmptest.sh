make clean
make

find . -name '*.sched' | while IFS= read -r line ; do
    echo "$line"
		./schedch.native -a "$line" > "$line".aout.tmp
		./schedch.native -s "$line" > "$line".sout.tmp
		./schedch.native -l "$line" > "$line".lout.tmp
done
