make clean
make

success = '### SUCCESS: Files Are Identical! ###'
fail = '### WARNING: Files Are Different! ###'

find . -name '*.sched' | while IFS= read -r line ; do
    echo "$line"
		./schedch.native -a "$line" > "$line".aout.tmp
		cmp --silent "$line".aout.tmp "$line".aout && echo $line $success || echo $line $fail
		./schedch.native -s "$line" > "$line".sout.tmp
		cmp --silent "$line".sout.tmp "$line".sout && echo $line $success || echo $line $fail
		./schedch.native -l "$line" > "$line".lout.tmp
		cmp --silent "$line".lout.tmp "$line".lout && echo $line $success || echo $line $fail
done
