make clean
make

./schedch tests/create.sched > tests/create.sched.out.tmp
cmp --silent tests/create.sched.output tests/create.sched.out.tmp && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
