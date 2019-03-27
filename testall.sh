make clean
make

./schedch -a tests/create.sched > tests/create.sched.out.tmp
./schedch -a tests/insert_statement_parsing/_smoke/insert.sched > tests/insert_statement_parsing/_smoke/insert.sched.out.tmp
cmp --silent tests/create.sched.output tests/create.sched.out.tmp && echo '### SUCCESS: Files Are Identical! ###' &&
cmp --silent tests/insert_statement_parsing/_smoke/insert.sched.out.tmp tests/insert_statement_parsing/_smoke/insert.sched.out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'

make clean
