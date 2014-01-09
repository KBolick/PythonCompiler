all: pytrans
	@echo "compiled"

lexer: lexer.rkt
	raco exe lexer.rkt

pyparse: pyparse.rkt python-ast.grm.sx
	raco exe pyparse.rkt

pytrans: pytrans.rkt derivative-parsers.rkt
	raco exe pytrans.rkt

lex: lexer
	@./lexer

parse: pyparse
	@./pyparse

trans: pytrans
	@./pytrans

part: lexer pyparse
	@./lexer | ./pyparse

run: lexer pyparse pytrans
	@./lexer | ./pyparse | ./pytrans

clean:
	rm -rf compiled pyparse sdiff lexer pytrans
	rm -f tests/*.py.out*
	rm -f tests/*.py.expected*

sdiff: sdiff.rkt
	raco exe sdiff.rkt

atest: lexer pyparse pytrans sdiff
	make sexptest

test: pytrans sdiff
	for i in tests/*.py; do printf "%s\n" "Testing: $$i"; ./lexer < $$i | ./pyparse | ./pytrans > $$i.out; cat hir-header.rkt $$i.expected > $$i.expected.rkt | cat hir-header.rkt $$i.out > $$i.out.rkt; racket $$i.expected.rkt > $$i.expected.txt; racket $$i.out.rkt > $$i.out.txt; diff -q $$i.expected.txt $$i.out.txt; done

answers:
	for i in tests/*.py; do if [ ! -f $$i.expected ]; then printf "%s\n" "Copying $$i"; scp $$i caprica:temp.py; ssh caprica "lexer < temp.py | pyparse | pytrans > temp.py.expected"; scp caprica:temp.py.expected $$i.expected; fi done

sextest: lexer pyparse pytrans sdiff
	for i in tests/*.py; do ./lexer < $$i | ./pyparse | ./pytrans > $$i.out; ./sdiff $$i.out $$i.expected; done
