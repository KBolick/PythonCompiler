all: pyparse
	@echo "compiled"

pyparse: pyparse.rkt python-ast.grm.sx
	raco exe pyparse.rkt

lex: lexer.rkt
	raco exe lexer.rkt

sdiff: sdiff.rkt
	raco exe sdiff.rkt

parse: pyparse lexer derivative-parsers.rkt
	@./lexer | ./pyparse

clean:
	rm pyparse lexer sdiff

test: pyparse lex sdiff
	for i in tests/*.py; do echo $$i; make parse < $$i > $$i.out; diff $$i.out $$i.expected; done
