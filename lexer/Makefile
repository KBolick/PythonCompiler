all:
	raco exe lexer.rkt

lexer: lexer.rkt
	raco exe lexer.rkt

clean:
	rm lexer

test:
	./lexer < tests.txt > test_output.txt
