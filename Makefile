.PHONY: all build format edit demo clean

src?=0
dst?=9
graph?=graph7.txt

all: build

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/ftest.exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./ftest.exe graphs/${graph} $(src) $(dst) outfile.dot
	@echo "\n   🥁  RESULT (content of outfile.dot)  🥁\n"
	@cat outfile.dot
	@dot -Tsvg outfile.dot > outfile.svg
	

clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	dune clean
