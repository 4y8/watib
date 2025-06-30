all: opt was wal

opt: dead-functions-elimination2.scm
	bigloo dead-functions-elimination2.scm -o opt

was: was.o leb128.o opcodes.sch
	bigloo was.o leb128.o -o was

wal: Val/validate.o Ast/node.o
	bigloo Val/validate.o -o wal

Val/validate.o: Val/validate.scm type-abbreviations.sch numtypes.sch vectypes.sch instruction-types.sch
	bigloo -c Val/validate.scm -o Val/validate.o -O3 -unsafe

%.o : %.scm
	bigloo -c $< -o $@ -O2

rapport.pdf: rapport.tex
	latexmk -pdf rapport.tex

clean:
	rm -f *.o
	rm -f was opt val
