all: opt was val

opt: dead-functions-elimination2.scm
	bigloo dead-functions-elimination2.scm -o opt

was: was.o leb128.o opcodes.sch
	bigloo was.o leb128.o -o was

val: validate.o type-abbreviations.sch numtypes.sch vectypes.sch instruction-types.sch
	bigloo validate.o -o val

validate.o: validate.scm type-abbreviations.sch numtypes.sch vectypes.sch instruction-types.sch
	bigloo -c validate.scm -o validate.o -O3 -unsafe

%.o : %.scm
	bigloo -c $< -o $@ -O2

rapport.pdf:
	latexmk -pdf rapport.tex

clean:
	rm -f *.o
	rm -f was opt val
