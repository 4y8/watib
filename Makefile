all: opt was val

opt: dead-functions-elimination2.scm
	bigloo dead-functions-elimination2.scm -o opt

was: was.o leb128.o opcodes.sch
	bigloo was.o leb128.o -o was

val: validate.o type-abbreviations.sch numtypes.sch vectypes.sch instruction-types.sch
	bigloo validate.o -o val

%.o : %.scm
	bigloo -c $< -o $@ -O2 -unsafe -g

clean:
	rm -f *.o
	rm -f was opt val
