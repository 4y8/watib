all: opt as val

opt: dead-functions-elimination2.scm
	bigloo dead-functions-elimination2.scm -o opt

as: as.o leb128.o opcodes.sch
	bigloo as.o leb128.o -o as

val: validate.o type-abbreviations.sch numtypes.sch vectypes.sch
	bigloo validate.o -o val

%.o : %.scm
	bigloo -c $< -o $@ -O2 -unsafe

clean:
	rm -f *.o
	rm -f as opt
