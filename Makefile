all: opt as

opt: dead-functions-elimination2.scm
	bigloo dead-functions-elimination2.scm -o opt

as: binary.o leb128.o
	bigloo binary.o leb128.o -o as

%.o : %.scm
	bigloo -c $< -o $@ -O2

clean:
	rm -f *.o
	rm -f as opt
