all: was watib

was: was.o leb128.o opcodes.sch
	bigloo was.o leb128.o -o was -O3 -unsafe

watib: Val/validate.o Ast/node.o
	bigloo Val/validate.o -o watib -O3 -unsafe

Val/validate.o: Val/validate.scm type-abbreviations.sch Val/numtypes.sch Val/vectypes.sch Val/instruction-types.sch Val/constant-instructions.sch Val/absheaptypes.sch
	bigloo -c Val/validate.scm -o Val/validate.o -O3 -unsafe

%.o : %.scm
	bigloo -c $< -o $@ -O3 -unsafe

rapport.pdf: rapport.tex
	latexmk -pdf rapport.tex

clean:
	rm -f *.o
	rm -f was opt val
