SRCS = Val/validate.scm Opt/TestBr/walk.scm Env/env.scm Ast/node.scm Misc/list.scm Type/type.scm Type/match.scm Misc/parse.scm

OBJS = $(SRCS:.scm=.o)

all: was watib

was: was.o leb128.o
	bigloo $(FLAGS) was.o leb128.o -o was -O3 -unsafe

watib: $(OBJS)
	bigloo $(FLAGS) $(OBJS) -o watib -O3

Val/validate.o: Val/validate.scm type-abbreviations.sch Val/numtypes.sch Val/vectypes.sch Val/instruction-types.sch Val/constant-instructions.sch Val/absheaptypes.sch
	bigloo -c $(FLAGS) Val/validate.scm -o Val/validate.o -O3 -unsafe

%.o : %.scm
	bigloo -c $(FLAGS) $< -o $@ -O3 -unsafe

rapport.pdf: rapport.tex
	latexmk -pdf rapport.tex

clean:
	rm -f $(OBJS) *.o
	rm -f watib was
