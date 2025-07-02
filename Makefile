SRCS = leb128.scm watib.scm Opt/optimise.scm Val/validate.scm Bin/binary.scm Opt/TestBr/walk.scm Opt/UnCast/walk.scm Env/env.scm Ast/node.scm Misc/list.scm Type/type.scm Type/match.scm Misc/parse.scm

OBJS = $(SRCS:.scm=.o)

#FLAGS = -O2 -g
FLAGS = -O3 -unsafe

all: was watib

was: was.o leb128.o
	bigloo $(FLAGS) was.o leb128.o -o was

watib: $(OBJS)
	bigloo $(FLAGS) $(OBJS) -o watib

Val/validate.o: Val/validate.scm type-abbreviations.sch Val/numtypes.sch Val/vectypes.sch Val/instruction-types.sch Val/constant-instructions.sch Val/absheaptypes.sch
	bigloo -c $(FLAGS) Val/validate.scm -o Val/validate.o

%.o : %.scm
	bigloo -c $(FLAGS) $< -o $@

rapport.pdf: rapport.tex
	latexmk -pdf rapport.tex

clean:
	rm -f $(OBJS) *.o
	rm -f watib was
