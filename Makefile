# Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

SRCS = Misc/let-if.scm Opt/optimise.scm Val/validate.scm \
       Asm/binary.scm Opt/TestBr/walk.scm Opt/UnCast/walk.scm \
       Opt/Unreachable/walk.scm Opt/Const/walk.scm Opt/PureDrop/walk.scm \
       Opt/CopyProp/walk.scm Opt/Peephole/walk.scm Opt/PropType/walk.scm \
       Env/env.scm Ast/node.scm Misc/list.scm Type/type.scm Type/match.scm \
       Misc/parse.scm Asm/leb128.scm Val/instructions.scm Opt/CFG/node.scm \
       Opt/CFG/order.scm Opt/CFG/dominance.scm Opt/CFG/walk.scm \
       Opt/CFG/dump.scm Opt/CFG/read.scm Opt/BBV/walk.scm

OBJS = $(SRCS:.scm=.o)

FLAGS = -O2
#FLAGS = -O3 -unsafe

all: watib tools

tools: tools/wati-test

tools/wati-test: tools/wati-test.o $(OBJS)
	bigloo $(FLAGS) tools/wati-test.o $(OBJS) -o tools/wati-test

watib: watib.o $(OBJS)
	bigloo $(FLAGS) watib.o $(OBJS) -o watib

%.o : %.scm
	bigloo -srfi multijob -c $(FLAGS) $< -o $@

report.pdf: report/report.tex report/report.bib
	latexmk -pdf report/report.tex

slides.pdf: report/slides.tex
	latexmk -pdflua report/slides.tex

clean:
	latexmk -C report/report.tex
	rm -f report-blx.bib report.bbl
	rm -f $(OBJS) *.o *.wasm tools/*.o
	rm -f watib tools/wati-test
