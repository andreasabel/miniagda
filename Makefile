# Makefile for miniagda

files=Main Lexer Parser Polarity Abstract ScopeChecker TypeChecker Value TCM Eval Termination SPos Concrete Warshall Util TreeShapedOrder TraceError Extract HsSyntax ToHaskell PrettyTCM Semiring SparseMatrix
# RETIRED: Completness
hsfiles=$(foreach file,$(files),$(file).hs)
ghcflags=-fglasgow-exts -ignore-package monads-fd -rtsopts
optflags=
# -O
profflags=-prof -auto-all
distfiles=*.hs *.hs-boot Lexer.x Parser.y Makefile 
distdirs=test/succeed test/fail examples

cabalp=cabal install -p --enable-executable-profiling

.PHONY : test examples current default clean veryclean

default : Main test

#current : miniagda-prof
#	miniagda-prof privateExamples/NisseContNorm/negative-2010-11-23.ma +RTS -prof
current : Main
#	Main test/fail/BoundedFake.ma
	Main test/features/Existential/StreamProcCase.ma
#	Main test/features/Existential/list.ma
#	Main test/features/Existential/nat.ma
#	Main examples/RBTree/RBTreeConor.ma
#	Main test/fail/InvalidField.ma
#	Main test/succeed/BuiltinSigma.ma
#	Main test/features/records.ma
#	Main test/succeed/MeasuredHerSubst2.ma
#	Main examples/Coinductive/SubjectReductionProblem.ma
#	Main examples/Sized/Maximum.ma
#	Main examples/Irrelevance/Vector.ma
#	Main examples/JeffTerellCoqClub20100120.ma
#	Main examples/HugoCantor/tryLoopInjData.ma
#	Main examples/HugoCantor/InjDataLoop.ma
#	Main test/features/countConstructors.ma
#	Main examples/HugoCantor/injectiveData.ma
#	Main examples/BoveCapretta/Eval.ma # vec.ma # examples/List.ma
#	Main test/fail/OverlappingPatternIndFam.ma # vec.ma # examples/List.ma

# ship : ../dist/miniagda-2009-07-03.tgz # 06-27.tgz
# 
# ../dist/%.tgz : $(distfiles)
# 	tar czf $@ $^ $(distdirs)
# 

miniagda-prof : Main.hs $(hsfiles)
	ghc $(ghcflags) $(profflags) $< --make -o $@

Main : Main.hs $(hsfiles)
	ghc $(ghcflags) $(optflags) $< --make -o $@

install-prof-libs :
	$(cabalp) transformers
	$(cabalp) mtl
	$(cabalp) syb
	$(cabalp) parsec
	$(cabalp) preprocessor-tools
	$(cabalp) cpphs
	$(cabalp) haskell-src-exts

SCT : SCT.hs Lexer.hs SCTParser.hs SCTSyntax.hs
	ghc $(ghcflags) $< --make -o $@

Lexer.hs : Lexer.x
	alex $<

%arser.hs : %arser.y Lexer.hs
	happy --info=$<-grm.txt $<

test : Main         
	@echo "======================================================================"
	@echo "===================== Suite of successfull tests ====================="
	@echo "======================================================================"
	make -C test/succeed
	@echo "======================================================================"
	@echo "======================= Suite of failing tests ======================="
	@echo "======================================================================"
	make -C test/fail

examples : Main
	@echo "======================================================================"
	@echo "========================== Suite of examples ========================="
	@echo "======================================================================"
	make -C examples


clean : 
	-rm *.o *.hi Main miniagda-prof
# 	make -C test/fail clean

veryclean : clean
	make -C test/fail clean

# EOF
