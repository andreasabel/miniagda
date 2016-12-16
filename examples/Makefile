# MiniAgda
# Makefile for successful tests
# Authors: Andreas Abel, Ulf Norell
# Created: 2004-12-03, 2008-09-03

mugda = miniagda

# Getting all miniagda files

# files that do not type check
exclude= \
  ./Coinductive/destructor-patterns \
  ./Coinductive/Oury \
  ./Gcd/gcd-continuation \
  ./HerSubst/spine \
  ./HenningBasold/NonProductive_Endrullis \
  ./HugoCantor/InjDataLoop \
  ./HugoCantor/injectiveData-em-as-arg \
  ./HugoCantor/injectiveData \
  ./HugoCantor/tryLoopInjData \
  ./Nisse/coSetOmega \
  ./Nisse/RecNat \
  ./Nisse/relabeling-inductive \
  ./Nisse/ThueMorse \
  ./Polarity/universe \
  ./Sized/Tree \
  ./Sized/Inflationary \
# end (files that do not check)

allagda=$(filter-out $(exclude),$(patsubst %.ma,%,$(shell find . -name "*.ma")))

all : $(allagda)

$(exclude) : % : %.ma
	@echo "----------------------------------------------------------------------"
	@echo "Skipping " $<
	@echo "----------------------------------------------------------------------"

$(allagda) : % : %.ma
	@echo "----------------------------------------------------------------------"
	@echo $<
	@echo "----------------------------------------------------------------------"
	@$(mugda) $<

debug :
	@echo $(allagda)

clean :
	-rm *~

#EOF
