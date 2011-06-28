# MiniAgda 
# Makefile for successful tests
# Authors: Andreas Abel, Ulf Norell
# Created: 2004-12-03, 2008-09-03

mugda = ../../Main

# Getting all miniagda files
allagda=$(patsubst %.ma,%,$(shell find . -name "*.ma"))

all : $(allagda) 

$(allagda) : % : %.ma
	@echo "----------------------------------------------------------------------"
	@echo $<
	@echo "----------------------------------------------------------------------"
	@$(mugda) $<

clean :
	-rm *~

#EOF
