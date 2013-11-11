# MiniAgda
# Makefile for failing tests
# Author: Andreas Abel
# Created: 2004-12-06, 2008-09-03

# How this file works
# ===================
#
# Whenever a .ma file is modified,
# a corresponding .err file is generated to save the model error message
# for this file.  When the test suite is processed the next time, e.g.,
# after some hacking on the MiniAgda implementation, the new error message
# is compared to the saved one.  If they do not match, this is considered
# an error.  Then one has to verify the new error message is actually the
# intended one (manually), and remove the .err file.

mugda=../../Main

# Enable read -n
SHELL=bash

# Getting all agda files
allagda=$(shell find . -name '*.ma')
allstems=$(patsubst %.ma,%,$(allagda))
allout=$(patsubst %.ma,%.err,$(allagda))

.PHONY : $(allstems)

default : all
all : $(allstems)

debug : 
	@echo $(allagda)

# No error recorded

$(allout) : %.err : %.ma
	@echo "----------------------------------------------------------------------"
	@echo "$*.ma"
	@echo "----------------------------------------------------------------------"
	@if $(mugda) $(shell if [ -e $*.flags ]; then cat $*.flags; fi) $< > $*.tmp; \
		then echo "Unexpected success"; rm -f $*.tmp; false; \
    else if [ -s $*.tmp ]; \
				 then sed -e "s/[^ ]*test.fail.//g" $*.tmp > $@; cat $@; rm -f $*.tmp; true; \
				 else rm -f $@ $*.tmp; false; \
				 fi; \
		fi

# Existing error


#				 echo `cat $*.err` > $*.tmp.2; \
#				 echo `cat $*.tmp` > $*.tmp.3; \

# NO WITH SPACES AFTER \ AT END OF LINE

$(allstems) : % : %.err
	@echo "----------------------------------------------------------------------"
	@echo "$*.ma"
	@echo "----------------------------------------------------------------------"
	@if $(mugda) $(shell if [ -e $*.flags ]; then cat $*.flags; fi) $*.ma \
		 > $*.tmp.2; \
		then echo "Unexpected success"; rm -f $*.tmp.2; false; \
    else sed -e "s/[^ ]*test.fail.//g" $*.tmp.2 > $*.tmp; \
				 echo `tail -1 $*.err` > $*.tmp.2; \
				 echo `tail -1 $*.tmp` > $*.tmp.3; \
				 true; \
		fi;
	@if cmp $*.tmp.2 $*.tmp.3; \
	   then if cmp $*.tmp $*.err; \
                   then rm -f $*.tmp $*.tmp.2 $*.tmp.3; true; \
                   else mv $*.tmp $*.err; \
                        rm -f $*.tmp.2 $*.tmp.3; true; \
                fi; \
	   else echo "== Old error ==="; \
		cat $*.err; \
		echo "== New error ==="; \
		cat $*.tmp; \
		/bin/echo -n "Accept new error [y/N]? "; \
		read -n 1; \
		echo ""; \
		if [ "fckShPrg$$REPLY" != "fckShPrgy"  ]; \
                  then echo "Keeping old error"; false; \
		  else echo "Replacing error, continuing..."; \
                    mv $*.tmp $*.err; \
		    rm -f $*.tmp.2 $*.tmp.3; true; \
                fi; \
	    fi

# CAUTION: NO SPACE AFTER \
# RETARDED!!!!!!!

#		echo rm -f $*.tmp; echo rm -f $*.tmp.2; \
#		false; 

# Clean

clean :
	-rm -f *.err *.tmp *.tmp.* *~ adm/*.err adm/*.tmp*

# EOF
