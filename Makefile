# -*- coding:utf-8; mode:makefile -*-

all:
	@echo "Nothing do for all"

clean:
	$(RM) $(shell find . -name "*~") $(shell find . -name "\#*")
