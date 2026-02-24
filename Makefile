ARCHFLGS ?=
OPTFLGS ?=
CFLAGS ?= $(ARCHFLGS) $(OPTFLGS) -O2
#CFLAGS= $(ARCHFLGS) -pipe $(OPTFLGS) -g -Wall
#CFLAGS= -pg -g -Wall
#CFLAGS= -g -Wall
CLANGFORMAT ?= clang-format-18
CLANGFORMAT_FLAGS ?= -i

CXX ?= g++
LIBS ?= -lm
POD2TEXT ?= pod2text

default: README vcddiff

vcddiff:	vcddiff.o
	$(CXX) $(CFLAGS) $(OPTFLGS) vcddiff.o $(LIBS) -o vcddiff
	@echo "** Now copy ./vcddiff to your binary directory, e.g. 'sudo cp -p vcddiff /usr/local/bin/vcddiff'"

vcddiff.o:	vcddiff.cpp vcddiff.h
	$(CXX) $(CFLAGS) -c vcddiff.cpp

README: README.pod
	-rm -f $@
	$(POD2TEXT) --loose $< > $@

clean distclean:
	-rm -rf README vcddiff vcddiff.o

format:
	$(CLANGFORMAT) $(CLANGFORMAT_FLAGS) *.cpp *.h

test:
	ci/test.sh
