ARCHFLGS= 
OPTFLGS=
CFLAGS= $(ARCHFLGS) $(OPTFLGS) -O2
#CFLAGS= $(ARCHFLGS) -pipe $(OPTFLGS) -g -Wall
#CFLAGS= -pg -g -Wall
#CFLAGS= -g -Wall

CC = gcc
LIBS = -lm
POD2TEXT = pod2text

default: README vcddiff

vcddiff:	vcddiff.o
	$(CC) $(CFLAGS) $(OPTFLGS) vcddiff.o $(LIBS) -o vcddiff
	@echo "** Now copy ./vcddiff to your binary directory, e.g. 'cp -p vcddiff /usr/local/bin/vcddiff'"

vcddiff.o:	vcddiff.c vcddiff.h
	$(CC) $(CFLAGS) -c vcddiff.c

README: README.pod
	-rm -f $@
	$(POD2TEXT) --loose $< > $@

clean distclean:
	-rm -rf README vcddiff vcddiff.o
