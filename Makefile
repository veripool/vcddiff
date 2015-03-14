ARCHFLGS= 
OPTFLGS=
CFLAGS= $(ARCHFLGS) $(OPTFLGS) -O2
#CFLAGS= $(ARCHFLGS) -pipe $(OPTFLGS) -g -Wall
#CFLAGS= -pg -g -Wall
#CFLAGS= -g -Wall

CC=gcc
LIBS=-lm

vcddiff:	vcddiff.o
	$(CC) $(CFLAGS) $(OPTFLGS) vcddiff.o $(LIBS) -o vcddiff
	rm vcddiff.o

vcddiff.o:	vcddiff.c vcddiff.h
	$(CC) $(CFLAGS) -c vcddiff.c
