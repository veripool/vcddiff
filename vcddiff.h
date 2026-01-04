/* Copyright (c) 1991-2004 Pragmatic C Software Corp. */

/*
  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write to the Free Software Foundation, Inc.,
  59 Temple Place, Suite 330, Boston, MA, 02111-1307.
*/

#define TRUE 1
#define FALSE 0
#define MAXSIG 256
#define MAXSCOPES 100
#define MAXTOKSIZE (1024 * 1024 + 1024)
#define MAXIDLENGTH 11

#define EDGE_PER_LINE 11
#define CHAR_PER_EDGE 5

#define SCALAR 0
#define VECTOR 1
#define REALSIZE 60

/*var types*/
#define BIT 0
#define BYTE 1
#define EVENT 2
#define INT 3
#define INTEGER 4
#define LOGIC 5
#define LONGINT 6
#define PARAMETER 7
#define PORT 8
#define REAL 9
#define REAL_PARAMETER 10
#define REALTIME 11
#define REG 12
#define SHORTINT 13
#define STRING 14
#define SUPPLY0 15
#define SUPPLY1 16
#define TIME 17
#define TRI 18
#define TRI0 19
#define TRI1 20
#define TRIAND 21
#define TRIOR 22
#define TRIREG 23
#define WAND 24
#define WIRE 25
#define WOR 26

#define UNDEFINED 27

/*dumpfile key words*/
#define V_ATTRBEGIN 1
#define V_COMMENT 2
#define V_DATE 3
#define V_END 4
#define V_ENDDEF 5
#define V_SCOPE 6
#define V_TIMESCALE 7
#define V_UPSCOPE 8
#define V_VAR 9
#define V_VERSION 10

#define MAX(a, b) ((a > b) ? a : b)

typedef unsigned long long vtime_t;
typedef char bool_t;

struct variable_types_t {
   const char* vnam;
   int vnum;
};

struct signal_t {
   int size;
   int type;
   unsigned int sig_code;
   char state;
   char* vector;
   char* signame;
   char* ident;
   bool_t found;
   bool_t in_both;
   struct signal_t* next;
};
