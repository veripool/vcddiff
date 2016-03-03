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
#define MAXTOKSIZE 2048

#define EDGE_PER_LINE 11
#define CHAR_PER_EDGE 5

#define SCALAR 0
#define VECTOR 1
#define REALSIZE 60

/*var types*/
#define EVENT 0
#define INTEGER 1
#define PARAMETER 2
#define REAL 3
#define REG 4
#define SUPPLY0 5
#define SUPPLY1 6
#define TIME 7
#define TRI 8
#define TRI0 9
#define TRI1 10
#define TRIAND 11
#define TRIOR 12
#define TRIREG 13
#define WAND 14
#define WIRE 15
#define WOR 16
#define UNDEFINED 17


/*dumpfile key words*/
#define V_COMMENT 1
#define V_DATE 2
#define V_END  3
#define V_ENDDEF  4
#define V_SCOPE    5
#define V_TIMESCALE 6
#define V_UPSCOPE  7
#define V_VAR    8
#define V_VERSION  9

#  define MAX(a,b) ((a  > b ) ?  a  :  b )

typedef unsigned long long vtime_t;
typedef char bool_t;

struct variable_types_t {
         char *vnam;
         int vnum;
};

struct signal_t {
    int size;
    int type;
    unsigned int sig_code;
    char state;
    char *vector;
    char *signame;
    char *ident;
    bool_t found;
    bool_t in_both;
    struct signal_t *next;
};
