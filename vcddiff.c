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

#define VERS2 "0.04c-veripool"
#define OFDT "07/28/2004"

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <ctype.h>
#include <time.h>
#include <string.h>
#include <assert.h>
#include "vcddiff.h"

static int get_token(FILE *, char *);
static void read_to_end(FILE *);
static char timescale(FILE *, int *);
static void add_signal(char *, char *, unsigned int, int, int);
static int get_var_type(char *);
static void variable(FILE *, char *);
static void alloc_sig_mem(void);
static int get_vkeywrd(register char *);
static long get_lines(FILE *, int *, int *, char *);
static void print_signal_info(struct signal_t *, struct signal_t *, vtime_t,
		vtime_t, bool_t);
static void print_scalar_state(struct signal_t *, struct signal_t *, vtime_t,
                vtime_t, char, char, bool_t);
static void print_vector_state(struct signal_t *, struct signal_t *, vtime_t,
                vtime_t, char *, char *, bool_t);
static void print_scalar_edge(struct signal_t *, struct signal_t *, vtime_t,
                vtime_t, char, char, bool_t);
static void print_edges(struct signal_t *, char *, int, int, char);
static void print_vector_edge(struct signal_t *, struct signal_t *, vtime_t,
                vtime_t, char *, char *, bool_t);
static int get_nxt_chg(FILE *, char *, int *, int *, char *, vtime_t *,
		         bool_t);
static vtime_t get_time_diffs(FILE *, FILE *, char *, char *, long, long,
	          vtime_t, vtime_t, bool_t);
static void print_map(void);
static void compare_types(char *, char *, struct signal_t *, struct signal_t *);
static bool_t map(char *, char *, int *, struct signal_t *,
                   struct signal_t *, struct signal_t **, bool_t);
static bool_t map_var_names(char *, char *);
static void run_diffs(FILE *, FILE *, char *, char *, long, long);
static void print_header(void);
static void print_help(void);
static void edge_space(int, int , bool_t);

/* globals */
static int line_num1G;   /* cur position of file1 */
static int line_num2G;  /* cur position of file2 */
static char curmodG[1000]; /* cur mod hier name */
static FILE *file1G;   /* to check if it is the first file */
static bool_t state_flagG; /* print edges or states */
static bool_t wrap_flagG; /* print edges or states */
static struct signal_t	**sig_int1G;  /* int codes for file 1 */
static struct signal_t	**sig_int2G;	/* int codes for file 2 */
static int *fd1_to_fd2_mapG;   /* mappings from one file to the other*/
static int *fd2_to_fd1_mapG;
static int max_codeG;         /* max code so sig_int? is large enough */
static char scopesG[MAXSCOPES][MAXSIG];  /* scope of mods */
static int  quit_flagG;    /* flag to quit */
static bool_t  next_listG;    /* to signal the next list of signals */
struct signal_t *sig1_hdG;   /* the head of the first file of signals */
struct signal_t *sig2_hdG;   /* the head of the second file of signals */
struct signal_t *lastsigG;   /* mark the last signal of the file */

/* signal to code from dinotrace wave viewer www.veripool.org/dinotrace */
/*extended vcd converted, removes '<' */
#define	VERILOG_ID_TO_POS(_code_) \
    strpbrk(_code_, "<" ) ? atoi(_code_+1) : \
    (_code_[0]?((_code_[0]-32) + 94 * \
		(_code_[1]?((_code_[1]-32) + 94 * \
			    (_code_[2]?((_code_[2]-32) + 94 * \
					(_code_[3]?((_code_[3]-32) \
						    ):0) \
					):0) \
			    ):0) \
		):0)

#define VERILOG_POS_TO_SIG1(_pos_) \
    (((unsigned)(_pos_)<(unsigned)max_codeG)?sig_int1G[(_pos_)]:0)

#define VERILOG_POS_TO_SIG2(_pos_) \
    (((unsigned)(_pos_)<(unsigned)max_codeG)?sig_int2G[(_pos_)]:0)
/**********************************************************************/

/*get a new token from the file fp, and place in token, return the length*/
static int get_token(FILE *fp, char *token)
{
 int i, c;

 i = 0;
 while((c = fgetc(fp)) != EOF)
 {
	 if(isspace(c))
	 {
           if(c == '\n')
           {
	     if(fp == file1G) line_num1G++;
	     else line_num2G++;
	   }
		 continue;
         }
	 break;
 }

 if(c == EOF) return EOF;
 token[i++] = c;

 while(!isspace(c = fgetc(fp)) && i < MAXTOKSIZE)
 {
   if(c == EOF)
 	 return EOF;
   else
     token[i++] = c;
 }

 if(i>=MAXTOKSIZE-1){
   printf("Token too long.\n");
   exit(1);
 }

 if(c == '\n')
 {
    if(fp == file1G) line_num1G++;
    else line_num2G++;
 }

 token[i]='\0';
 return i;
}

/*read the file until an $end is reached*/
static void read_to_end(FILE *fp)
{
  static char token[MAXTOKSIZE];

    while(get_token(fp, token) != EOF)
    {
	if(!strncmp (token, "$end", 4))
	    return;
    }
}

/* get the timescale information for comparison return char of units, and
 * the number 1, 10, 100 in tnum
 * */
static char timescale(FILE *fp, int *tnum)
{
 int i, toklen;
 static char token[MAXTOKSIZE*2]; //Needs to be able to hold 2 tokens because of the concatenation.
 static char tmp[MAXTOKSIZE];
 char *tok;

    toklen = get_token(fp, token);
    if(toklen==EOF){
      printf("*** ERROR Invalid timescale specification.\n");
      exit(1);
    }
    assert(toklen < MAXTOKSIZE);

    tok = token;
    /* AIV 02/04/03 there can be a space between 1 ns, not always 1ns */
    for(i = 0; i <toklen; i++)
    {
      if(!isdigit(token[i])) break;
    }
    if(i == toklen)
    {
      get_token(fp, tmp);
      strcat(token, tmp);
    }

    *tnum = atoi(tok);

    if(*tnum != 1 && *tnum != 10 && *tnum != 100)
    {
	    printf("*** ERROR-time number(%d) in timescale on line %d is illegal - assuming 1\n", *tnum, (fp == file1G) ? line_num1G : line_num2G );
	    *tnum = 1;
    }

    while(isdigit (*tok))
	    tok++;
    if (!*tok)
	    get_token(fp, token);

    switch (*tok) {
    case 's':
    case 'm':
    case 'u':
    case 'n':
    case 'p':
    case 'f':
      return(*tok);
    default:
	  printf("*** ERROR-illegal character(%c) in timescale on line %d\n", *tok, (fp == file1G) ? line_num1G : line_num2G );
	  return(0);
    }
}

/* add a new signal to the list, init values signal name, identifies
 * the code to index the array of signals, and the number of bits, if
 * bits is 1 it is scalar
 * */
static void add_signal(char *signame,char *ident, unsigned int code,
                                   int bits, int type)
{
 char  *cp;
 struct signal_t *newsig;
   /*get rid of scapes before and after*/
   while(isspace(*signame)) signame++;
   for (cp = signame + strlen(signame) - 1;cp >= signame && isspace(*cp);  cp--)    *cp = '\0';

   newsig = (struct signal_t *) malloc(sizeof(struct signal_t));
   newsig->sig_code = code;
   newsig->ident = (char *) malloc(strlen(ident)+1);
   /* init values to x's */
   newsig->state = '?';
   newsig->type = type;
   newsig->in_both = TRUE;
   newsig->size = bits;
   newsig->next = NULL;

   if(type == REAL)
   {
     /*FIXME what about large reals ???*/
     newsig->vector = (char *) malloc(REALSIZE);
     newsig->vector[0] = '0';
     newsig->vector[1] = '\0';
   }
   else if(bits > 1)
   {
     newsig->vector = (char *) malloc(bits + 2);
     newsig->vector[bits] = '\0';
     bits--;
     for(; bits >= 0; bits--)
       newsig->vector[bits] = '?';
   }else{
     newsig->vector = NULL;
   }

   /*signal not found so print first diff*/
   newsig->found = FALSE;
   strcpy(newsig->ident, ident);
   /*link signals for hash setup and mapping*/
   if(lastsigG)
      lastsigG->next= newsig;

   if(sig1_hdG == NULL)
      sig1_hdG = newsig;

   if(next_listG && sig2_hdG == NULL)
      sig2_hdG = newsig;

   lastsigG = newsig;
   newsig->signame = (char *) malloc(strlen(signame)+1);
   strcpy(newsig->signame, signame);
   max_codeG = MAX((unsigned)max_codeG, code+1);
}


/*dump variable types must be kept in alphabetical order */
/*Values must match the var type #defines in vcddiff.h */
static struct variable_types_t var_types[] = {
        {"bit", BIT},  // GTKWave's fst2vcd
        {"byte", BYTE},  // GTKWave's fst2vcd
        {"event", EVENT },
        {"int", INT},  // GTKWave's fst2vcd
        {"integer", INTEGER},
        {"logic", LOGIC},  // GTKWave's fst2vcd
        {"longint", LONGINT},  // GTKWave's fst2vcd
        {"parameter", PARAMETER},
        {"port", PORT},  // GTKWave's fst2vcd
        {"real", REAL},
        {"real_parameter", REAL},  // GTKWave's fst2vcd, handle as if real
        {"realtime", REALTIME},  // GTKWave's fst2vcd
        {"reg", REG},
        {"shortint", SHORTINT},  // GTKWave's fst2vcd
        {"supply0", SUPPLY0},
        {"supply1", SUPPLY1},
        {"time", TIME},
        {"tri", TRI},
        {"tri0", TRI0},
        {"tri1", TRI1},
        {"triand", TRIAND},
        {"trior", TRIOR},
        {"trireg", TRIREG},
        {"wand", WAND},
        {"wire", WIRE},
        {"wor", WOR}
};


#define VARTYPE (sizeof(var_types) / sizeof(struct variable_types_t))

static int get_var_type(char *tstr)
{
 int l, h;
 register int m, cv;


 l = 0; h = VARTYPE - 1;
 for (;;)
  {
   m = (l + h)/2;
   if((cv = strcmp(var_types[m].vnam, tstr)) == 0)
      return(var_types[m].vnum);
   if(cv < 0) l = m + 1; else h = m - 1;
   if(h < l) break;
  }
 return(-1);

}

/*var line so add a new signal with its identifier*/
static void variable(FILE *fp, char *file_name)
{
  char signame[MAXSIG];
  char ident[MAXIDLENGTH];
  static char token[MAXTOKSIZE];
  int  bits;
  char type;

    /*type, size*/
    {
      int len=get_token(fp, token);
      if(len==EOF){
        printf("ERROR - EOF during declaration of variable.  Aborting.\n");
        exit(1);
      }
    }
    /*most ofter a reg otherwise do a search */
    if(strncmp(token, "reg", 3) == 0)
	    type = REG;
    else
            type = get_var_type(token);

    if(type < 0 || type>UNDEFINED)
    {
      type = UNDEFINED;
      printf("Error- Unknown variable type %s\n", token);
    }

    /* get number of bits */
    /* AIV FIXME error recovery should be used here for invalid tokens
     * if a $var line is short vars (starts with a '$') rewind, print
     * message and return */
    {
      int len=get_token(fp, token);
      if(len==EOF){
        printf("ERROR - EOF during declaration of variable.  Aborting.\n");
        exit(1);
      }
    }
    bits = atoi(token);

    if(bits < 0)
    {
       printf ("Negative size illegal on line %d of %s\n", (fp == file1G) ? line_num1G : line_num2G, file_name);
       return;
    }

    /*identifier*/
    get_token(fp, token);

    bzero(ident,sizeof(ident)); //ensure that ident is null-terminated, even it is too long to fit in ident
    strncpy(ident, token, sizeof(ident)-1);

    for(char *s=token;*s;s++){
      if( !isprint(*s)){
        printf("*** ERROR-illegal character(%c) in signal name on line %d\n", *s, (fp == file1G) ? line_num1G : line_num2G );
        exit(1);
      }
    }

    /*variable name*/
    get_token(fp, token);
    bzero(signame,MAXSIG);
    strncpy(signame, curmodG, MAXSIG-1);
    {
	    int available=MAXSIG-1-strlen(signame);
	    if(available>0){
		    strncat(signame, token, available);
	    }
    }

    get_token(fp, token);
    /*if there is a space between the var name and the range concat it
     * to the name */
    if (token[0] != '$'){
	int available=MAXSIG-1-strlen(signame);
	if(available>0){
		strncat (signame, token, available);
	}
    }

    add_signal(signame, ident, VERILOG_ID_TO_POS(ident), bits, type);
}


/* setup memory for the hash of signals, which is the max of all signals,
 * then setup up arrays according to the signal's code index
 */
static void alloc_sig_mem(void)
{
  struct signal_t *sig;

  sig_int1G = (struct signal_t **) calloc(sizeof(struct signal_t *), (max_codeG + 1));
  for (sig = sig1_hdG; sig !=NULL; sig = sig->next)
     sig_int1G[sig->sig_code] = sig;

  sig_int2G = (struct signal_t **) calloc(sizeof(struct signal_t *), (max_codeG + 1));
  for (sig = sig2_hdG; sig !=NULL; sig = sig->next)
     sig_int2G[sig->sig_code] = sig;

}

/*dump file keywords must be kept in alphabetical order */
static struct variable_types_t vkeywds[] = {
        { "attrbegin", V_ATTRBEGIN},  // GTKWave
        { "comment", V_COMMENT},
        { "date", V_DATE },
        { "end", V_END },
        { "enddefinitions", V_ENDDEF},
        { "scope", V_SCOPE},
        { "timescale", V_TIMESCALE },
        { "upscope", V_UPSCOPE },
        { "var", V_VAR },
        { "version", V_VERSION }
};

#define VKEYWDS (sizeof(vkeywds) / sizeof(struct variable_types_t))

/*search for verilog keywords*/
static int get_vkeywrd(register char *tstr)
{
 int l, h;
 register int m, cv;

 //start at $end
 l = -2; h = VKEYWDS - 1;
 for (;;)
  {
   m = (l + h)/2;
   if(m<0) return EOF;
   if((cv = strcmp(vkeywds[m].vnam, tstr)) == 0)
        return(vkeywds[m].vnum);
   if(cv < 0) l = m + 1; else h = m - 1;
   if(h < l) break;
  }
 return(EOF);
}


/* process all the lines until $enddef is reached, determines all variable
 * names, seperated by a '.' between scopes, place the timescale number
 * and units, and return the file location to start processing diffs
 */
static long get_lines(FILE *fp, int *units, int *tnum, char *file_name)
{
  char sep[2];
  int level;
  register int i;
  char *tok;

  //1+because of the line with "tok++", which would otherwise make the buffer
  //smaller than expected by get_token
  static char token[MAXTOKSIZE+1];

    sep[0] = '.';
    sep[1] = '\0';
    level = 0;
    *units = 1;
    *tnum=1;

    while(get_token(fp, token) != EOF)
    {
      tok = token;
      if(tok[0] != '$')
      {
        printf ("***ERROR Unknown token '%s' on line %d of %s\n",
           tok, (fp == file1G) ? line_num1G: line_num2G, file_name);
        continue;
      }
      tok++;

      switch(get_vkeywrd(tok)){
        case V_END:
                break;

	case V_VAR:
		 variable(fp, file_name);
	      break;

        case V_UPSCOPE:
	         if(level > 0) level--;
	      break;

	case V_SCOPE:
	    get_token(fp, tok);
	    get_token(fp, tok);

	    if(level < MAXSCOPES)
            {
		strcpy(scopesG[level], tok);

	        strcpy(curmodG, scopesG[0]);
	        strcat(curmodG, sep);
		level++;

		if(level)
		{
                  for (i=1; i<level; i++) {
	             strcat(curmodG, scopesG[i]);
	             strcat(curmodG, sep);
                  }
	        }

	    }
	    else
            {
	       printf("*** ERROR-exceeded max scope levels %d\n", level);
	       exit(0);
	    }
          break;
	case V_ATTRBEGIN:
        case V_COMMENT:
	case V_DATE:
	case V_VERSION:
	       read_to_end(fp);
	     break;

        case V_TIMESCALE:
		 *units = timescale(fp, tnum);
 		 break;

        case V_ENDDEF:
	    next_listG = TRUE;
	    lastsigG = NULL;
	    return(ftell(fp));

	default:
	    printf ("Unknown command '%s' on line %d of %s\n",
		     tok, (fp == file1G) ? line_num1G: line_num2G, file_name);
	    break;

    }
  }
    return(-1);
}

/* print the diff signal information */
static void print_signal_info(struct signal_t *sig1,
                              struct signal_t *sig2,
                              vtime_t time1, vtime_t time2, bool_t isone)
{

   if(time1 == time2)
   {

      if(sig1->sig_code == sig2->sig_code)
          printf("%s (%s) differs at time %lld\n", sig1->signame,sig1->ident, time1);
      else
          printf("%s (%s , %s) differs at time %lld\n", sig1->signame,
                sig1->ident, sig2->ident, time1);
     return;
    }
   else
   {
       if(sig1->sig_code == sig2->sig_code)
              printf("%c %s (%s) at time %lld next occurence at time %lld\n", isone ? '>' : '<',  sig1->signame, sig1->ident, time1, time2);
       else
              printf("%c %s (%s, %s) at time %lld next occurence at time %lld\n", isone ? '>' : '<',  sig1->signame,sig1->ident, sig2->ident, time1, time2);
     return;
   }

}



/* print the scalar value  */
static void print_scalar_state(struct signal_t *sig1,
                               struct signal_t *sig2,
                               vtime_t time1, vtime_t time2,
                               char state1, char state2, bool_t isone)
{

   if(time1 == time2)
   {
      sig1->found = TRUE;
      sig2->found = TRUE;
      if(state1 != state2)
      {
         print_signal_info(sig1, sig2, time1, time2, isone);
         printf("\t%c\n\t%c\n",state1, state2);
	 return;
      }
   }
  else
  {
         print_signal_info(sig1, sig2, time1, time2, isone);
         printf("%c #%lld \t%c\n", isone ? '>' : '<', time1, state1);
	 printf("%c #%lld \t%c\n", isone ? '>' : '<', time2, state2);
	      return;
     }
}


/* print the vector value  */
static void print_vector_state(struct signal_t *sig1,
                               struct signal_t *sig2,
                               vtime_t time1, vtime_t time2,
                               char *sval1, char *sval2, bool_t isone)
{
 int size1, size2;
 register int i;

 size1 = strlen(sval1);
 size2 = strlen(sval2);
 if(time1 == time2)
 {
    sig1->found = TRUE;
    sig2->found = TRUE;
   if(strcmp(sval1, sval2) != 0)
   {
       print_signal_info(sig1, sig2, time1, time2, isone);
       if(size1 == size2)
         printf("\t%s\n\t%s\n", sval1, sval2);
       else
       {
         if(size1 > size2)
         {
           printf("\t%s\n\t", sval1);
           for(i = 0; i < size1 - size2; i++)
               putc(' ', stdout);
           printf("%s\n", sval2);
         }
         else
         {
           printf("\t");
           for(i = 0; i < size2-size1; i++)
               putc(' ', stdout);
           printf("%s\n\t%s\n", sval1, sval2);
         }
       }
       return;
      }
   }
  else
  {
     print_signal_info(sig1, sig2, time1, time2, isone);
     if(size1 == size2)
     {
       printf("%c #%lld \t%s\n", isone ? '>' : '<', time1, sval1);
       printf("%c #%lld \t%s\n", isone ? '>' : '<', time2, sval2);
     }
     else
     {
         if(size1 > size2)
         {
           printf("%c #%lld \t%s\n", isone ? '>' : '<', time1, sval1);
           printf("%c #%lld \t", isone ? '>' : '<', time2);
           for(i = 0; i < size1 - size2; i++)
               putc(' ', stdout);
           printf("%s\n", sval2);
         }
         else
         {
           printf("%c #%lld \t", isone ? '>' : '<', time1);
           for(i = 0; i < size2-size1; i++)
               putc(' ', stdout);
           printf("%s\n", sval1);
           printf("%c #%lld \t%s\n", isone ? '>' : '<', time2, sval2);
         }
     }
   }
}



/* print scalar edge value  */
static void print_scalar_edge(struct signal_t *sig1,
                              struct signal_t *sig2,
                              vtime_t time1, vtime_t time2,
                              char state1, char state2, bool_t isone)
{

        if(time1 == time2)
        {
          sig1->found = TRUE;
          sig2->found = TRUE;
          if(state1 != state2)
          {
            print_signal_info(sig1, sig2, time1, time2, isone);

	    if(sig1->state == state1)
               printf("\t(%c-)\n", state1);
	    else
               printf("\t(%c%c)\n", sig1->state, state1);
	    if(sig2->state == state2)
               printf("\t(%c-)\n", state2);
	    else
               printf("\t(%c%c)\n", sig2->state, state2);
	    return;
	  }
	 }
         else
	 {
           print_signal_info(sig1, sig2, time1, time2, isone);

           if(sig1->state == state1)
              printf("%c #%lld \t(%c-)\n", isone ? '>' : '<', time1, state1);
           else
              printf("%c #%lld \t(%c%c)\n", isone ? '>' : '<', time1, sig1->state, state1);
           if(sig2->state == state2)
              printf("%c #%lld \t(%c-)\n", isone ? '>' : '<', time2, state2);
           else
              printf("%c #%lld \t(%c%c)\n", isone ? '>' : '<', time2, sig2->state, state2);
           return;
        }
}

/* print the edge values
  sig - signal pointer
  sval - sval string value
  str_size - size of sval
  search - '<' or '>' if a search was performed otherwise 'n' for none
  this is needed for wrapping of the line
*/
static void print_edges(struct signal_t *sig, char *sval, int str_size,
                        int vec_size, char searchc)
{
 register int i;
 int min, tprint;

 /*AIV 02/06/03 the print edges were wrong for different sizes */
 /* for example, b10 is now b101 at the next time */
 /* vector is the old value and sval is the new */

 if(str_size > vec_size)
	 min = vec_size;
 else
	 min = str_size;

 tprint = 0;
 for(i=0; i < min; i++, tprint++)
 {
   if(sig->vector[i] == sval[i])
     printf(" (%c-)", sig->vector[i]);
   else
     printf(" (%c%c)", sig->vector[i], sval[i]);

   /* only want to print 11 edges per line */
     if(wrap_flagG && tprint != 0 && !(tprint % EDGE_PER_LINE))
     {
        if(searchc != 'n')
          printf(" \\\n%c\t", searchc);
        else
          printf(" \\\n\t");
     }
 }
 if(str_size == vec_size) return;

 if(min == vec_size)
 {
   for(i = min; i < str_size; i++, tprint++)
   {
      printf(" (?%c)", sval[i]);
      if(wrap_flagG && tprint != 0 && !(tprint % EDGE_PER_LINE))
      {
        if(searchc != 'n')
          printf(" \\\n%c\t", searchc);
        else
          printf(" \\\n\t");
      }
   }
 }
 else
 {
   /* if the first value is a '?', no need to print the rest
    * since it can only be '?' on the first value change */
   if(sig->vector[0] != '?')
   {
     for(i = min; i < vec_size; i++, tprint++)
     {
      printf(" (%c?)", sig->vector[i]);
      if(wrap_flagG && tprint != 0 && !(tprint % EDGE_PER_LINE))
      {
        if(searchc != 'n')
          printf(" \\\n%c\t", searchc);
        else
          printf(" \\\n\t");
      }
     }
   }
 }

}

/* add space so the vector edge valued line up correctly */
static void edge_space(int size1, int size2, bool_t is_sigone)
{
 register int i, r, diff;

 if(size1 == size2) return;
 if(is_sigone && size2 > size1)
 {
    diff = (size2 - size1) * CHAR_PER_EDGE;
    if(wrap_flagG)
    {
      r = diff / EDGE_PER_LINE;
      if(r > 0)
      {
        r = (r * EDGE_PER_LINE);
        diff -= r;
        if(diff <= CHAR_PER_EDGE) diff = 0;
      }
    }
    for(i=0; i < diff; i++)
         putc(' ', stdout);
 }
 if(!is_sigone && size1 > size2)
 {
     diff = (size1 - size2) * CHAR_PER_EDGE;
     if(wrap_flagG)
     {
       r = diff / EDGE_PER_LINE;
       if(r > 0)
       {
        r = (r * EDGE_PER_LINE);
        diff -= r;
        if(diff <= CHAR_PER_EDGE) diff = 0;
       }
     }
     for(i=0; i < diff; i++)
          putc(' ', stdout);
 }

}




/* print vector edge */
static void print_vector_edge(struct signal_t *sig1,
                              struct signal_t *sig2,
                              vtime_t time1, vtime_t time2,
                              char *sval1, char *sval2, bool_t isone)
{
 int size1, size2;
 int vsize1, vsize2;
 int max1, max2;

   if(time1 == time2)
   {
       sig1->found = TRUE;
       sig2->found = TRUE;

       if(strcmp(sval1, sval2) != 0)
	{

	  if(sig1->type == REAL)
	  {
            print_vector_state(sig1, sig2, time1, time2, sval1, sval2, isone);
	    return;
	  }

	  print_signal_info(sig1, sig2, time1, time2, isone);
          printf("\t");

          size1 = strlen(sval1);
          vsize1 = sig1->vector?strlen(sig1->vector):0;
          max1 = MAX(size1, vsize1);

          size2 = strlen(sval2);
          vsize2 = sig2->vector?strlen(sig2->vector):0;
          max2 = MAX(size2, vsize2);

          edge_space(max1, max2, TRUE);
          print_edges(sig1, sval1, size1, vsize1, 'n');
          if(wrap_flagG && sig1->size > EDGE_PER_LINE)
             printf("\n\n\t");
          else
            printf("\n\t");
          edge_space(max1, max2, FALSE);
          print_edges(sig2, sval2, size2, vsize2, 'n');
          printf("\n");
	  return;
	}
   }
   else
   {
	 char dirc;

	 if(sig1->type == REAL)
	 {
            print_vector_state(sig1, sig2, time1, time2, sval1, sval2, isone);
	    return;
	 }
         print_signal_info(sig1, sig2, time1, time2, isone);
         dirc = isone ? '>' : '<';
         printf("%c #%lld \t", dirc, time1);

         size1 = strlen(sval1);
         vsize1 = sig1->vector?strlen(sig1->vector):0;
         max1 = MAX(size1, vsize1);

         size2 = strlen(sval2);
         vsize2 = sig2->vector?strlen(sig2->vector):0;
         max2 = MAX(size2, vsize2);

         edge_space(max1, max2, TRUE);
         print_edges(sig1, sval1, size1, vsize1, dirc);
         printf("\n%c #%lld \t", dirc, time2);
         edge_space(max1, max2, FALSE);
         print_edges(sig2, sval2, size2, vsize2, dirc);
         printf("\n");
	 return;
  }
}

/* get the next time change in the other file if there is one */
static int get_nxt_chg(FILE *fp, char *fname, int *sigcode, int *bit,
                       char *value, vtime_t *time, bool_t isone)
{
 char *line;
 char * separator;
 static char token[MAXTOKSIZE];

   while(get_token(fp, token) != EOF)
   {
      line = token;

      switch (*line++)
      {
        /* scalar cases */
	case '0':
	case '1':
	case 'z':
	case 'Z':
	case 'x':
	case 'X':
  
      /*check if it contains '<', if true parsing the eVCD*/
      separator =strpbrk(token, "<" ); // get the separator
      if (!separator)
      {
        printf("Unknown Identifier not found '%s' in file %d '%s' on line %d\n",
		  line, isone ? 1 : 2, fname,
	        isone ? line_num1G : line_num2G );
	     continue;
      } else  if(separator-line>1){
            /*it is a vector*/
            line--;
            get_token(fp, token);
            strncpy(value, line, separator-line);
      	    *sigcode = VERILOG_ID_TO_POS(separator);
      	    if(isone)
      	    {
      	       if(VERILOG_POS_TO_SIG1(*sigcode) == NULL)
      	       {
                       printf("Unknown Identifier '%s' in file %d '%s' on line %d\n",
      		        line, isone ? 1 : 2, fname,
      		        isone ? line_num1G : line_num2G );
      	       continue;
      	       }
      	    }
      	    else if(VERILOG_POS_TO_SIG2(*sigcode) == NULL)
      	    {
                       printf("Unknown Identifier '%s' in file %d '%s' on line %d\n",
      		        line, isone ? 1 : 2, fname,
      		        isone ? line_num1G : line_num2G );
      		 continue;
      	    }
      	    return(VECTOR);
        } 
        /*otherwise vcd*/
          *bit = *(line-1);
          *sigcode = VERILOG_ID_TO_POS(line);
         if(isone)
         {
           if(VERILOG_POS_TO_SIG1(*sigcode) == NULL)
	   {
             printf("Unknown Identifier '%s' in file %d '%s' on line %d\n",
		line, isone ? 1 : 2, fname,
	        isone ? line_num1G : line_num2G );
	     continue;
	   }
	 }
	 else if(VERILOG_POS_TO_SIG2(*sigcode) == NULL)
	 {
           printf("Unknown Identifier '%s' in file %d '%s' on line %d\n",
		line, isone ? 1 : 2, fname,
               isone ? line_num1G : line_num2G );
   	    continue;
	 }
	 return(SCALAR);
	 break;
        /* vector or real cases */
	case 'b':
	case 'r':
	    strncpy(value, line, MAXTOKSIZE);
	    get_token(fp, token);
	    *sigcode = VERILOG_ID_TO_POS(token);
	    if(isone)
	    {
	       if(VERILOG_POS_TO_SIG1(*sigcode) == NULL)
	       {
                 printf("Unknown Identifier '%s' in file %d '%s' on line %d\n",
		        line, isone ? 1 : 2, fname,
		        isone ? line_num1G : line_num2G );
	       continue;
	       }
	    }
	    else if(VERILOG_POS_TO_SIG2(*sigcode) == NULL)
	    {
                 printf("Unknown Identifier '%s' in file %d '%s' on line %d\n",
		        line, isone ? 1 : 2, fname,
		        isone ? line_num1G : line_num2G );
		 continue;
	    }
	    return(VECTOR);

        /* time change value */
	case '#':
	    *time = (vtime_t) atoll(line);
	    return(TIME);
	case '$':
	    /* AIV 02/03/03 need to read until $end for $comment */
	    if(!strcmp(line, "comment") || !strcmp(line, "dumpall"))
	       read_to_end(fp);
	    break;
        default:
	    line--;
            printf("***ERROR Unknown token '%s' in file %d '%s' on line %d\n",
	  	     line, isone ? 1 : 2, fname,
		     isone ? line_num1G : line_num2G );
	     /*FIXME shouldn't die here if possible */
	      exit(0);
            continue;
	  break;
	}
   }
   return(EOF);
}

/* used to place the string back on the fp to be used later */
static void restore(FILE *fp, char *str, int size)
{
 int i;
 char *cp;

 {
	int r=ungetc(' ', fp);
	assert(r!=EOF);
 }
 cp = &str[size-1];

 for(i =0; i< size; i++, cp--){
	 int r=ungetc(*cp, fp);
	 if(r==EOF){
		 //because ungetc implementations are not required to support
		 //more than one call of it in a row, this sort of usage is
		 //fundamentally flawed.
		 printf("*** ERROR Failed using ungetc.  Aborting.\n");
		 exit(1);
 	}
 }
}

/* return true if the next signal isn't the same, otherwise false */
static bool_t peek_nxt_sig(FILE *fp, int sigcode1, bool_t isone)
{
  static char tmp[MAXTOKSIZE], sig[MAXTOKSIZE];
  int size1, sigcode2;
  char *cp;

  size1 = get_token(fp, tmp);
  cp = tmp;

  if(tmp[0] == 'b')
  {
   int size2;
   size2 = get_token(fp, sig);
   restore(fp, sig, size2);
   restore(fp, tmp, size1);
   cp = sig;
  }
  else if(tmp[0] == '0' || tmp[0] == '1' || tmp[0] =='x' || tmp[0] =='z')
  {
      restore(fp, tmp, size1);
      cp++;
  }
  else
  {
   restore(fp, tmp, size1);
   return(TRUE);
  }

  /* map signal to the right sigcode */
  sigcode2 = VERILOG_ID_TO_POS(cp);

  /* LOOKATME can this ever happen ??*/
  if(sigcode2<0 || sigcode2 > max_codeG) return(TRUE);
  if(isone)
  {
    if(fd2_to_fd1_mapG[sigcode2] == -1) return(TRUE);
    sigcode2 = fd2_to_fd1_mapG[sigcode2];
  }
  else
  {
    if(fd1_to_fd2_mapG[sigcode2] == -1) return(TRUE);
    sigcode2 = fd1_to_fd2_mapG[sigcode2];
  }

  return(sigcode1 != sigcode2);
}

/* rewind to file and reset line count to the start of a #time */
static void rewind_time(FILE *fp, long seek, vtime_t *time,
  	                vtime_t treset, int *linenum, int lreset)
{
  *time = treset;
  fseek(fp, seek, SEEK_SET);
 /* reset the line counts */
  *linenum = lreset;
}

/* get the differences of a time block from #time to the next #time2 or EOF */

static vtime_t get_time_diffs(FILE *mainfp, FILE *otherfp, char *mname,
                           char *oname, long seek1, long seek2,
                           vtime_t ltime1, vtime_t ltime2, bool_t isone)
{
  static char svalue1[MAXTOKSIZE], svalue2[MAXTOKSIZE];
  int retval = 0;
  int sigcode1, sigcode2;
  int  state1='?', state2='?';
  int tmpline1, tmpline2;
  vtime_t time, nxt_time;
  struct signal_t *sig1, *sig2;
  bool_t first;

  sigcode1 = sigcode2 = 0;
  first = TRUE;
  time = ltime1;
  nxt_time = ltime2;

  /* AIV 02/04/03 added temps to reset line count for unkown tokens, etc */
   tmpline1 = line_num1G;
   tmpline2 = line_num2G;


  /* LOOKATME rewind needed here because of reset ungets, and seek from
     the start of the file, should write own buffer  */
   rewind(isone ? mainfp : otherfp);
   if(isone)
     fseek(mainfp, seek1, SEEK_CUR);
   else
     fseek(otherfp, seek2, SEEK_CUR);

  while(retval != EOF)
  {

    retval = get_nxt_chg(mainfp, mname, &sigcode1, &state1, svalue1, &time, isone);
    switch(retval)
    {
      case SCALAR:
      case VECTOR:
      /* rewind if the next sig isn't the same */
      /* LOOKATME could change so that if the first sig isn't*/
      /* get_nxt would rewind, to speed up ?? */
      if(!first && peek_nxt_sig(otherfp, sigcode1, isone))
      {
          rewind_time(otherfp, seek2, &nxt_time, ltime2,
		      isone ? &line_num2G : &line_num1G,
		      isone ? tmpline2 : tmpline1);
      }

      first = FALSE;
      if(isone)
      {
        sig1 = VERILOG_POS_TO_SIG1(sigcode1);
        sigcode1 = fd1_to_fd2_mapG[sigcode1];
        sig2 = VERILOG_POS_TO_SIG2(sigcode1);
      }
      else
      {
        sig1 = VERILOG_POS_TO_SIG2(sigcode1);
        sigcode1 = fd2_to_fd1_mapG[sigcode1];
        sig2 = VERILOG_POS_TO_SIG1(sigcode1);
      }

      if(!sig1->in_both)
	    break;
      if(sig1->found)
      {
        sig1->found = FALSE;
        sig2->found = FALSE;
        if(retval == SCALAR) sig1->state = state1;
        else{
		if(sig1->vector){
	       		strcpy(sig1->vector, svalue1);
		}
	}
        break;
     }
     /*should check to see if sig1 and sig 2 are the same type??*/
     for(;;)
     {
       if(get_nxt_chg(otherfp, oname, &sigcode2, &state2, svalue2, &nxt_time, !isone) == EOF)
       {
          printf("%s (%s) Never found in file %d at time %lld\n", sig1->signame,
                     sig1->ident, isone ? 1 : 2, time);
         break;
       }
       else if(sigcode1 == sigcode2)
	        break;
      }

     if(retval == SCALAR)
     {
	if(state_flagG)
          print_scalar_state(sig1, sig2, time, nxt_time,
                                          state1, state2, isone);
	else
          print_scalar_edge(sig1, sig2, time, nxt_time,
                                          state1, state2, isone);
        sig1->state = state1;
     }
     else if(retval == VECTOR)
     {
        if(state_flagG)
          print_vector_state(sig1, sig2, time, nxt_time,
                                         svalue1, svalue2, isone);
	else
          print_vector_edge(sig1, sig2, time, nxt_time,
                                          svalue1, svalue2, isone);
	if(sig1->vector){
 	  strcpy(sig1->vector, svalue1);
	}
     }
    /* if isn't the same time must of scanned foward so rewind */
     if(nxt_time != time)
     {
        rewind_time(otherfp, seek2, &nxt_time, ltime2,
		      isone ? &line_num2G : &line_num1G,
		      isone ? tmpline2 : tmpline1);
     }

    break;
   case TIME:
      /* rewind to the start time, to process other file*/
     if(isone)
     {
         rewind_time(otherfp, seek2, &nxt_time, ltime2,
		      isone ? &line_num2G : &line_num1G,
		      isone ? tmpline2 : tmpline1);
     }
    if(time != 0)
       return(time);
    break;
  case EOF:
     quit_flagG = EOF;
     return(time);
  default:
	   continue;
	}
   }
  /*FIXME should never get here emit error */
  return(time);
}



/* print the map of identifiers to signal name between files */
static void print_map(void)
{
   register int i;
   struct signal_t *sig;

   for(i = 0; i< max_codeG; i++)
   {
    if(fd1_to_fd2_mapG[i] != -1)
    {
        sig = VERILOG_POS_TO_SIG2(fd1_to_fd2_mapG[i]);
  	if(!sig || !sig->in_both) continue;
	printf("%d %d %s\n", i, fd1_to_fd2_mapG[i], sig->signame);
    }
   }
   printf("*********Second*********\n");
   for(i = 0; i< max_codeG; i++)
   {
    if(fd2_to_fd1_mapG[i] != -1)
     {
        sig = VERILOG_POS_TO_SIG1(fd2_to_fd1_mapG[i]);
	if(!sig || !sig->in_both) continue;
	printf("%d %d %s\n", i, fd2_to_fd1_mapG[i], sig->signame);

     }
   }

}

static void print_var_type_name(int vnum){
  if(vnum<0 || vnum>=UNDEFINED){
    printf("UNDEFINED");
  }else{
    printf("'%s'",var_types[vnum].vnam);
  }
}

/* check to make sure the files are the same in type/size in both files */
static void compare_types(char *file_nam1, char *file_nam2,
                          struct signal_t *sig1, struct signal_t *sig2)
{

  if(sig1->in_both)
  {
    if(sig1->type != sig2->type)
    {
      if(sig1->sig_code == sig2->sig_code)
         printf("WARNING - Ignoring signal %s (%s) - types don't match\n",
                    sig1->signame, sig1->ident);
      else
         printf("WARNING - Ignoring signal %s (%s %s) - types don't match\n",
                    sig1->signame, sig1->ident, sig2->ident);

      printf("\t file '%s' type ", file_nam1);
      print_var_type_name(sig1->type);
      printf("\n");

      printf("\t file '%s' type ", file_nam2);
      print_var_type_name(sig2->type);
      printf("\n\n");

      sig1->in_both = FALSE;
      sig2->in_both = FALSE;
      return;
    }
    if(sig1->size != sig2->size)
    {
       if(sig1->sig_code == sig2->sig_code)
          printf("WARNING - Ignoring signal %s (%s) - sizes don't match\n",
                        sig1->signame, sig1->ident);
       else
          printf("WARNING - Ignoring signal %s (%s %s) - sizes don't match\n",
                        sig1->signame, sig1->ident, sig2->ident);

        printf("\t file '%s' size '%d'\n", file_nam1, sig1->size);
        printf("\t file '%s' size '%d'\n\n", file_nam2, sig2->size);
        sig1->in_both = FALSE;
        sig2->in_both = FALSE;
        return;
     }
  }
}

/* map the signal names to identifiers from one file to the other */
static bool_t map(char *file_nam1, char *file_nam2, int *file_map,
                     struct signal_t *startsig, struct signal_t *otherstart,
                     struct signal_t **sig_arr, bool_t isone)
{
  struct signal_t *sig1, *sig2;
  bool_t one_found;

  if (sig_arr) {}  // UNUSED

  one_found = FALSE;
  for(sig1 = startsig; sig1 != NULL; sig1 = sig1->next)
  {
     if(isone)
       sig2 = VERILOG_POS_TO_SIG2(sig1->sig_code);
     else
       sig2 = VERILOG_POS_TO_SIG1(sig1->sig_code);

    if(sig2 == NULL)
    {
       for(sig2 = otherstart; sig2 != NULL; sig2 = sig2->next)
       {
           if(!sig2->in_both) continue;
           if(strcmp(sig1->signame, sig2->signame) == 0)
           {
              file_map[sig1->sig_code] = sig2->sig_code;
              compare_types(file_nam1, file_nam2, sig1, sig2);
              one_found = TRUE;
              break;
           }
       }
    }
    else if(strcmp(sig1->signame, sig2->signame) == 0)
    {
        file_map[sig1->sig_code] = sig1->sig_code;
        compare_types(file_nam1, file_nam2, sig1, sig2);
        one_found = TRUE;
    }
    else
    {
       for(sig2 = otherstart; sig2 != NULL; sig2 = sig2->next)
       {
           if(!sig2->in_both) continue;
           if(strcmp(sig1->signame, sig2->signame) == 0)
           {
              file_map[sig1->sig_code] = sig2->sig_code;
              compare_types(file_nam1, file_nam2, sig1, sig2);
              one_found = TRUE;
              break;
           }
       }
    }
    if(sig2 == NULL)
    {
       printf("WARNING - Ignoring signal %s (%s) - not defined in both files\n", sig1->signame, sig1->ident);
       printf("\t  Defined in file '%s' and not file '%s'\n\n", file_nam1, file_nam2);
       file_map[sig1->sig_code] = sig1->sig_code;
       sig1->in_both = FALSE;
    }
 }

  return(one_found);
}

/* map both files */
static bool_t map_var_names(char *file_nam1, char *file_nam2)
{
  fd1_to_fd2_mapG = (int*) malloc(sizeof(int) * max_codeG);
  for(int i=0;i<max_codeG;i++) fd1_to_fd2_mapG[i]=-1;

  fd2_to_fd1_mapG = (int*) malloc(sizeof(int) * max_codeG);
  for(int i=0;i<max_codeG;i++) fd2_to_fd1_mapG[i]=-1;

  if (0) {
    print_map();
  }

  map(file_nam1, file_nam2, fd1_to_fd2_mapG, sig1_hdG, sig2_hdG, sig_int1G, TRUE);
  return(map(file_nam2, file_nam1, fd2_to_fd1_mapG, sig2_hdG, sig1_hdG, sig_int2G, FALSE));
}

/* acutally run the files to diff the two */
static void run_diffs(FILE *fp1, FILE *fp2, char *fnam1, char *fnam2,
                      long start1, long start2)
{
 vtime_t time1, time2, temp_time1;
 long temp1_chars;
 static char token[MAXTOKSIZE];

  time1 = time2 = temp_time1 = 0;

  while(quit_flagG != EOF)
  {

   /* while the times are the same */
   while(time1 == time2)
   {
     temp_time1 = time1;
     time1 =  get_time_diffs(fp1, fp2, fnam1, fnam2, start1, start2,
                              time1, time2, TRUE);
     temp1_chars = ftell(fp1);

     time2 =  get_time_diffs(fp2, fp1, fnam2, fnam1, start2, start1,
                              time2, temp_time1, FALSE);
     start2=ftell(fp2);
     start1=temp1_chars;
     if(quit_flagG) break;
   }

   /* time one is ahead */
    while(time2 < time1)
    {
     time2 = get_time_diffs(fp2, fp1, fnam2, fnam1, start2, start1,
                             time2, temp_time1, FALSE);
     start2 = ftell(fp2);
     if(quit_flagG)
     {
          if(get_token(fp1, token) != EOF)
	  {
		  printf("*****End of file hit at file 2 ('%s') time %lld quiting\n", fnam2, time2);
		  printf("WARNING - Files have different end times\n");
	  }
	  break;
     }
   }


   /* while time two is ahead */
    while(time1 < time2)
    {
     time1 =  get_time_diffs(fp1, fp2, fnam1, fnam2, start1, start2, time1, time2, TRUE);
     start1=ftell(fp1);
     if(quit_flagG)
     {
          if(get_token(fp2, token) != EOF)
	  {
		  printf("*****End of file hit at file 1 ('%s') time %lld quiting\n", fnam1, time1);
		  printf("WARNING - Files have different end times\n");
	  }
          break;
     }
    }
  }
}

/* print help information */
static void print_help(void)
{
  printf("Usage: [options] 'file1' 'file2'\n Compares Verilog VCD dump files.\n\n");

  printf(" --state \n -s \n");
  printf("\tPrints the current state of variables, instead of\n \tthe default edge value. \n");
  printf(" --wrap  \n -w \n");
  printf("\tWraps the line, only used for default edge print values\n\tnot --state print messages. \n");

  printf("\n");
  print_header();
  exit(0);
}


/* print program header info */
static void print_header(void)
{
 char s1[30];
 time_t now;

 printf("%s_%s of %s (%s).\n", "vcddiff", VERS2, OFDT, "Linux-ELF");
 printf("Copyright (c) 2002-2004 Pragmatic C Software Corp.\n");
 printf("All Rights reserved.  Licensed under the GNU General Public License (GPL).\n");
 printf("See the 'COPYING' file for details.  NO WARRANTY provided.\n");
 time(&now);
 strcpy(s1, ctime(&now));
 s1[24] = '\0';
 printf("Today is %s.\n\n", s1);

}

static void set_options(int argc, char **argv)
{
 int i;

 for(i = 1; i < argc-2; i++)
 {
   if(!strcmp(argv[i], "--state") || !strcmp(argv[i],"-s"))
   {
      if(wrap_flagG)
        printf("WARNING - wrap cannot be used with --state option, wrap disabled\n\n");
      else
       state_flagG = TRUE;
   }
   else if(!strcmp(argv[i], "--wrap") || !strcmp(argv[i],"-w"))
   {
      if(state_flagG)
        printf("WARNING - wrap cannot be used with --state option, wrap disabled\n\n");
      else
        wrap_flagG = TRUE;
   }
   else
   {
     printf("ERROR - Unknown option %s\n", argv[i]);
     printf("\t Try 'vcddiff --help' for more infomation\n");
     exit(0);
   }
 }

}

/*according to the std ieee verilog if it is port is an evcd (port not allowed in vcd)
no need to check dumpports or dumpvars
*/
int main(int argc, char **argv)
{
   int unit1, unit2, tnum1, tnum2;
   long start1, start2;
   char *file_nam1, *file_nam2;
   bool_t map_found;
   FILE *fp1, *fp2;

  quit_flagG = FALSE;
  sig1_hdG = NULL;
  sig2_hdG = NULL;

  //print_header();
   if(argc < 2)
   {
     printf("ERROR - Usage: [options] 'file1' 'file2'\n");
     printf("\t Try 'vcddiff --help' for more infomation\n");
     exit(1);
   }
   if(!strcmp(argv[1], "--help") || !strcmp(argv[1],"-h"))
      print_help();

   if(argc < 3)
   {
     printf("ERROR - Usage: [options] 'file1' 'file2'\n");
     printf("\t Try 'vcddiff --help' for more infomation\n");
     exit(1);
   }


   wrap_flagG = state_flagG = FALSE;

   if((fp1 = fopen(argv[argc-2], "r")) == NULL)
   {
     printf("*** ERROR-opening file %s\n", argv[argc-2]);
     exit(1);
   }

   /* set first file to the global pointer to check the line count */
   file1G = fp1;

   if((fp2 = fopen(argv[argc-1], "r")) == NULL)
   {
     printf("*** ERROR-opening file %s\n", argv[argc-1]);
     exit(1);
   }
   set_options(argc, argv);

   sig_int1G = NULL;
   sig_int2G = NULL;

   /*could skip copying the filename, just pass argv*/
   file_nam1 = (char*) malloc(strlen(argv[argc-2])+ 1);
   strcpy(file_nam1, argv[argc-2]);
   start1 = get_lines(fp1, &unit1, &tnum1, file_nam1);

   file_nam2 = (char *) malloc(strlen(argv[argc-1]) + 1);
   strcpy(file_nam2, argv[argc-1]);
   start2 = get_lines(fp2, &unit2, &tnum2, file_nam2);
   alloc_sig_mem();
   map_found = map_var_names(file_nam1, file_nam2);


  if(map_found)
  {
   if(tnum1 != tnum2)
   {
     printf("*** ERROR-dump files have different timescale time numbers '%d' in file 1, and '%d' in file 2\n", tnum1, tnum2);
     exit(1);
   }

   if(unit1 != unit2)
   {
     printf("*** ERROR-dump files have different time scale units '%cs' in file 1, and '%cs' in file 2\n", unit1, unit2);
     exit(1);
   }

    run_diffs(fp1, fp2, file_nam1, file_nam2, start1, start2);
  }
  else
     printf("*** ERROR-dump files have no matching variables to diff\n");

   /* free and close */
   free (sig_int1G);
   free (sig_int2G);
   free(fd1_to_fd2_mapG);
   free(fd2_to_fd1_mapG);
   fclose(fp1);
   fclose(fp2);
   return(0);

}
