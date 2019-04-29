#define DEFAULT_VERB_FILE "./finverbs.new" /* For Unix */
/* For my Windows it was "D:\\InetPub\\scripts\\conjugat\\finverbs.new" */

#define stricmp  strcasecmp  /* For Unix */
#define strnicmp strncasecmp


/* CONJTEST.CPP  --   Copyright (C) (1993-1997) by Antti Karttunen
   Contains the "main" routine that calls for module CONJUGAT.CPP
   which contains the appropriate C++ routines for inflecting Finnish verbs.

   CHANGES:
   2-JAN-1997   Added CGI-functionality. Can be called from the Web now.


   19-FEB-2003  Changed '{' -> 'ä' = \344, '|' -> 'ö' = \366.

   07-MAR-2003  Commented out few incorrect conjugations (when using -t option)

   Compile as:

#!/bin/sh
gcc -c -o wildcard.o wildcard.cpp
gcc -c -o conjtest.o conjtest.cpp
gcc -c -o conjugat.o conjugat.cpp
gcc -o conjugat.cgi conjugat.o conjtest.o wildcard.o


 */

typedef unsigned char Uchar;
typedef unsigned int Uint;
typedef int (*PFUC)(Uchar); /* Pointer to function returning Uchar. */

#define toUstr(X) ((Uchar*) (X))
#define toStr(X) ((char *) (X))

#include "conjugat.h"

extern "C"
 {
#include "stdio.h"
#include "stdlib.h"
#include "ctype.h"
 };
#include "string.h"

#define progname "conjugat"

extern int wildcard(Uchar *pattern,Uchar *string);

#define MAXBUF 82

char buf[MAXBUF+3];

#define END_MASK 077777


struct Texts_and_Masks
 {
   char *Text;
   Uint Mask;
 };

struct Texts_and_Masks t_and_m[] =
 {
   "PRESENT INDICATIVE",    (PRESENT_INDICATIVE),
   "IMPERFECT INDICATIVE",  (IMPERFECT+INDICATIVE),
   "PERFECT INDICATIVE",    (PERFECT+INDICATIVE),
   "PLUPERFECT INDICATIVE", (PLUPERFECT+INDICATIVE),
   "PRESENT IMPERATIVE",    (PRESENT+IMPERATIVE),
   "PERFECT IMPERATIVE",    (PERFECT+IMPERATIVE),
   "PRESENT CONDITIONAL",   (PRESENT+CONDITIONAL),
   "PERFECT CONDITIONAL",   (PERFECT+CONDITIONAL),
   "PRESENT POTENTIAL",     (PRESENT+POTENTIAL),
   "PERFECT POTENTIAL",     (PERFECT+POTENTIAL),
   NULL,                    END_MASK
 };

unsigned int masks1[] =
 {
   (PERS1+SINGPERS), (PERS2+SINGPERS), (PERS3+SINGPERS),
   (PERS1+PLURPERS), (PERS2+PLURPERS), (PERS3+PLURPERS),
   PASSIVE,
   END_MASK
 };

unsigned int masks2[] = { 002,046,026,016,0246,040,036, END_MASK };

unsigned int masks3[] =
 {
   NOMINATIVE,
   GENITIVE,
   PARTITIVE,
   ACCUSATIVE,
   TRANSLATIVE,
   ESSIVE,
   INESSIVE,
   ELATIVE,
   ILLATIVE,
   ADESSIVE,
   ABLATIVE,
   ALLATIVE,
   ABESSIVE,
   INSTRUCTIVE,
   COMITATIVE,
   PROLATIVE,
   END_MASK
 };

char *casenames[] =
 {
   "NOMINATIVE",
   "GENITIVE",
   "PARTITIVE",
   "ACCUSATIVE",
   "TRANSLATIVE",
   "ESSIVE",
   "INESSIVE",
   "ELATIVE",
   "ILLATIVE",
   "ADESSIVE",
   "ABLATIVE",
   "ALLATIVE",
   "ABESSIVE",
   "INSTRUCTIVE",
   "COMITATIVE",
   "PROLATIVE",
   NULL
 };

static Uchar stem[MAXBUF+3];
static Uchar result[MAXBUF+3],infinitive[MAXBUF+3];

#define ONE_FROM_EACH 99


Uchar cgi_option=0; /* If used via the Web. */
Uchar case_option=0,special_option=0,table_option=0,only_option=0;

void print_n_spaces(int n)
{
   while(n--) { putchar(' '); }
}


/* When cgi_option is not on, this prints out the string just as it is.
   However, if the program is used via Web, this will convert all the
   old seven-bit ascii representations of the Finnish diacritic letters
   a with dots and o with dots to the corresponding ISO-8859/1 values.
   fputs'es printing out the corresponding HTML-entities &auml; and &ouml;
   have now been commented out, because raw ISO-8859/1 will work also
   with text/plain output mode.
 */
void HTML_print(Uchar *s)
{
    if(!cgi_option) { fputs(toStr(s),stdout); }
    else
     {
       while(*s)
        {
          switch(*s)
           {
             case '{':  /* %E4 = ä */
              { putc('\344',stdout); /* fputs("&auml;",stdout); */ break; }
             case '|':  /* %F6 = ö */
              { putc('\366',stdout); /* fputs("&ouml;",stdout); */ break; }
             case '[':  /* %C4 = Ä */
              { putc('\304',stdout); /* fputs("&AUML;",stdout); */ break; }
             case '\\': /* %D6 = Ö */
              { putc('\326',stdout); /* fputs("&OUML;",stdout); */ break; }
             default: { putc(*s,stdout); break; }
           }
          s++;
        }
     }
}

void print_cases(Uchar *stem,Uint type,Uint flags)
{
       char **casnamptr = casenames;
       Uint *maskptr = masks3;
       int len;

       printf("\n");
       for(;*casnamptr;casnamptr++,maskptr++)
        {
          printf("%-16s",*casnamptr);
          conjugate(result,stem,type,((*maskptr+flags)+SINGCASE));
          /* printf("%-32s",result); */
          HTML_print(result);
          len = strlen(toStr(result));
          for(len = (32-len);len>0;len--) { putc(' ',stdout); }
          conjugate(result,stem,type,((*maskptr+flags)+PLURCASE));
          HTML_print(result); printf("\n");
        }
       printf("\n");
}

void print_row(Uchar *stem,Uint type,Uint flags,Uint *masks)
{
       Uint persmask;

       persmask = *masks++;
       goto creep_in;
       while(1)
        {
          persmask = *masks++;
          if(persmask == END_MASK) { break; }
          printf(", ");
creep_in:
          conjugate(result,stem,type,flags|persmask);
          HTML_print(result);
        }
       printf("\n");
}


void print_table(Uchar *stem,Uint type,Uint *masks)
{
    struct Texts_and_Masks *tmptr = t_and_m;

    printf("\n");
    while(tmptr->Text)
     {
       printf("%s\n", tmptr->Text);
       printf("  "); print_row(stem,type,(tmptr->Mask+AFFIRMATIVE),masks);
       printf("  "); print_row(stem,type,(tmptr->Mask+NEGATIVE),masks);
       tmptr++;
     }
    printf("\n");
}

void print_NF_table(Uchar *stem,Uint type)
{
    printf("\n");
    printf("FIRST INFINITIVE\n  SHORT FORM: ");
    HTML_print(conjugate(result,stem,type,INFINITIVE1));
    printf("    LONG FORM: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE1+PS_NI)));
    printf("\nSECOND INFINITIVE\n  INESSIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE2+INESSIVE)));
    printf("   INSTRUCTIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE2+INSTRUCTIVE)));
    printf("  INESSIVE PASSIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE2+PASSIVE)));
    printf("\nTHIRD INFINITIVE\n  ILLATIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE3+ILLATIVE)));
    printf("   INESSIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE3+INESSIVE)));
    printf("  ELATIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE3+ELATIVE)));
    printf("\n  ADESSIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE3+ADESSIVE)));
    printf("  ABESSIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE3+ABESSIVE)));
    printf("  INSTRUCTIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE3+INSTRUCTIVE)));
    printf("\nFOURTH INFINITIVE\n  NOMINATIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE4+NOMINATIVE)));
    printf("   PARTITIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE4+PARTITIVE)));
    printf("\nFIFTH INFINITIVE: ");
    HTML_print(conjugate(result,stem,type,(INFINITIVE5+PS_NI)));
    printf("\nPRESENT PARTICIPLES:\n  ACTIVE: ");
    HTML_print(conjugate(result,stem,type,PART1ACT));
    printf("  (gen. ");
    HTML_print(conjugate(result,stem,type,(PART1ACT+GENITIVE)));
    printf(")   PASSIVE: ");
    HTML_print(conjugate(result,stem,type,PART1PAS));
    printf("   (gen. ");
    HTML_print(conjugate(result,stem,type,(PART1PAS+GENITIVE)));
    printf(")\nPAST PARTICIPLES:\n  ACTIVE: ");
    HTML_print(conjugate(result,stem,type,PART2ACT));
/* Gives still incorrect results:
    printf(" (gen. ");
    HTML_print(conjugate(result,stem,type,(PART2ACT+GENITIVE)));
    printf(")");
 */
/* Put instead some fodder between: */
    print_n_spaces(strlen((char *) conjugate(result,stem,type,(PART1ACT+GENITIVE)))+8);

    printf("   PASSIVE: ");
    HTML_print(conjugate(result,stem,type,PART2PAS));
/* Gives still incorrect results:
    printf("   (gen. ");
    HTML_print(conjugate(result,stem,type,(PART2PAS+GENITIVE)));
    printf(")");
 */
    
    printf("\nAGENTIAL NOMINAL:\n  NOUN: ");
    HTML_print(conjugate(result,stem,type,AGENTIAL_NOUN));
    printf(" (pl.gen. ");
    HTML_print(conjugate(result,stem,type,(AGENTIAL_NOUN+PLURCASE+GENITIVE)));
    printf(")  NEGATIVE ADJECTIVE: ");
    HTML_print(conjugate(result,stem,type,NEGATIVE_ADJ));
/* Gives still incorrect results:
    printf("   (pl.gen. ");
    HTML_print(conjugate(result,stem,type,(NEGATIVE_ADJ+PLURCASE+GENITIVE)));
    printf(")");
 */
    printf("\nABSTRACT NOUN (a guess): ");
    HTML_print(conjugate(result,stem,type,ABSTRACT_NOUN));

    printf("\n\n\n");
}


void usage(void)
{
     fprintf(stderr,
"Usage: %s [-c] [-o] [-s] [-t] finverbs.new [pattern] [mask1 ...]\n",
       progname);
     fprintf(stderr,
"Use pattern *ita to select verbs whose infinitive ends with letters \"ita\".\n");
     fprintf(stderr,
"Use pattern -17 to select verbs of the classification type 17 (NSS class 11)\n");
     fprintf(stderr,
"Use +%u to select one case (the first) from each classification type.\n",
        ONE_FROM_EACH);
     fprintf(stderr,
"Options:\n");
     fprintf(stderr,
" -t  To print out the full inflection table of each selected verb.\n");
     fprintf(stderr,
" -s  Forms with possessive suffixes.\n");
     fprintf(stderr,
" -o  To print out ONLY the form corresponding to the mask specified.\n");
     fprintf(stderr,
" -c  For inflecting nominal cases. Use only if you give a mask also.\n");
     fprintf(stderr,
"         E.g. use the mask 0x%3x to get agential adjective\n",
                                          INFINITIVE3);
     fprintf(stderr,
"                           0x%3x (to get infinitive no. 4)\n",
                                          INFINITIVE4);
     fprintf(stderr,
"                           0x%3x (to get Present Active Participle)\n",
                                          PART1ACT);
     fprintf(stderr,
"                           0x%3x (to get Present Passive Participle)\n",
                                          PART1PAS);
     fprintf(stderr,
"                           0x%3x (to get Perfect (Past) Active Participle)\n",
                                          PART2ACT);
     fprintf(stderr,
"                           0x%3x (to get Perfect (Past) Passive Participle)\n",
                                          PART2PAS);
     fprintf(stderr,
"                           0x%3x (to get agential noun)\n",
                                          AGENTIAL_NOUN);
     fprintf(stderr,
"                           0x%3x (to get negative adjective)\n",
                                          NEGATIVE_ADJ);
     fprintf(stderr,
"                           0x%3x (to get abstract noun)\n",
                                          ABSTRACT_NOUN);
     fprintf(stderr,
"Add %d to these masks (e.g. 0x%3x) to get 3rd person possessive suffixes.\n",
              PERS3,(ABSTRACT_NOUN+PERS3));
     fprintf(stderr,
"Note that the nominal inflection (with -c option) has not been fully and\n");
     fprintf(stderr,
"correctly implemented than just for a few of the non-finite forms.\n");
     fprintf(stderr,
"Many of the abstract nouns are erroneous (actually just wild guesses).\n");
     exit(1);
}

Uchar *parse_url_value(Uchar *url_piece)
{
    Uchar *s,*t;
    Uchar save_char;
    unsigned int tmp;

    s = t = url_piece;

    while(*s)
     {
       switch(*s)
        {
          case '+': { *t++ = ' '; s++; break; } /* Plus signs to spaces */
          case '%':
           {
             if(strlen((char *)s) >= 3) /* There are at least two chars more */
              {
                save_char = *(s+3);
                *(s+3) = '\0';
                /* Convert two hex digits to int */
                sscanf(((const char *)s+1),"%x",&tmp);
                *(s+3) = save_char;
                *t++ = tmp;
                s += 3;
              }
             else { *t++ = *s++; } /* Copy the percent sign literally. */
             break;
           }
          default:  { *t++ = *s++; break; }
        }
     }

    *t = '\0';
    return(url_piece);
}


int parse_url_query_string(Uchar **pattern_result,unsigned int *min_class,
                                                  unsigned int *max_class,
                                                  Uchar *query_string)
{
    Uchar *ptr1,*ptr2,*ptr3;
    unsigned int mask=0,mask2=0;

    ptr1 = query_string;
    while(ptr1)
     {
       if(!(ptr2 = toUstr(strchr(((char *)ptr1),'=')))) { break; }
       if((ptr3 = toUstr(strchr(((char *)ptr2),'&'))))
        { *ptr3++ = '\0'; } /* Overwrite the ampersand, and skip past. */
       *ptr2++ = '\0'; /* Overwrite the equal sign, and skip past. */
/* ptr1 points to the beginning of the variable name.
   ptr2 points two characters past the end of the variable name, i.e. one
    past the equal sign (=) which has been overwritten by zero '\0',
    that is to the beginning of the variable value, corresponding to
    variable name where ptr1 points to.
   ptr3 points two characters past the end of variable value, i.e. one
    past the ampersand (&) which has been overwritten by zero, that is
    to the beginning of the next variable name. Or if we have found the
    last name=value pair, then it contains the NULL.
 */
       if(!strnicmp(((char *)ptr1),"mask",4)) /* Mask1, mask2, MASK3, ... */
        {
          Uchar *s = parse_url_value(toUstr(ptr2));
          if(isdigit(*s))
           {
             if(*(s+1) == 'x') { sscanf(toStr((s+2)),"%x",&mask2); }
             else { sscanf(toStr(s),"%o",&mask2); }
             mask |= mask2;
           }
        }
       else if(!stricmp(((char *)ptr1),"pattern"))
        {
/* If we get just pattern=&mask_f=something&... then don't store anything*/
          if(*ptr2) /* ... to pattern_result. */
           { *pattern_result = parse_url_value(toUstr(ptr2)); }
        }
       else if(!stricmp(((char *)ptr1),"classes"))
        {
          unsigned int classval;
          classval = atoi(toStr(parse_url_value(toUstr(ptr2))));
          if(!*min_class || (classval < *min_class))
           { *min_class = classval; }
          if(classval > *max_class) { *max_class = classval; }
        }
       else if(!stricmp(((char *)ptr1),"options"))
        {
          Uchar *s;
          for(s=parse_url_value(toUstr(ptr2)); *s; s++)
           {
             switch(tolower(*s))
              {
                case 'c': { case_option = 1; break; }
                case 'o': { only_option = 1; break; }
                case 's': { special_option = 1; break; }
                case 't': { table_option = 1; break; }
                case 'u':
                case '?': { usage(); exit(0); }
                default: { break; }
              }
           }
        }
       ptr1 = ptr3;
     }
    fflush(stdout);

    return(mask);
}


void main(Uint argc,Uchar **argv)
{
    FILE *verbfp=NULL;
    unsigned int mask=0,mask2,type=0,prevtype=0,wasneg=0;
    unsigned int min_class=0,max_class=0;
    int i,only_type=0;
    Uchar *pattern=NULL,*filename=NULL;
    Uchar *s;

    if(s = toUstr(getenv("QUERY_STRING"))) /* There is a query string? */
     {
       cgi_option = 1;
       printf("Content-Type: text/plain\r\n\r\n");
/* Note that parse_url_query_string puts to pattern the pointer to
   a substring of s, returned by getenv. If I remember right, getenv
   returns pointers to static string buffer, so if you call getenv second
   time, its result will overwrite the old s, and probably also the pattern
 */
       mask = parse_url_query_string(&pattern,&min_class,&max_class,s);

       printf("Selecting all verbs");
       if(pattern)
        { printf(" with PATTERN="); HTML_print(pattern); }
       if(min_class == ONE_FROM_EACH)
        { printf(", but only the first verb from each class"); }
       else if(min_class)
        { printf(" only from the class %d",min_class); }
       if(case_option){ printf(" (with option -c)"); }
       if(only_option){ printf(" (with option -o)"); }
       if(special_option){ printf(" (with option -s)"); }
       if(table_option){ printf(" (with option -t)"); }
       printf("\n\n\n");

/* Ranges like min_class - max_class not implemented yet. */
       if(min_class) { only_type = min_class; wasneg = 1; }
     }
    else /* The original command-line usage. */
     {
       if(argc < 2) { usage(); }

       while(s = *++argv)
        {
          if((*s == '-') || (*s == '+'))
           {
             switch(*(s+1))
              {
                case 'c': { case_option = 1; break; }
                case 'o': { only_option = 1; break; }
                case 's': { special_option = 1; break; }
                case 't': { table_option = 1; break; }
                case '0': case '1': case '2': case '3': case '4':
                case '5': case '6': case '7': case '8': case '9':
                 {  
                   only_type = atoi(toStr(s));
                   if(only_type < 0) { only_type = -only_type; wasneg = 1; }
                   break;
                 }
                default:
                 {
                   fprintf(stderr,"%s: unknown option: %s !\n",progname,s);
                   usage();
                 }
              }
           }
          else /* Not option. */
           {
             if(isdigit(*s))
              {
                if(*(s+1) == 'x') { sscanf(toStr((s+2)),"%x",&mask2); }
                else { sscanf(toStr(s),"%o",&mask2); }
                mask |= mask2;
              }
             else
              {
                if(!filename) { filename = s; }
                else { pattern = s; }
              }
           }
        } /* while */
     } /* else it was a command line usage */

    if(!filename)
     {
       if(!cgi_option) { usage(); }
       else { filename = toUstr(DEFAULT_VERB_FILE) ; }
     }
    if(!(verbfp = fopen(toStr(filename),"r")))
     {
       fprintf(stderr,"%s: Cannot open verb file: %s\n",progname,filename);
       exit(1);
     }

    while(fgets(buf,MAXBUF,verbfp))
     {
       prevtype = type;
       if(isdigit(*buf))
        {
          sscanf(toStr(buf),"%d %s",&type,stem);
          *infinitive = '\0';
        }
       else
        {
          sscanf(toStr(buf),"%s %d %s",infinitive,&type,stem);
        }
       if(only_type)
        {
          if((only_type == ONE_FROM_EACH))
           { if(type == prevtype) { continue; } }
          else if((wasneg ? (type != only_type) : (type < only_type)))
           { continue; }
        }
       conjugate(result,stem,type,INFINITIVE1);
       if(pattern && !wildcard(pattern,result)) { continue; }
       if(*infinitive && strcmp(toStr(infinitive),toStr(result)))
        {
          fprintf(stderr,
"**ERROR IN VERBFILE %s: produced infinitive %s!=%s (given one)\n",
             filename,result,infinitive);
        }
       HTML_print(result);
       printf(" %-2d (%d)",type,get_nssclass(type));
       if(table_option)
        {
          print_table(stem,type,masks1);
          print_NF_table(stem,type);
        }
       else if(case_option) { print_cases(stem,type,mask); }
       else if(only_option) // Only with those flags that were requested.
        {
          conjugate(result,stem,type,mask);
          printf(" ");
          HTML_print(result);
          printf("\n");
        }
       else
        {
          i = (8 - strlen(toStr(result)));
          do { putchar(' '); } while(i-- > 0);
          print_row(stem,type,mask,(special_option ? masks2 : masks1));
        }
     }
}

