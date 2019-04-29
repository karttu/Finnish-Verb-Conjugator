
/*
;
;   This is the module coded by Antti Karttunen and used by many programs.
;   Following text applies to this module and to all other modules in this
;   package unless otherwise noted:
;
;   Copyright (C) 1991  Antti J. Karttunen
;
;   (wildcard is public domain, however, other modules in this directory
;    are PROPRIETARY, unless otherwise noted.)
*/

/*
   Pattern can contain following characters:

    ?   Matches one character which can be anything.
    *   Matches zero or more of any characters.

    <   Start of the "group-expression", which contains some chars,
         and must end in >
        If first char after < is ^ then its semantics are negated.
        If first char after < or ^ is > then it's not understood yet as end
         delimiter.
    Examples:
        <abc>          Matches any of the letters a, b and c.
        <^0123456789>  Matches anything, except digits.
        <>>            Matches >
        <^>>           Matches anything except >

    @   Matches character last matched to ? or group-expression.
         For example ?*@ matches to all strings which begin with same
         character they end.
        However, if pattern starts with @ then it sets the group
         start & end characters, e.g. pattern: @{tuu<ba{123}pasuuna
         matches to anything which begins tuu<ba then after that is
         1, 2 or 3 and after that pasuuna.

     Any other characters match just to themselves.

   Note that unix-like [0-9] (corresponding to <0123456789>) is not
   implemented yet.

   New shorthand patterns added 16-NOV-1993 for easier analyzing of
   Finnish words:

    V matches any lowercase finnish vowel, i.e. a, e, i, o, u, y, ä and ö
    C matches anything which above one doesn't match. (i.e. 'consonants').
    A matches to 'a' or 'ä'  (ä = a with dots).
    O matches to 'o' or 'ö'  (ö = o with dots).
    U matches to 'u' or 'y'.

*/

#define REGISTER register

typedef unsigned char Uchar;
typedef unsigned int Uint;
typedef int (*PFI)(Uchar); /* Pointer to function returning int. */

#define tostr(X) ((Uchar*) (X))

#ifdef AZTEC
#define strchr index
#endif

extern "C" Uchar *strchr(Uchar const *, Uchar);

#define VOWELS tostr("aeiouyäö")

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE  !FALSE
#endif


char GROUP_BEG_CHAR = '<';
char GROUP_END_CHAR = '>';

#define get_end_delimiter(C) (((C) == '(') ? ')' : ((C)+2))

unsigned char *string_org;
unsigned char last_matched=0;


/* Check whether pattern and string match, and returns 0 if they not.
   If they match then return integer whose high byte is the last character
   matched to ? or <> expression (if any), and whose low byte is
   index +2 to string, to that point which matched to the first
   non-* and non-? expression.

   Note that value of last_matched is updated in double-recursive
   function, although it is static variable.
   Intuitively, this should cause some problems, but I haven't yet
   discovered any pattern which would match incorrectly.
 */
int wildcard(Uchar *pattern,Uchar *string)
{
    Uint wild_aux(Uchar *pattern,Uchar *string,Uint fnwc);
    Uint i;

    last_matched = 0;
    string_org = string;
    if(*pattern == '@') /* Set group-expression delimiter characters. */
     { /* Set GROUP_BEG_CHAR to be char after @, and if it is not '\0' */
       if(GROUP_BEG_CHAR = *++pattern) { pattern++; } /* then skip it also. */
       GROUP_END_CHAR = get_end_delimiter(GROUP_BEG_CHAR);
     }

    i = wild_aux(pattern,string,1);
    return(i ? ((last_matched << 8)|i) : 0);

}


Uint wild_aux(Uchar *pattern,Uchar *string,Uint fnwc)
{
    int group_match(Uchar **pat_adr,Uchar **str_adr);

loop:

    if(!*pattern && !*string) /* if BOTH are in the end */
     { return(fnwc); }
    /* Asterisk may match emptiness at the end of string: */
    if((*pattern == '*') && !*string)
     { pattern++; goto loop; }
    if(!*pattern || !*string) return(FALSE); /* If only OTHER is in the end */
    if(*pattern == GROUP_BEG_CHAR)
     {
       if(group_match(&pattern,&string)) { goto jalava; }
       else { return(FALSE); }
     }
    if(*pattern == '?') /* Question-mark in pattern ? */
     { pattern++; last_matched = *string++; goto loop; }
/* New perversions at 16-NOV-1993: C,V,A,O and U shorthands */
    if((*pattern == 'C')) /* Is "consonant"? I.e. not vowel. */
     {
       if(!strchr(VOWELS,*string)) { last_matched = *string; goto silava; }
       else { goto sulava; }
     }
    if((*pattern == 'V'))
     {
       if(strchr(VOWELS,*string))  { last_matched = *string; goto silava; }
       else { goto sulava; }
     }
    if((*pattern == 'A'))
     {
       if(strchr(tostr("aä"),*string)) { last_matched = *string; goto silava; }
       else { goto sulava; }
     }
    if((*pattern == 'O'))
     {
       if(strchr(tostr("oö"),*string)) { last_matched = *string; goto silava; }
       else { goto sulava; }
     }
    if((*pattern == 'U'))
     {
       if(strchr(tostr("uy"),*string)) { last_matched = *string; goto silava; }
       else { goto sulava; }
     }
    if((*pattern == '@') && last_matched)
     {
       if(*string == last_matched) { goto silava; }
       else { goto sulava; }
     }
    if(*pattern == '*')
     {
       Uint muu,kuu;
       /* Save the value of last_matched at this level... */
       kuu = last_matched; /* if next level of recursion fucks it up. */
       if(muu = wild_aux(pattern,string+1,fnwc)) { return(muu); }
       last_matched = kuu; /* Restore value of last_matched at this level */ 
       return(wild_aux(pattern+1,string,fnwc));
#ifdef VANHA_PASKA
 /* It (*) matches several chars?: */
       return(wild_aux(pattern,string+1,fnwc)
               || /* Matches one character: (not really necessary)
              wild_aux(pattern+1,string+1,fnwc)
               ||  */             /* Or it matches 0 characters? */
              wild_aux(pattern+1,string,fnwc));
#endif
     }
    else sulava: if(*pattern == *string) /* Same characters ? */
     {
silava:
       pattern++; string++;
jalava:
       if(fnwc == 1) { fnwc = ((string - string_org)+1); }
       goto loop;
     }
    else { return(FALSE); }
}


int group_match(Uchar **pat_adr,Uchar **str_adr)
{
        REGISTER Uchar *pat;
        REGISTER Uchar c,positive_flag;

        /* Take current char. from string, and advance string by one: */
        c = *(*str_adr)++;
        pat = (*pat_adr)+1; /* Skip group beginning char */

/* positive_flag is on if there is no negation-sign (^) in the beginning: */
        if(*pat == '^') { positive_flag = 0; pat++; }
        else { positive_flag = 1; }

        while(*pat)
         {
           if(*pat == c) /* If found c from the pattern. */
            { /* If group ending char not found, then return false: */
              if(!(pat = strchr((pat+1),GROUP_END_CHAR))) { return(FALSE); }
              else
               {
                 /* Set pattern to point one after group_end_char: */
nakki:           *pat_adr = (pat+1);
                 if(positive_flag) /* Set last_matched char. */
                  { last_matched = c; }
                 return(positive_flag);
               }
            }
           if(*++pat == GROUP_END_CHAR)
            {
/* If there was negation-sign (^) in the beginning, meaning that
    positive_flag was 0, then set it to 1, and jump to nakki
    to return true result. Because we are here it means that
    c doesn't match to group, so if ^ in the beginning, then
    return true.
 */           /* He he hee hee hee, some sick code again: */
              positive_flag = !positive_flag;
              goto nakki;
            }
         }

        return(FALSE); /* If no group_ending_character */
}

