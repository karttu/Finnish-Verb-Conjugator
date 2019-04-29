
/*
   conjugate - verb conjugation routines in Lisp, C and C++
   
   Coded by Antti Karttunen, http://www.iki.fi/~kartturi/
   Copyright (C) (1993-2003) by Antti Karttunen.

   Compile as:

#!/bin/sh
gcc -c -o wildcard.o wildcard.cpp
gcc -c -o conjtest.o conjtest.cpp
gcc -c -o conjugat.o conjugat.cpp
gcc -o conjugat.cgi conjugat.o conjtest.o wildcard.o


   References:

   Eugene Holman: Handbook of Finnish Verbs, published by SKS,
                  ISBN 951-717-362-8 (ISSN 0355-1768)

   Fred Karlsson: Finnish Grammar, published by WSOY.

   CHANGES:
 
      1.1.1997 by Antti Karttunen:

      Fixed inflections of the verbs "jaella" 41 ja- (NSS 28)
      "maata" 50 maa- (NSS 35), "taata" 50 taa- (NSS 35) and
      "koota" 56 koo- (NSS 38), by ensuring that their stem is gradated
      correctly from weak to strong when the strong form is needed.
      (by overwriting the method gradate_for_svs() in the respective
       verb classes).
 
      Transferred verbs "rangaista" and "vavista" (in our class 33)
      from the NSS class 24 to the unused NSS class 41, which
      allows us to use a different method get_abstract_noun for them than
      for the verbs of our classes 34 & 36.
      This "transferring" was made by editing the array verbclassflags

      Added more implementations of the method
         virtual void get_abstract_noun()
      to various verb classes.
      Note that this method is only for experimental use!
      For many verbs it will produce ERRONEOUS or totally NONEXISTENT forms.
      For some verb classes it will simply return invalid_combination();
      which is a default action.
      Note that only with a few Finnish verb classes it is possible to
      produce all the "abstract nouns" productively from the
      corresponding verbs. Even the whole concept of the "abstract noun"
      has some laxity in it. What is actually meant with it?

      The nominal inflection of these nouns has not been implemented
      (the singular cases MIGHT accidentally work with some verb classes.)

      2.JAN.1997

      Fixed a little bit more.
      Added a CGI-script functionality by editing CONJTEST.CPP
      Can be now used from the Web.


      19. FEB 2003

      Changed '{' -> 'ä' = \344, '|' -> 'ö' = \366.
      I guess this could be made much more compact with the
      help of "natural categories" for Finnish consonants.

 */

typedef unsigned char Uchar;
typedef unsigned int Uint;
typedef int (*PFI)(Uchar); /* Pointer to function returning int. */

#define toUstr(X) ((Uchar*) (X))
#define toStr(X) ((char *) (X))

#include "conjugat.h"
#include "string.h"

extern "C"
 {

#include "stdio.h"
#include "ctype.h"

#ifdef AZTEC
#define strchr index
#endif

/* extern Uchar  *strcpy(Uchar *, Uchar const *); */
/*
extern Uchar  *strncpy(Uchar *, Uchar const *, size_t);
extern Uchar  *strcat(Uchar *, Uchar const *);
extern Uchar  *strncat(Uchar *, Uchar const *, size_t);
extern Uchar  *strchr(Uchar const *, Uchar);
 */
/* extern size_t strlen(Uchar const *); */


 };

int weak2strong(Uchar *s);
int strong2weak(Uchar *s);

#define MASK_GRADATION 1
#define MASK_VHARMONY  6

#define NO_GRADATION   0
#define GRADATION      1
/* Use strong2weak or weak2strong to get another stem */

#define ALWAYS_BACK    0 /* Infinitive ends with 'a' */
#define ALWAYS_FRONT   2 /* Infinitive ends with 'ä' */
#define LAST_OF_STEM   4 /* Determine it from the last character of stem. */
#define USE_BACKHARMOP 6 /* Use back1harmop() function to determine it. */

#define get_gradmask() (verbclassflags[(Otype<<1)+1] & MASK_GRADATION)
#define get_vharmonymask() (verbclassflags[(Otype<<1)+1] & MASK_VHARMONY)

#define tunteap() (NSS_Class == 14)
/* Are we inflecting "olla"-verb? */
#define ollap() ((NSS_Class == 46))
#define nahda_or_tehdap()   (NSS_Class == 33)
#define tietaa_or_taitaap() (NSS_Class == 43)
#define seistap() (Otype == 35)

#define class1verb() (!(Otype & 32))
#define class2verb() (Otype & 32) /* Verbs with infinitive stem. */
#define class2verb_or_nahda_tehdap() (Otype >= 30)

/* Should we apply gradation to stem to get another stem? */
#define gradationp get_gradmask

/* Even indexed bytes in this array contain the "traditional" verb class 
   number used in Nykysuomen Sanakirja (NSS class).
   Odd indexed bytes contain flags which tell whether we should apply
   the consonantal gradation to the stem of verb (to get another stem),
   and also flags which tell how it's is determined whether to use
   backharmonizing vowels (a, o and u) or front harmonizing
   (ä, ö and y, that is, the same vowels with dots) with that verb.
 */

/* Note that now NSS verbclasses 3-8, 43, 12, 14, 16 and 33 are defined
    with gradation on, although their strong vowel stems are not
    synthetized until at run time. The alternative get_wvs() methods
    for synthetizing the weak vovel stems are commented out. Instead,
    the wvs is get from the synthetized svs with Strong2Weak call.
    Maybe it's a little bit slower and little bit more compact this way.
    (space-time tradeoff)
    1.1.1997: verbs rangaista + vavista (in our class 33) were
    transferred from the NSS class 24 to unused NSS class 41, which
    allows us to use different method get_abstract_noun for them than
    for the verbs of our classes 34 & 36.
 */
Uchar verbclassflags[]=
 {
   /* 0 */  0, 0,
   /* 1 */  1, GRADATION   +LAST_OF_STEM,   /* ahavoitu+a, ehty+ä */
   /* 2 */  1, NO_GRADATION+LAST_OF_STEM,   /* ammu+a, edisty+ä */
   /* 3 */  2, GRADATION   +ALWAYS_BACK,    /* aloitt+aa */
   /* 4 */  2, NO_GRADATION+ALWAYS_BACK,    /* ajast+aa */
   /* 5 */  2, GRADATION   +ALWAYS_FRONT,   /* eitt+ää */
   /* 6 */  2, NO_GRADATION+ALWAYS_FRONT,   /* el+ää */
   /* 7 */  3, GRADATION   +LAST_OF_STEM,   /* huu+taa, löy+tää, pyy+tää */
   /* 8 */ 43, GRADATION   +USE_BACKHARMOP, /* tai+taa, tie+tää */
   /* 9 */  4, GRADATION   +USE_BACKHARMOP, /* hoi+taa, kii+tää */
  /* 10 */  5, GRADATION   +USE_BACKHARMOP, /* ime+ltää, joke+ltaa */
  /* 11 */  6, GRADATION   +USE_BACKHARMOP, /* ahe+rtaa, hyke+rtää */
  /* 12 */  7, GRADATION   +LAST_OF_STEM,   /* so+rtaa */
  /* 13 */  8, GRADATION   +ALWAYS_BACK,    /* ale+ntaa (Also NSS class 42) */
  /* 14 */  8, GRADATION   +ALWAYS_FRONT,   /* koele+ntää (Also NSS class 42)*/
  /* 15 */  9, GRADATION   +ALWAYS_BACK,    /* aht+aa, aj+aa */
  /* 16 */ 10, GRADATION   +ALWAYS_BACK,    /* autt+aa, haast+aa */
  /* 17 */ 11, GRADATION   +ALWAYS_BACK,    /* paist+aa, taitt+aa, virkk+aa */
  /* 18 */ 12, GRADATION   +ALWAYS_BACK,    /* kaar+taa, saar+taa */
  /* 19 */ 13, GRADATION   +USE_BACKHARMOP, /* hak+ea, im+eä, pot+ea, pät+eä */
        /* Also NSS class 15 (päteä) */
  /* 20 */ 14, GRADATION   +ALWAYS_BACK,    /* tun+tea */
  /* 21 */ 16, GRADATION   +ALWAYS_FRONT,   /* lä+hteä */
  /* 22 */ 17, NO_GRADATION+USE_BACKHARMOP, /* aist+ia, ets+iä, nuuhk+ia */
  /* 23 */ 17, GRADATION   +USE_BACKHARMOP, /* ahneht+ia, eht+iä, pyyhk+iä */
  /* 24 */ 18, NO_GRADATION+LAST_OF_STEM,   /* analyso+ida, na+ida */
  /* 25 */ 30, NO_GRADATION+LAST_OF_STEM,   /* epärö+ida, luenno+ida */
  /* 26 */ 19, NO_GRADATION+LAST_OF_STEM,   /* saa+da (NSS 19), myy+dä (20) */
                                            /* juo+da (21), vie+dä (22) */
  /* 27 */ 23, NO_GRADATION+ALWAYS_FRONT,   /* kä+ydä */
  /* 28 */ 44, GRADATION   +USE_BACKHARMOP, /* anta+utua, ehe+ytyä */
  /* 29 */  0, 0,
  /* 30 */ 33, GRADATION   +ALWAYS_FRONT,   /* nä+hdä, te+hdä */
  /* 31 */  0, 0,
  /* 32 */  0, 0,
  /* 33 */ 41 /* WAS 24 ! */, GRADATION   +ALWAYS_BACK, /* rangai+sta */
  /* 34 */ 24, NO_GRADATION+ALWAYS_BACK,    /* hai+sta, huri+sta */
  /* 35 */ 45, NO_GRADATION+ALWAYS_FRONT,   /* seis+tä (defective verb) */
  /* 36 */ 24, NO_GRADATION+ALWAYS_FRONT,   /* aivopes+tä, heli+stä */
  /* 37 */ 25, NO_GRADATION+LAST_OF_STEM,   /* kuu+lla, nie+llä */
  /* 38 */ 26, NO_GRADATION+LAST_OF_STEM,   /* pu+rra, su+rra */
  /* 39 */ 27, NO_GRADATION+LAST_OF_STEM,   /* me+nnä, pa+nna */
  /* 40 */ 28, NO_GRADATION+ALWAYS_BACK,    /* aavist+ella, aj+ella */
  /* 41 */ 28, GRADATION   +ALWAYS_BACK,    /* ajat+ella */
  /* 42 */ 28, NO_GRADATION+ALWAYS_FRONT,   /* el+ellä, elvist+ellä */
  /* 43 */ 28, GRADATION   +ALWAYS_FRONT,   /* esit+ellä */
  /* 44 */ 29, NO_GRADATION+ALWAYS_BACK,    /* halke+illa */
  /* 45 */ 29, NO_GRADATION+ALWAYS_FRONT,   /* ente+illä, varjonyrkke+illä */
  /* 46 */ 31, NO_GRADATION+USE_BACKHARMOP, /* ansa+ita, ilo+ita */
  /* 47 */ 32, NO_GRADATION+LAST_OF_STEM,   /* juo+sta, pie+stä, syö+stä */
  /* 48 */ 34, GRADATION   +USE_BACKHARMOP, /* aue+ta, hapa+ta, ilje+tä */
  /* 49 */ 34, NO_GRADATION+USE_BACKHARMOP, /* ale+ta, ene+tä */
  /* 50 */ 35, GRADATION   +LAST_OF_STEM,   /* ahda+ta, evä+tä (Also NSS 40) */
  /* 51 */ 35, NO_GRADATION+LAST_OF_STEM,   /* arva+ta, herä+tä (Also NSS 40)*/
  /* 52 */ 36, GRADATION   +USE_BACKHARMOP, /* au+eta, ilj+etä */
  /* 53 */ 36, NO_GRADATION+USE_BACKHARMOP, /* hirv+etä, katk+eta */
  /* 54 */ 37, GRADATION   +USE_BACKHARMOP, /* lämm+itä */
  /* 55 */ 37, NO_GRADATION+USE_BACKHARMOP, /* häv+itä, lev+itä */
  /* 56 */ 38, GRADATION   +LAST_OF_STEM,   /* hio+ta, koo+ta */
  /* 57 */ 38, NO_GRADATION+LAST_OF_STEM,   /* ero+ta, löhö+tä */
  /* 58 */ 39, GRADATION   +LAST_OF_STEM,   /* kavu+ta, ryöpy+tä */
  /* 59 */ 39, NO_GRADATION+LAST_OF_STEM,   /* halu+ta, äly+tä */
  /* 60 */ 46, NO_GRADATION+ALWAYS_BACK,    /* o+lla (special case of NSS25) */
  /* 61 */  0, 0,
  /* 62 */  0, 0,
  /* 63 */  0, 0
 };

/* For words like lukema and raivaaja. With plural forms change
   last 'a' to 'i' (except with NOMINATIVE just add 't'.) */
static Uchar *CASE_SUFFIXES[] =
 {
   toUstr(""),    // NOMINATIVE   raivaaja     raivaajat
   toUstr("n"),   // GENITIVE     raivaajan    raivaajien (note the 'e')
   toUstr("A"),   // PARTITIVE    raivaajaa    raivaajia
   toUstr(""),    // ACCUSATIVE   raivaaja(n)  raivaajat
   toUstr("ksi"), // TRANSLATIVE  raivaajaksi  raivaajiksi
   toUstr("nA"),  // ESSIVE       raivaajana   raivaajina
   toUstr("ssA"), // INESSIVE     raivaajassa  raivaajissa
   toUstr("stA"), // ELATIVE      raivaajasta  raivaajista
   toUstr("@n"),  // ILLATIVE     raivaajaan   raivaajiin
   toUstr("llA"), // ADESSIVE     raivaajalla  raivaajilla
   toUstr("ltA"), // ABLATIVE     raivaajalta  raivaajilta
   toUstr("lle"), // ALLATIVE     raivaajalle  raivaajille
   toUstr("ttA"), // ABESSIVE     raivaajatta  raivaajitta
   toUstr("n"),   // INSTRUCTIVE               raivaajin
   toUstr("ne"),  // COMITATIVE                raivaajine/ni/si/en/mme/nne
   toUstr("tse")  // PROLATIVE
 // maitse, meritse, ilmateitse, postitse, raivaajitse?
 };

static Uchar *POSS_SUFFIXES[] = /* Possessive suffixes. */
 { toUstr("ni"), toUstr("mme"), toUstr("si"), toUstr("nne"), toUstr("@n"),
   toUstr("@n")
 };
/* @ stands for: duplicate last vowel. */

static Uchar *NEG_AUXILIARIES[] =
 { toUstr("en"), toUstr("emme"), toUstr("et"), toUstr("ette"), toUstr("ei"),
   toUstr("eivät")
 };

static Uchar *NEG_IMPERATIVES[] =
 { toUstr("-"), toUstr("älkäämme"), toUstr("älä"), toUstr("älkää"),
   toUstr("älköön"), toUstr("älkööt")
 };

/*
 Sixth (and seventh) element is used with negatives:
 Except with conditionals, the seventh element (oltaisiin) is used for passive
 */
static Uchar *OLLA_PRESENTS[] =
 { toUstr("olen"), toUstr("olemme"), toUstr("olet"), toUstr("olette"),
   toUstr("on"), toUstr("ovat"), toUstr("ole")
 };
static Uchar *OLLA_IMPERFECTS[] =
 { toUstr("olin"), toUstr("olimme"), toUstr("olit"), toUstr("olitte"),
   toUstr("oli"), toUstr("olivat"), toUstr("ollut"), toUstr("olleet")
 };
static Uchar *OLLA_CONDITIONALS[] =
 { toUstr("olisin"), toUstr("olisimme"), toUstr("olisit"), toUstr("olisitte"),
   toUstr("olisi"), toUstr("olisivat"), toUstr("olisi"), toUstr("oltaisiin")
 };
static Uchar *OLLA_IMPERATIVES[] =
 { toUstr("-"), toUstr("-"), toUstr("-"), toUstr("-"), toUstr("olkoon"),
   toUstr("olkoot"), toUstr("olko")
 };
static Uchar *OLLA_POTENTIALS[] =
 { toUstr("lienen"), toUstr("lienemme"), toUstr("lienet"), toUstr("lienette"),
   toUstr("lienee"), toUstr("lienevät"), toUstr("liene")
 };

Uchar *Rp; /* Result Pointer */
Uchar *Result;
Uchar Otype,NSS_Class;
Uint Flags;
Uchar Backf;
Uchar A,O,U;

Uchar *VOWELS = toUstr("aeiouyäö");

#define SEP1 (' ') /* Separator 1 is blank. */

#define strequ(s,t)         !strcmp((s),(t))
#define contains_a_char(S,C) (strchr(toStr(S),C))
#define isvowelp(c)         (contains_a_char(VOWELS,(c)))
#define addchar(c)          (*++Rp = ((Uchar) (c)))
#define firstchar()         (*Result)
#define lastchar()          (*Rp)
#define penultchar()        (*(Rp-1))
#define changelastchar(c)   (lastchar() = (c))
#define changepenultchar(c) (penultchar() = (c))
#define deletelastchar()    (*Rp-- = '\0')
#define duplastchar()       ((*(Rp+1)=*Rp),(*++Rp)) /* addchar(lastchar()) */
/* Set the Rp to beginning, and replace the whole Result with s: */
#define replacestring(s)    ((Rp = (Result-1)),addstring(toUstr(s)))

/* We have to add the terminating zero before we use front1harmop: */
#define frontharmop()       ((*(Rp+1)='\0'),front1harmop(Result))

#define invalid_combination() { replacestring("-"); return; }

/* If no quantitative gradation occurs (i.e. weak2strong returns 0 or 1)
    then decrement Rp back to the last character:
 */
#define Weak2Strong()  { addchar('\0'); Rp -= (weak2strong(Result)!=2); } 
/* If quantitative gradation occurs (i.e. strong2weak returns 2)
    then decrement Rp by two characters. (Otherwise only by one character,
     so that it again points to the last character of Result, after
     addchar('\0') incremented it by one.)
 */
#define Strong2Weak()  { addchar('\0'); Rp -= ((strong2weak(Result)==2)+1); } 

#define get_syllable_count() (*(Rp+1) = '\0',count_syllables(Result))

/* String s can contain 'variables' @, A, O, or U. @ means that the last
   character should be duplicated. A, O and U stand for a, o and u or
   ä, ö and y (corresponding 'dotted' vowels), depending from whether
   the stem is back or front harmonizing.
 */
void addstring(Uchar *s)
{
    while(*s)
     {
       if(*s == '@')      { duplastchar(); }
       else if(*s == 'A') { addchar(A); }
       else if(*s == 'O') { addchar(O); }
       else if(*s == 'U') { addchar(U); }
       else               { addchar(*s); }
       s++;
     }
}


/*
; Duplicate last vowel, but only if it's not the second vowel of
; diphthong or long vowel.
; In this case all the other vowel combinations except those which
; have a or ä as the second vowel are treated as "diphthongs".
 */
void duplastvow(void)
{
    if((lastchar() != penultchar()) && 
       ((lastchar() == A) || !isvowelp(penultchar())))
     { duplastchar(); }
}

int back1harmop(Uchar *s)
{   /* Check whether the string s contains a, o or u: */
    return(!!(contains_a_char(s,'a') || contains_a_char(s,'o')
                                     || contains_a_char(s,'u')));
}

int back2harmop(Uchar *s)
{
/* If the last letter is a, o or u, then it's backharmonizing:
   (but not with e, i, y, ä and ö) */
    return(!!contains_a_char(toUstr("aou"),*(s+strlen(toStr(s))-1)));
}

int front1harmop(Uchar *s)
{   /* Check whether the string s contains ä, ö or y: */
    return(!!(contains_a_char(s,'ä') || contains_a_char(s,'ö')
                                     || contains_a_char(s,'y')));
}

/* =========== SOME MISC. FUNCTIONS FOR STRING MANIPULATION ========== */


int f_isvowelp(Uchar c)
{
    return(!!isvowelp(c));
}

int f_isspace(Uchar c)
{ return(isascii(c) && isspace(c)); }

/* Gets pointer to first character in s which, when applied to testfun,
    returns non-zero. For example   fun_index(buf,f_isdigit)
    returns pointer to first digit in buf, NULL if there is
    no digits at all in buf.
 */
Uchar *fun_index_gen(Uchar *s,PFI testfun,Uint negate_flag)
{
    while(*s)
     {
       if(negate_flag ^ (!!((*testfun)(*s)))) { return(s); }
       s++;
     }
    return(NULL);
}


/* This is like previous, but start searching from the end. */
Uchar *fun_rindex_gen(Uchar *s,PFI testfun,Uint negate_flag)
{
    Uchar *orig_s;

/*  if(!s) { return(s); } */ /* If s is NULL, return NULL immediately */
    if(!*s) { return(NULL); } /* Nope ! If s is empty, then return NULL */
    orig_s = s; /* Save the beginning of s */
    s += (strlen(toStr(s))-1); /* Get pointer to last character */

    while(1)
     {
       if(negate_flag ^ (!!((*testfun)(*s)))) { return(s); }
       if(s == orig_s) { break; }
       --s;
     }
    return(NULL);
}


Uchar *strip_blankos(Uchar *s)
{
    Uchar *t;

    /* Get pointer to last non white space char: */
    t = fun_rindex_gen(s,f_isspace,((Uint)1));
/* If there were nothing else than white spaces (blanks, tabs, newlines) then
   change s to empty string. Otherwise cut after last non white space char: */
    if(!t) { *s = '\0'; } else { *(t+1) = '\0'; }
    return(s);
}

/* ================ STUFF FOR CONSONANT GRADATION, ETC. =============== */

int is_first_vowel_a(Uchar *s)
{
    return((s = fun_index_gen(s,f_isvowelp,((Uint)0))) && (*s == 'a'));
}


int is_long_or_diftong(Uchar c1,Uchar c2,int count)
{
    if(c1 == c2) { return(1); } /* Long vowel. */
    if(c2 == 'i') { return(1); }
    if((c2 == 'u') && contains_a_char("aeio",c1)) { return(1); }
    if((c2 == 'y') && contains_a_char("eiäö",c1)) { return(1); }
    if(!count) /* These are diftongs only in first syllable: */
     {
       if((c1 == 'i') && (c2 == 'e')) { return(1); }
       if((c1 == 'u') && (c2 == 'o')) { return(1); }
       if((c1 == 'y') && (c2 == 'ö')) { return(1); }
     }
    return(0);
}


int count_syllables(Uchar *s)
{
    int count;
    Uchar c1;

    count=0;

    while(*s)
     {
       if(isvowelp(*s))
        {
          c1 = *s++;
          if(is_long_or_diftong(c1,*s,count))
           { s++; }
          count++;
        }
       else { s++; } /* Skip consonants. */
     }

    return(count);
}


/* Get pointer to pointing to the last consonant or vowel combination
    which is not long nor diftong, whichever is met first.
 */
Uchar *getposition(Uchar *s)
{
    Uchar *orig_s;

    orig_s = s;

    s = (s+strlen(toStr(s))-1); /* Point to last character. */

    while(s > orig_s)
     {
       if(!isvowelp(*s)) { return(s); } /* Last 'consonant'. */
       if(*(s+1) && !is_long_or_diftong(*s,*(s+1),1)) { return(s); }
       --s;
     }

    return(NULL); /* Didn't find any sensible position. */
}


/* Returns
    0  if no changes.
    1  if qualitative  gradation (i.e. length doesn't change.)
    2  if quantitative gradation (i.e. length grows.)

    s should be positioned to the last consonant or to last vowel boundary ???

    Note that this doesn't gradate double vowels like "maa" or "koo".
 */
int aux_weak2strong(Uchar *s,Uchar next)
{
    Uchar prev,isvowelf;

    prev = *(s-1);
    isvowelf = !!isvowelp(prev);

/*  if((prev == 's') || (prev == 't')) { return(0); } */ /* No gradation. */

    if((prev == 'n') && (*s == 'g')) { *s = 'k'; return(1); } /* ng -> nk */

    if(*s != prev) /* Not geminaatta already... */
     {
       if((((*s == 'b') || (*s == 'p')) &&
              (isvowelf || contains_a_char("lmr",prev)))
          ||
         (contains_a_char("gkt",*s) &&
           (isvowelf || contains_a_char("lnr",prev))))
        {
          *(s+1) = *s; /* Duplicate the consonant. */
          return(2);
        }
     }

    if((*s == 'v') && (isvowelf || (prev == 'l') || (prev == 'r')))
     { *s = 'p'; return(1); } /* v -> p, lv -> lp, rv -> rp */

    if((*s == 'm') && (prev == *s)) { *s = 'p'; return(1); } /* mm -> mp */

    if((*s == 'd') && ((prev == 'h') || isvowelf)) /* d -> t, hd -> ht */
     { *s = 't'; return(1); }

    if((*s == prev) && contains_a_char("lnr",prev))
     { *s = 't'; return(1); } /* ll -> lt, nn -> nt, rr -> rt */

    if((prev == 'u') && (*s == '\'') && (next == 'u')) /* u'u -> uku */
     { *s = 'k'; return(1); }

    if(contains_a_char("hlr",*s) && isvowelf) /* h -> hk, l -> lk, r -> rk */
     { *(s+1) = 'k'; return(2); }

    if(contains_a_char("hlr",prev) && (*s == 'j') && (next == 'e'))
     { *s = 'k'; return(1); } /* lje -> lke, rje -> rke, hje -> hke */

    if(isvowelp(*s) && isvowelp(next) && !is_long_or_diftong(*s,next,1))
     {
       *(s+1) = 'k'; return(2); /* 0 -> k (lue -> luke) */
     }

    return(0); /* No gradation. */
}


/* Returns
    0  if no changes.
    1  if qualitative  gradation (i.e. length doesn't change.)
    2  if quantitative gradation (i.e. length lessens.)

    s should be positioned to the last consonant or to last vowel boundary ???
 */
int aux_strong2weak(Uchar *s,Uchar next)
{
    Uchar prev,isvowelf;

    prev = *(s-1);
    isvowelf = !!isvowelp(prev);

    if(contains_a_char("bgkpt",*s) && (*s == prev))
     { /* If bb, gg, kk, pp, or tt */
       return(2); /* This indicates that the other one should be deleted. */
     }

    if(*s == 'p')
     {
       if(isvowelf || (prev == 'l') || (prev == 'r'))
        { *s = 'v'; } /* p -> v, lp -> lv, rp -> rv */
       else if(prev == 'm') { *s = 'm'; } /* mp -> mm */
       return(1);
     }

    if(*s == 't')
     {
       if((prev == 'h') || isvowelf) /* t -> d, ht -> hd */
        { *s = 'd'; }
       else if(contains_a_char("lnr",prev))
        { *s = prev; } /* lt -> ll, nt -> nn, rt -> rr */
       return(1);
     }

    if((prev == 'u') && (*s == 'k') && (next == 'u')) /* uku -> u'u */
     { *s = '\''; return(1); }

    if(contains_a_char("hlr",prev) && (*s == 'k') && (next == 'e'))
     { *s = 'j'; return(1); } /* lke -> lje, rke -> rje, hke -> hje */

    if(*s == 'k')
     { /* k -> -, hk -> h, lk -> l, rk -> r */
       if(contains_a_char("hlr",prev) || isvowelf)
        { return(2); }
       else if(prev == 'n') { *s = 'g'; return(1); } /* nk -> ng */
     }

    return(0); /* No gradation. */
}


/* 
   Routines weak2strong and strong2weak to do the consonant gradation.
 */

int weak2strong(Uchar *s)
{
    Uchar next,nextnext;
    int z;

    if(!(s = getposition(s))) { return(0); }
    else
     {
       next = *(s+1);
       z = aux_weak2strong(s,next);
       if(z == 2)
        { /* One character is inserted between, move the remaining ones
              one step to right: */
          for(s += 2; next; s++)
           {
             nextnext = *s;
             *s = next;
             next = nextnext;
           }
          *s = '\0';
        }
       return(z);
     }
}


int strong2weak(Uchar *s)
{
    Uchar next;
    int z;

    if(!(s = getposition(s))) { return(0); }
    else
     {
       next = *(s+1);
       z = aux_strong2weak(s,next);
       if(z == 2) { strcpy(toStr(s),toStr((s+1))); } /* Delete one char. */
       return(z);
     }
}

/* ===================== VERB CLASS DEFINITIONS ======================= */


class FVC /* Finnish Verb Class */
 {
public:
   virtual Uchar *get_negative(void);
   virtual Uchar *get_olla_verb(void);
   virtual void get_auxiliaries(void);

   virtual void add_pers_ending(void);
   virtual void add_poss_suffix(void);
   virtual void inflect_nominal(void);
   virtual void conj_passive(void);
   virtual void conj_present_indicative(void);
   virtual void conj_imperfect_indicative(void);
   virtual void conj_present_imperative(void);
   virtual void conj_present_conditional(void);
   virtual void conj_present_potential(void);
   virtual void conj_infinitive1(void);
   virtual void conj_infinitive2(void);
   virtual void conj_infinitive3(void);
   virtual void conj_infinitive4(void);
   virtual void conj_infinitive5(void);
   virtual void conj_part1act(void);
   virtual void conj_part1pas(void);
   virtual void conj_part2act(void);
   virtual void conj_part2pas(void);
   virtual void conj_agential_noun(void);
   virtual void conj_negative_adj(void);
   virtual void conj_abstract_noun(void); // Not functional yet?

   virtual void add1missing_letters() { } // Added before cons. gradation.
   virtual void add2missing_letters() { } // Added after  cons. gradation.

   virtual void get0infstem() = 0;
   virtual void get1infstem() { this->get0infstem(); } // By default.
   virtual void get2infstem()
    { this->get1infstem(); if(lastchar() == 'e') { changelastchar('i'); } }
   virtual void get3infstem() // Used for Third Infinitive
    { this->get_svs(); addchar('m'); addchar(A); } // punoma, sukima, pakkaama
   virtual void get4infstem()
    { this->get_svs(); addstring(toUstr("minen")); } // sukiminen

   virtual void get_svs() = 0;
   virtual void get_wvs() { this->get_svs(); }
   virtual void get_vs()  { this->get_svs(); }
   virtual void get_cs()  { this->get_svs(); addstring(toUstr("isi")); }
   virtual void get_sis() { this->get_svs(); changelastchar('i'); }
   virtual void get_wis() { this->get_wvs(); changelastchar('i'); }
   virtual void get_is()  { this->get_sis(); }
   virtual void get_wps() { this->get1infstem(); }
   virtual void get_sps() { this->get_wps(); Weak2Strong(); }
   virtual void get_part2actstem(void);
   virtual void get_potstem() { this->get_part2actstem(); addchar('e'); }

   virtual void get_part1act() // Present Active Participle (sukiva)
    { this->get_svs(); addchar('v'); addchar(A); }
   virtual void get_part1pas() // Present Passive Participle (suittava)
    { this->get_sps(); addstring(toUstr("AvA")); }
   virtual void get_part2act() // Past Active Participle. (sukinut/sukineet)
    {
      if(plurpersp()) { this->get_part2act_pl(); }
      else            { this->get_part2act_sg(); }
    }
   virtual void get_part2act_sg()
    { this->get_part2actstem(); addchar(U); addchar('t'); }
   virtual void get_part2act_pl()
    { this->get_part2actstem(); addstring(toUstr("eet")); }
   virtual void get_part2pas() // Past Passive Participle (suittu)
    { this->get_sps(); addchar(U); }
   virtual void get_agential_noun() = 0;
// For cases which have not been implemented yet:
   virtual void get_abstract_noun() { invalid_combination(); }

   virtual Uchar isLongAgentPluralp() { return(0); } // By default short.
 };



/*
; Get appropriate negative auxiliary:
 */
Uchar *FVC::get_negative(void)
{
    int n;

    n = (get_person_number() - 2);
    if(n == -2) { n = 4; } /* Kludge for passive mode (PERSON_NUMBER == 0) */
    return((imperativep()?NEG_IMPERATIVES:NEG_AUXILIARIES)[n]);
}


/*
; Get appropriate affirmative auxiliary for secondary tenses:
 */
Uchar *FVC::get_olla_verb(void)
{
    int n;
    Uchar **pp;

    n = (get_person_number() - 2);
/*
 Kludge for passive mode (PERSON_NUMBER == 0), and Passive Perfect Conditional:
 */
    if(n == -2) { n = (conditionalp() ? 7 : 4); }
    if(negativep()) /* Kludge for negatives. */
     {
       if(pluperfectp() && plurpersp()) { n = 7; } /* olleet */
       else { n = 6; } /* ole, ollut, olisi, liene */
     }

    if(conditionalp())     { pp = OLLA_CONDITIONALS; } /* Perf. Cond. */
    else if(potentialp())  { pp = OLLA_POTENTIALS; }   /* Perf. Pot. */
    else if(imperativep())  /* Perfect Imperative */
     { /* If not valid combination, then mark the result as invalid: */
       if(!thirdpersp() && !passivep()) { return(toUstr("-")); }
       else { pp = OLLA_IMPERATIVES; }
     }
    else if(pluperfectp() || imperfectp())   /* Pluperf. || Imperf. Indic. */
                           { pp = OLLA_IMPERFECTS; }
    else { pp = OLLA_PRESENTS; } /* Present or Perf. Indic. */

    return(pp[n]);
}


/*
; This copies the necessary negative-auxiliary and/or olla-auxiliary into
; result, if any of them is needed.
 */
void FVC::get_auxiliaries(void)
{
   if(negativep())
    {
      addstring(this->get_negative());
      if(lastchar() == '-') { invalid_combination(); }
      addchar(SEP1);
    }
   if(sectensep())
    {
      addstring(this->get_olla_verb());
      if(lastchar() == '-') { invalid_combination(); }
      addchar(SEP1);
    }
}


void FVC::add_poss_suffix(void)
{
    int n;

    n = get_poss_suffix();
    if(n) // Add nothing if poss_suffix bits are zero.
     {
       if(translativep()) { changelastchar('e'); } // -ksi -> -kse-
       if(!isvowelp(lastchar())) { deletelastchar(); }
// lukeakseen (not lukeaksensa), nauroivat nauramistaan (not nauramistansa)
// and saamaisillaan (not saamaisillansa)
// However, nähtensä (second infinitive instructive + third pers. poss suffix)
       if(thirdpersp() && !infinitive1p()&& !infinitive4p()&& !infinitive5p()
        &&((get_nominal_case() <= ACCUSATIVE) ||illativep() || instructivep()))
        { addstring(toUstr("nsA")); } // Instead of default @n
// What about partitive plural? raivaajiaan or raivaajiansa ?
       else { addstring(POSS_SUFFIXES[n-2]); }
     }
}

void FVC::inflect_nominal(void)
{
    int i;

    i = get_case_index();

    if(plurcasep())
     {
       if(nominativep() || accusativep()) { addchar('t'); }
       else if(agential_nounp() && this->isLongAgentPluralp())
        {
          changelastchar(O); // lukijoi
          addchar('i');
          if(genitivep()) { addstring(toUstr("de")); } // lukijoiden
          else if(partitivep()) { addchar('t'); }     // lukijoita
          else if(illativep())                        // lukijoihin
           { addstring(toUstr("hin")); this->add_poss_suffix(); return; }
        }
       else
        {
          changelastchar('i');
          if(genitivep()) { addchar('e'); }
        }
     }
    else // singular
     {
// COMITATIVE (lukijoineen) and PROLATIVE (lukijoitse) can be applied
// only in the plural. However, let INSTRUCTIVE work also with singular,
// e.g. LUKEMAN (third infinitive instructive) or JALAN (some future addition?)
       if(get_nominal_case() > INSTRUCTIVE) { invalid_combination(); }
     }

    addstring(CASE_SUFFIXES[i]);
    this->add_poss_suffix();
}


void FVC::add_pers_ending(void)
{
    if(affirmativep()) /* Do nothing if negative. */
     {
       if(singpersp())
        {
          if(firstpersp())        { addchar('n'); }
          else if(secondpersp())  { addchar('t'); }
          else /* thirdpersp() */
           {
/* If present indicative or potential, then lengthen the last vowel: */
             if(presindicp() || potentialp()) { duplastvow(); }
           }
        }
       else /* Else it's plural person. */
        {
          if(firstpersp())        { addstring(toUstr("mme")); }
          else if(secondpersp())  { addstring(toUstr("tte")); }
          else /* thirdpersp() */
           {
             addstring(toUstr("vAt"));
           }
        }
     }
}

void FVC::conj_passive(void)
{
   /* If secondary tense or negative imperfect, then this is enough: */
   if(sectensep() || (negativep() && imperfectp())) { this->get_part2pas(); }
   else
    {
      if(presindicp()) { this->get_wps(); } else { this->get_sps(); }
      if(imperfectp()) { addchar('i'); }
      else
       {
         addchar(A);
         if(imperativep())       { addchar('k'); addchar(O); }
         else if(conditionalp()) { addstring(toUstr("isi")); }
         else if(potentialp())   { addstring(toUstr("ne")); }
         /* Else it's Present Indicative, do nothing for that. */
       }
      if(affirmativep()) /* If negative, do nothing, but if affirmative */
       { /* then duplicate the last character and add 'n', so we get: */
         duplastchar();
         addchar('n'); /* juodaan, juotiin, juotakoon, juotaisiin, juotaneen */
       }
    }
}


void FVC::conj_present_indicative(void)
{
    this->get_vs();
    this->add_pers_ending();
}



void FVC::conj_imperfect_indicative(void)
{
    if(negativep()) { this->get_part2act(); }
    else
     {
       this->get_is();
       this->add_pers_ending();
     }
}

/* Active Present Imperative goes like this:
   ---       onkikaamme      ---            älkäämme onkiko
   ongi      onkikaa         älä ongi       älkää onkiko
   onkikoon  onkikoot        älk||n onkiko  älk||t onkiko
 */
void FVC::conj_present_imperative(void)
{
    /* Use Weak Vovel Stem when 2nd person singular: */
    if(singpersp() && !thirdpersp())
     {
       if(firstpersp()) { invalid_combination(); }
       else /* if(secondpersp()) */ { this->get_wvs(); }
     }
    else
     {
       this->get0infstem(); /* Else use infinitive stem. */
       addchar('k'); /* Plus -ko, -koon, -kaamme, -kaa or -koot */
       addchar((negativep() || thirdpersp()) ? O : A);
       if(affirmativep())
        {
          duplastchar();
          if(thirdpersp())
           { addchar((singpersp() ? 'n' : 't')); }
          else if(firstpersp()) { addstring(toUstr("mme")); }
        }
     }
}


void FVC::conj_present_conditional()
{
    this->get_cs(); /* Get Conditional Stem. */
    this->add_pers_ending(); /* Add Personal Ending if needed. */
}


void FVC::conj_present_potential()
{
    this->get_potstem(); /* Get Potential Stem. */
    this->add_pers_ending(); /* Add Personal Ending if needed. */
}

void FVC::conj_infinitive1(void)
{
    this->get1infstem();
    addchar(A);
    // If possessive suffix specified, then return the long form:
    if(get_poss_suffix())
     { addstring(toUstr("kse")); this->add_poss_suffix(); }
}

void FVC::conj_infinitive2(void)
{   /* If no case defined, give an inessive passive (luettaessa) to user: */
    if(nominativep()) { this->get_sps(); addstring(toUstr("AessA")); }
    else if(inessivep())
     { this->get2infstem(); addstring(toUstr("essA")); } // lukiessa (ni/si/an)
    else if(instructivep())
     { this->get2infstem(); addstring(toUstr("en")); } // lukien
    else { invalid_combination(); } // No other cases possible.
    this->add_poss_suffix(); // Doesn't yet work correctly with instructive...
// nähden, nähteni, nähdessään (consonantal gradation!)
}


void FVC::conj_infinitive3(void)
{
    this->get3infstem();
    this->inflect_nominal();
}

void FVC::conj_infinitive4(void)
{
    this->get4infstem(); // Inflection for this not implemented yet!
}

void FVC::conj_infinitive5(void)
{
    this->get3infstem();
    addstring(toUstr("isillA"));
    this->add_poss_suffix();
}

void FVC::conj_part1act(void)
{
    this->get_part1act();
    this->inflect_nominal();
}

void FVC::conj_part1pas(void)
{
    this->get_part1pas();
    this->inflect_nominal();
}

void FVC::conj_part2act(void)
{
    if(plurcasep()) { this->get_part2act_pl(); }
    else            { this->get_part2act_sg(); }
}

void FVC::conj_part2pas(void)
{
    this->get_part2pas();
}


void FVC::conj_agential_noun(void)
{
    this->get_agential_noun();
    this->inflect_nominal();
}

void FVC::conj_abstract_noun(void)
{
    this->get_abstract_noun();
    this->inflect_nominal();
}

void FVC::conj_negative_adj(void)
{ // Inflection not implemented yet!
    this->get3infstem();
    addstring(toUstr("tOn"));
}



/*
   Get Past Active Participle stem of the verb (also used for Potential)
 */
void FVC::get_part2actstem(void)
{
    this->get0infstem();
    if(lastchar() == 't') { changelastchar('n'); }
    if((lastchar() == 'l')||(lastchar() == 'r')||(lastchar() == 's'))
     { duplastchar(); }
    else { addchar('n'); }
}


class Class1Verbs : public FVC
 {
public:
   virtual void get0infstem() { /* Do nothing. */ }
   virtual void get_svs() { get0infstem(); }
   virtual void get_wvs() { this->get_svs(); gradate_for_wvs(); }
   virtual void get_vs() 
    {
      if(thirdpersp() && affirmativep()) { get_svs(); } else { get_wvs(); }
    }
   virtual void get_is()
    {
      if(thirdpersp()) { get_sis(); } else { get_wis(); }
    }
   virtual void get_wps() { get_wvs(); changelastchar('e'); addchar('t'); }
   virtual void gradate_for_svs() { /* Do nothing. */ }
   virtual void gradate_for_wvs()
    {
      if(gradationp()) { Strong2Weak(); }
    }
   virtual void get_agential_noun()
    { this->get2infstem(); addchar('j'); addchar(A); } // punoja, sukija
   virtual void get_abstract_noun()
    { // huuto, lento, sorto, taito, tieto
      if(get_syllable_count() == 1)
       {
         int frontf = frontharmop(); /* Do this before get_svs adds ä */
         get_svs();
         if(frontf) { changelastchar('ö'); }
         else { changelastchar('o'); }
/*       changelastchar(O);  Produced many incorrect ones:
         vetö, elö, kieltö, kiertö, kiiltö, tietö */
       }
      else
       { // pahennus, kavallus (but incorrect: koelennys! (should be koelento)
         get_wvs(); changelastchar(U); addchar('s');
       }
    }
 };

class Class2Verbs : public FVC
 {
public:
   virtual void gradate_for_svs()
    {
      if(gradationp()) { Weak2Strong(); }
    }
   virtual void gradate_for_wvs() { /* Do nothing. */ }
   virtual void get_agential_noun()
    { this->get_sis(); addchar('j'); addchar(A); } // valitsija
 };
 
class nss1 : public Class1Verbs /* aiko+a, puno+a */
 {
public:
   virtual void get_wps() { get_wvs(); addchar('t'); }
   virtual void get_sis() { this->get_svs(); addchar('i'); }
   virtual void get_wis() { this->get_wvs(); addchar('i'); }
   virtual void get_abstract_noun()
    { get0infstem(); addstring(toUstr("mUs")); }
 };

class nss44 : public nss1 /* anta+utua */
 {
public:
   virtual void get0infstem()
    { addstring(toUstr("UtU")); }
/* Not needed now, because we use Strong2Weak in Class1Verbs::get_wvs()
   virtual void get_wvs()
    { addstring(toUstr("UdU")); }
 */
 };

class nss2and9to11 : public Class1Verbs
 { /* muist+aa, kaiv+aa, haist+aa, paist+aa */
public:
   virtual void get0infstem() { addchar(A); }
 };

class nss2 : public nss2and9to11 /* edust+aa, muist+aa */
 {
 };

class nss3to8and43 : public Class1Verbs
 {
public:
   virtual void get0infstem() { addchar('t'); addchar(A); }
/* Not needed now, because we use Strong2Weak in Class1Verbs::get_wvs()
   virtual void get_wvs()     { addchar('d'); addchar(A); }
 */
 };


class nss3and5to8and43 : public nss3to8and43
 {
public:
   virtual void get_sis() { addstring(toUstr("si")); }
   virtual void get_wis() { get_sis(); }
 };

class nss3 : public nss3and5to8and43 /* huu+taa */
 {
 };

class nss4 : public nss3to8and43 /* sou+taa */
 {
public:
 };

class nss43 : public nss3and5to8and43 /* tai+taa */
 {
public:
/* tai+nn+ut & tie+nn+yt: */
   virtual void get_part2actstem() { addchar('n'); duplastchar(); }
/* mutta: tai+ta+ne & tie+tä+ne: */
   virtual void get_potstem() { get1infstem(); addstring(toUstr("ne")); }
 };

class nss5to8 : public nss3and5to8and43
 { /* puol+taa, mur+taa, sor+taa, pahen+taa */
public:
/* Not needed now, because we use Strong2Weak in Class1Verbs::get_wvs()
   virtual void get_wvs()     { duplastchar(); addchar(A); }
 */
 };

class nss5 : public nss5to8 /* puo+ltaa */
 {
public:
   virtual void add1missing_letters() { addchar('l'); }
 };

class nss6 : public nss5to8 /* mu+rtaa */
 {
public:
   virtual void add1missing_letters() { addchar('r'); }
 };

class nss7 :  public nss6 /* so+rtaa */
 {
public:
   virtual void get_sis() { this->get_svs(); changelastchar('i'); }
   virtual void get_wis() { this->get_wvs(); changelastchar('i'); }
 };

class nss8 : public nss5to8 /* pahe+ntaa */
 {
public:
   virtual void add1missing_letters() { addchar('n'); }
 };

class nss42 : public nss8 /* rake+ntaa */
 {
public:
 };

class nss9to11 : public nss2and9to11 /* kaiv+aa, haist+aa, paist+aa */
 {
   virtual void get_sis()
    { this->get_svs(); changelastchar('o'); addchar('i'); }
   virtual void get_wis()
    { this->get_wvs(); changelastchar('o'); addchar('i'); }
 };

class nss9 : public nss9to11 /* kaiv+aa */
 {
 };

class nss10 : public nss9 /* haast+aa */
 {
 };

class nss11 : public nss10 /* paist+aa */
 {
 };

class nss12 : public nss10 /* saar+taa */
 {
public:
   virtual void get0infstem() { addchar('t');  addchar(A); }
/* Not needed now, because we use Strong2Weak in Class1Verbs::get_wvs()
   virtual void get_wvs()     { duplastchar(); addchar(A); }
 */
 };

class nss13to16 : public Class1Verbs /* lask+ea, tun+tea, pot+ea, lä+hteä */
 {
public:
   virtual void get_cs()
    { this->get_svs(); changelastchar('i'); addstring(toUstr("si")); }
   virtual Uchar isLongAgentPluralp() { return(1); }
   virtual void get_abstract_noun() { addchar(O); } // tunto, lähtö
 };

class nss13 : public nss13to16 /* lask+ea */
 {
public:
   virtual void get0infstem() { addchar('e'); }
   virtual void get_abstract_noun()
    { // 
/* The old kludge for imu, isku, itku! (not imy*,isky*,itku*) commented out
      if(firstchar() == 'i') { addchar('u'); }
      else { addchar(U); } // haku, luku, lasku, kytky, käsky, nylky
 Replaced with this more "generic" piece of code:
 */
      if(frontharmop()) { addchar('y'); }
      else { addchar('u'); }
// Still leaves many incorrect ones, e.g. kaitsu*, kitky*, tuku*, koku*,
// kosku*, kyty*, kätky* (should be kätkö)
    }
 };

class nss14 : public nss13to16 /* tun+tea */
 {
public:
   virtual void get0infstem() { addstring(toUstr("te")); }
/* Not needed now, because we use Strong2Weak in Class1Verbs::get_wvs()
   virtual void get_wvs()     { addstring(toUstr("ne")); }
 */
   virtual void get_sis() { addstring(toUstr("si")); }
   virtual void get_wis() { get_sis(); }
   virtual void get_abstract_noun() { addstring(toUstr("tO")); } // tunto
 };

class nss15 : public nss13 /* pot+ea */
 {
   virtual void get_abstract_noun() { invalid_combination(); }
 };

class nss16 : public nss13to16 /* lä+hteä */
 {
public:
   virtual void get0infstem() { addstring(toUstr("hte")); }
/* Not needed now, because we use Strong2Weak in Class1Verbs::get_wvs()
   virtual void get_wvs()     { addstring(toUstr("hde")); }
 */
   virtual void get_abstract_noun() { addstring(toUstr("htO")); } // lähtö
 };

class nss17 : public Class1Verbs /* opp+i */
 {
public:
   virtual void get0infstem() { addchar('i'); }
   virtual void get_wps() { get_wvs(); addchar('t'); }
   virtual void get_sis() { this->get_svs(); }
   virtual void get_wis() { this->get_wvs(); }
   virtual void get_cs()
    { this->get_svs(); addstring(toUstr("si")); }
   virtual Uchar isLongAgentPluralp() { return(1); }
   virtual void get_abstract_noun()
    { gradate_for_wvs(); addstring(toUstr("intA")); }
 };

class nss18to23and30and33 : public Class1Verbs
 {
public:
   virtual void get1infstem() { this->get0infstem(); addchar('d'); }
   virtual void get_wps() { this->get1infstem(); }
   virtual void get_cs()
    { this->get_sis(); addstring(toUstr("si")); }
   virtual void get_agential_noun()
    { this->get_sis(); addchar('j'); addchar(A); } // kävijä, näkijä
   virtual void get_abstract_noun()
    { get0infstem(); addstring(toUstr("nti")); } // nainti, vointi, käynti
 };

class nss18_30 : public nss18to23and30and33 /* na+i, vo+i, luenno+i */
 {
public:
   virtual void get0infstem() { addchar('i'); }
   virtual void get_sis() { this->get_svs(); }
   virtual void get_wis() { this->get_wvs(); }
 };

class nss18 : public nss18_30
 {
 };

class nss30 : public nss18_30
 {
 };

class nss19to22 : public nss18to23and30and33
 { /* saa+da, myy+dä, juo+da, vie+dä */
public:
   virtual void get0infstem() { /* Do nothing. */ }
   virtual void get_sis()
    { /* Overwrite the second vowel with last: */
      penultchar() = lastchar();
      changelastchar('i'); /* Overwrite the last one with 'i'. */
    } /* so we get jäi, sai, myi, joi, loi, vei */
   virtual void get_wis() { get_sis(); }
   virtual void get_agential_noun() // saaja, myyjä, juoja, viejä
    { this->get_svs(); addchar('j'); addchar(A); }
 };

class nss19 : public nss19to22 /* saa */
 {
 };

class nss20 : public nss19to22 /* myy */
 {
 };

class nss21 : public nss19to22 /* juo, luo */
 {
 };

class nss22 : public nss19to22 /* vie */
 {
 };

class nss23 : public nss18to23and30and33 /* kä+ydä */
 {
public:
   virtual void get0infstem() { addchar('y'); }
   virtual void get_sis() { addstring(toUstr("vi")); }
   virtual void get_wis() { get_sis(); }
   virtual Uchar isLongAgentPluralp() { return(1); }
 };

class nss33 : public nss18to23and30and33 /* nä+hdä, te+hdä */
 {
public:
   virtual void get0infstem() { addchar('h'); }
   virtual void get_svs()     { addstring(toUstr("ke")); }
/* Not needed now, because we use Strong2Weak in Class1Verbs::get_wvs()
   virtual void get_wvs()     { addchar('e'); }
 */
   virtual Uchar isLongAgentPluralp() { return(1); }
   virtual void get_abstract_noun() { addstring(toUstr("kO")); } // näkö, teko
 };

class nss24to34except30and33 : public Class2Verbs
 {
   virtual void get_cs()
    { this->get_sis(); addstring(toUstr("si")); }
   virtual Uchar isLongAgentPluralp() { return(1); }
 };

class nss24_32_41 : public nss24to34except30and33
 {
// Verbs rangai+sta, vavi+sta  transferred to class nss41 which was unused
public:
   virtual void get0infstem() { addchar('s'); }
   virtual void get1infstem() { get0infstem(); addchar('t'); }
 };

class nss24 : public nss24_32_41 /* nuolai+sta, hai+sta, humi+sta */
 {
public:
   virtual void get_svs()
    { gradate_for_svs(); addstring(toUstr("se")); }
   virtual void get_abstract_noun() // nuolaisu, haisu, mutta humina */
    {
      if((lastchar() == 'i') && // The stem ends with -I, but not with -AI
           (penultchar() != 'a') && (penultchar() != 'ä'))
       { addstring(toUstr("nA")); } // Then use -na, to get humina, vilinä
/* One day old kludges commented out 2.JAN.1997: 
      else if(lastchar() == 'e') { addstring(toUstr("su")); } //pesu ei pesy
      else { addstring(toUstr("sU")); } // nuolaisu,haisu,ehkäisy
   Replaced with this more generic piece of code:
 */
      else if(frontharmop()) { addstring(toUstr("sy")); }
      else { addstring(toUstr("su")); }
    } // should be silmäys, ei silmäisy??? (but silmätä -> silmäys)
 };

static nss1  NSS1; // This one was transferred here...
FVC *ClassPtr;

class nss24seista : public nss24 /* defective verb sei+stä -> sei+soa */
 {
public:
   virtual void get_svs() { subst_seisoa(); addstring(toUstr("so")); }
   virtual void get_sis() { this->get_svs(); addchar('i'); }
   virtual void get_wis() { this->get_sis(); }
   virtual void subst_seisoa() // Substitute "seisoa" for "seistä".
    {
      Backf = 1; NSS_Class = 1; Otype = 2; A = 'a'; O = 'o'; U = 'u';
      ClassPtr = &NSS1;
    }
   virtual void get_agential_noun() // seisoja (not seisoija)
    { replacestring("seisoja"); subst_seisoa(); }
   virtual Uchar isLongAgentPluralp() { return(0); } // seisojien
// It's not "seinä" (the wall) !!! But why not?
// cf. Spanish parar (seisahduttaa) - pared (seinä) apenas - tuskin ???
// virtual void get_abstract_noun() { invalid_combination(); }
 };

class nss32 : public nss24_32_41 /* juo+sta */
 {
public:
   virtual void get_svs() { addstring(toUstr("kse")); }
   virtual void get_abstract_noun() { addstring(toUstr("ksU")); }
// juoksu, syöksy, pieksy???
 };

class nss41 : public nss24 /* The original NSS classification was for verbs
                              like kihi+stä */
 { // Now reserved for verbs rangai+sta  and  vavi+sta
   virtual void get_abstract_noun() { addstring(toUstr("stUs")); }
// Produce nouns: rangaistus (punishment), vavistus (vibration)
// (vapina would be good also...) (vavistaa -> vavistus ?)
 };

class nss25to29 : public nss24to34except30and33
 { /* tu+lla, pu+rra, pa+nna, ajat+ella, arva+illa */
public:
   virtual void get0infstem()
    { this->add2missing_letters(); }
   virtual void get1infstem()
    { get0infstem(); duplastchar(); }
   virtual void get_svs()
    { gradate_for_svs(); this->add2missing_letters(); addchar('e'); }
 };

class nss25to27 : public nss25to29 /* tu+lla, pu+rra, pa+nna */
 {
public:
 };

class nss25 : public nss25to27 /* tu+lla */
 {
public:
   virtual void add2missing_letters() { addchar('l'); }
   virtual void get_abstract_noun() { addstring(toUstr("lO")); }
// tulo, kuolo, luulo, olo, mutta ei:
// nielö* (nielu), piilö* (piilo), tuulo* (tuuli), vuolo? -> vuolu
 };

class nss25olla : public nss25 /* o+lla */
 {
public:
   virtual void get_potstem()
    { replacestring("liene"); Backf = 1; A = 'a'; O = 'o'; U = 'u'; }
   virtual void conj_present_indicative(void)
    {
      if(thirdpersp() && affirmativep())
       { /* We want "on" & "ovat" instead of "olee" & "olevat": */
         replacestring(this->get_olla_verb());
       }
      else { FVC::conj_present_indicative(); }
    }
 };


class nss26 : public nss25to27 /* pu+rra */
 {
public:
   virtual void add2missing_letters() { addchar('r'); }
   virtual void get_abstract_noun() { addstring(toUstr("rU")); } //puru,suru
 };

class nss27 : public nss25to27 /* pa+nna */
 {
public:
   virtual void add2missing_letters() { addchar('n'); }
   virtual void get_abstract_noun() { addstring(toUstr("no")); } //meno,pano
 };

class nss28 : public nss25to29 /* ajat+ella, kats+ella, ja+ella */
 {
public:
// An insidious bug fixed 1.1.1997 with the following kludgous overwrite
// of the method gradate_for_svs(). Because verb 'jaella' has only the
// part 'ja' listed as its stem in the database, Weak2Strong doesn't
// convert it to jak, as it should. Instead, we have to temporarily
// add the missing 'e' before converting it to 'jake', and then remove
// the 'e' again from the end of the stem.
// This is cumbersome. It would be better if the stems were in the database
// two letters longer (ajatel, katsel, jael) and then we wouldn't need
// the next two methods at all!
   virtual void gradate_for_svs()
    {
      if(gradationp())
       { // ja+e = jae;  jae -> jake;  jake -> jak
         addchar('e'); Weak2Strong(); deletelastchar();
       }
    }
   virtual void add2missing_letters() { addstring(toUstr("el")); }
   virtual void get_abstract_noun()
    { gradate_for_svs(); addstring(toUstr("elU")); } // ajattelu
 };

class nss29 : public nss25to29 /* arva+illa */
 {
public:
   virtual void add2missing_letters() { addstring(toUstr("il")); }
   virtual void get_abstract_noun() { addstring(toUstr("ilU")); }
 };

class nss31 : public nss24to34except30and33 /* val+ita */
 {
public:
   virtual void get0infstem() { addstring(toUstr("it")); }
   virtual void get_svs()
    { gradate_for_svs(); addstring(toUstr("itse")); }
   virtual void get_abstract_noun()
    { gradate_for_svs(); addstring(toUstr("intA")); }
// Not correct: havainta*, sijainta*, ravinta*, tuominta*, palkinta?
 };

class nss34 : public nss24to34except30and33 /* vaie+ta, hapa+ta */
 {
// These verbs are formed from adjectives, and it's not possible to find
// any sensible abstract noun for them.
// Forms like heikennys, jyrkennys and mädännys make more sense
//  as abstract nouns of verbs heikentää, jyrkentää and mädäntää,
//  not of the verbs heiketä, jyrketä and mädätä.
public:
   virtual void get0infstem() { addchar('t'); }
   virtual void get_svs()
    { gradate_for_svs(); addstring(toUstr("ne")); }
 };

class ContractingVerbs : public Class2Verbs
 {
public:
// An insidious bug fixed 1.1.1997 with the following kludgous overwrite
// of the method gradate_for_svs().
// Now should handle the verbs maata 50 maa, taata 50 taa, and koota 56 koo
// correctly. (NSS classes 35, 35 and 38 respectively).
// Weak2Strong doesn't gradate the stems with a long vowel, so we have
// to do it explicitly here.
   virtual void gradate_for_svs()
    {
      if(gradationp())
       {
         Uchar vika = lastchar();
         if(vika == penultchar()) // Check whether we have maa, taa or koo?
          { changelastchar('k'); addchar(vika); } // to maka, taka or koko
         else { Weak2Strong(); }
       }
    }
   virtual void get0infstem() { addchar('t'); }
   virtual void get_svs()
    { gradate_for_svs(); addchar(A); }
   virtual void get_sis() { gradate_for_svs(); addstring(toUstr("si")); }
   virtual void get_wis() { get_sis(); }
   virtual void get_agential_noun()
    { this->get_svs(); addchar('j'); addchar(A); } // hakkaaja, kampeaja
 };

class nss35 : public ContractingVerbs // ahda+ta, evä+tä, maa+ta, arva+ta
 { // noteera+ta
   virtual void get_cs()
    { get_svs(); changelastchar('i'); addstring(toUstr("si")); }
   virtual void get_abstract_noun()
    { gradate_for_svs(); addstring(toUstr("Us")); } // ahtaus,epäys,makaus
// Not correct: kehtaus*,lepäys* (lepo),pelkäys* (pelko),
// heräys*, matkaus*, murhaus*, omaus*, palaus* (paluu), stressaus*
// elokuvaus? although valokuvaus is correct form.
 };

class nss36 : public ContractingVerbs
 {
public:
   virtual void add1missing_letters() { addchar('e'); }
 };

class nss37 : public ContractingVerbs
 {
public:
   virtual void add1missing_letters() { addchar('i'); }
 };

class nss38 : public ContractingVerbs
 {
   virtual void get_abstract_noun()
    { gradate_for_svs(); addstring(toUstr("Us")); } // kokous,kirous,verhous
// Most of these are very dubious!
 };

class nss39 : public ContractingVerbs // kavu+ta,liidu+ta,halu+ta,höyry+tä
 {
// These are mainly formed from more or less concrete nouns.
   virtual void get_abstract_noun() // Give that back as an "abstract noun"
    { gradate_for_svs(); } // kapu?, liitu, halu, höyry (Some dubious ones)
 };

class nss40 : public nss35
 {
 };


static nss2  NSS2;
static nss3  NSS3;
static nss4  NSS4;
static nss5  NSS5;
static nss6  NSS6;
static nss7  NSS7;
static nss8  NSS8;
static nss9  NSS9;
static nss10 NSS10;
static nss11 NSS11;
static nss12 NSS12;
static nss13 NSS13;
static nss14 NSS14;
static nss15 NSS15;
static nss16 NSS16;
static nss17 NSS17;
static nss18 NSS18;
static nss19 NSS19;
static nss20 NSS20;
static nss21 NSS21;
static nss22 NSS22;
static nss23 NSS23;
static nss24 NSS24;
static nss25 NSS25;
static nss26 NSS26;
static nss27 NSS27;
static nss28 NSS28;
static nss29 NSS29;
static nss30 NSS30;
static nss31 NSS31;
static nss32 NSS32;
static nss33 NSS33;
static nss34 NSS34;
static nss35 NSS35;
static nss36 NSS36;
static nss37 NSS37;
static nss38 NSS38;
static nss39 NSS39;
static nss40 NSS40;
static nss41 NSS41;
static nss42 NSS42;
static nss43 NSS43;
static nss44 NSS44;
static nss24seista NSS45;
static nss25olla NSS46;

FVC *VerbClassPtrs[] =
 {
   NULL,&NSS1,&NSS2,&NSS3,&NSS4,&NSS5,&NSS6,&NSS7,&NSS8,&NSS9,
   &NSS10,&NSS11,&NSS12,&NSS13,&NSS14,&NSS15,&NSS16,&NSS17,&NSS18,&NSS19,
   &NSS20,&NSS21,&NSS22,&NSS23,&NSS24,&NSS25,&NSS26,&NSS27,&NSS28,&NSS29,
   &NSS30,&NSS31,&NSS32,&NSS33,&NSS34,&NSS35,&NSS36,&NSS37,&NSS38,&NSS39,
   &NSS40,&NSS41,&NSS42,&NSS43,&NSS44,&NSS45,&NSS46
 };


extern Uint get_nssclass(Uint type)
{
    return(verbclassflags[(type<<1)]);
}

extern Uchar *conjugate(Uchar *result,Uchar *stem,Uchar type,Uint flags)
{
    Otype = type;
    Flags = flags;
    Result = result;
    Rp = (Result-1);

    NSS_Class = get_nssclass(type);

    ClassPtr = VerbClassPtrs[NSS_Class];

    switch(get_vharmonymask())
     {
       case ALWAYS_BACK:  { Backf = 1; break; }
       case ALWAYS_FRONT: { Backf = 0; break; }
       case USE_BACKHARMOP: { Backf = back1harmop(stem); break; }
       case LAST_OF_STEM:   { Backf = back2harmop(stem); break; }
     }

    A = (Backf ? 'a' : 'ä');
    O = (Backf ? 'o' : 'ö');
    U = (Backf ? 'u' : 'y');

    if(nonfinitep())
     {
       addstring(stem); /* Add supplied stem to Result. */
       ClassPtr->add1missing_letters();
       switch(get_nonfinite_flags())
        {
          case INFINITIVE1:   { ClassPtr->conj_infinitive1(); break; }
          case INFINITIVE2:   { ClassPtr->conj_infinitive2(); break; }
          case INFINITIVE3:   { ClassPtr->conj_infinitive3(); break; }
          case INFINITIVE4:   { ClassPtr->conj_infinitive4(); break; }
          case INFINITIVE5:   { ClassPtr->conj_infinitive5(); break; }
          case PART1ACT:      { ClassPtr->conj_part1act(); break; }
          case PART1PAS:      { ClassPtr->conj_part1pas(); break; }
          case PART2ACT:      { ClassPtr->conj_part2act(); break; }
          case PART2PAS:      { ClassPtr->conj_part2pas(); break; }
          case AGENTIAL_NOUN: { ClassPtr->conj_agential_noun(); break; }
          case NEGATIVE_ADJ:  { ClassPtr->conj_negative_adj(); break; }
          case ABSTRACT_NOUN: { ClassPtr->conj_abstract_noun(); break; }
        }
     }
    else
     {
       ClassPtr->get_auxiliaries();

       // If get_auxiliaries detected invalid flags combination, then
       //  return immediately:
       if(lastchar() == '-') { addchar('\0'); return(result); }

       addstring(stem); /* Add supplied stem to Result. */
       ClassPtr->add1missing_letters();

       if(passivep()) { ClassPtr->conj_passive(); } // Otherwise it's active.
       else if(sectensep()) // Secondary tense? Just add Past Active Participle
        { // because get_auxiliaries() has done all the rest.
          ClassPtr->get_part2act();
        }
       else if(presindicp())   { ClassPtr->conj_present_indicative(); }
       else if(imperfectp())   { ClassPtr->conj_imperfect_indicative(); }
       else if(imperativep())  { ClassPtr->conj_present_imperative(); }
       else if(conditionalp()) { ClassPtr->conj_present_conditional(); }
       else if(potentialp())   { ClassPtr->conj_present_potential(); }
     }

    addchar('\0'); // Terminate the result string.
    return(result); // And return it.
}

