
/*
   conjugate - verb conjugation routines in Lisp, C and C++
   
   Copyright (C) (currently 1993-1994) by Antti Karttunen.

   References:

   Eugene Holman: Handbook of Finnish Verbs, published by SKS,
                  ISBN 951-717-362-8 (ISSN 0355-1768)

   Fred Karlsson: Finnish Grammar, published by WSOY.
 */

/*
   conjugate is the function which gives the required form of verb
   when it is supplied the stem of verb, it's classification type
   and the flags which define how it should be conjugated.

   result  should be pointer to a buffer where the resulting string
           is copied. It's also returned as the result of conjugate function.
   stem    should be the original stem of the verb, just as it is stored
           in the verbfile.
   type    should be the classification type of the verb. (It is stored
           in the verb file together with stem.) We use classification
           system which is modified from the NSS (Nykysuomen Sanakirja)
           system. (However, type is internally converted to NSS class).
   flags   should be a 16-bit integer, whose bits determine how the
           stem is conjugated. In this file we define the constants
           which you can use in the calling function to get the
           desired form of the verb.
           These can be ored (or plussed) together, e.g. with stem "luk"
           these should produce:
           POTENTIAL+PERS3            -> lukenee
           IMPERATIVE+PASSIVE         -> luettakoon
           NEGATIVE+PERFECT+CONDITIONAL+PERS2+PLURPERS -> ette olisi lukeneet
           INFINITIVE2+INESSIVE+PS_NI -> lukiessani
           PART1ACT+ALLATIVE+PLURCASE -> lukeville
 */

/* This converts external type (which is used in verbfile) to internal
    NSS_Class type: */
extern Uint get_nssclass(Uint type);

extern Uchar *conjugate(Uchar *result,Uchar *stem,Uchar type,Uint flags);


/* Definitions for flags which define how the verb should be conjugated: */

/* With Finite forms:

   7 6 5 4 3 2 1 0

   V!TEN!MO !PER!N
   E!SE ! OD!SON!U
   R!   !   !   !M
   I!   !   !   !B
   D!   !   !   !E
   I!   !   !   !R (of PERSON)
   C!   !   !   !
   T!   !   !   !
   I!   !   !   !
   O!   !   !   !
   N!   !   !   !

 */

#define PERSNUM      0001 /* Number of Person (Singular or plural person) */
#define PERSON       0006
#define MOOD         0030
#define TENSE        0140
#define VERIDICTION  0200

#define TENSE_MOOD    (TENSE+MOOD)
#define PERSON_NUMBER (PERSON+PERSNUM)

#define SINGPERS     0000
#define PLURPERS     0001

#define PASSIVE      0000
#define PERS1        0002
#define PERS2        0004
#define PERS3        0006

#define INDICATIVE   0000
#define IMPERATIVE   0010
#define CONDITIONAL  0020
#define POTENTIAL    0030

#define PRESENT      0000
#define IMPERFECT    0040
#define PERFECT      0100
#define PLUPERFECT   0140

#define AFFIRMATIVE  0000
#define NEGATIVE     0200

#define PRESENT_INDICATIVE (PRESENT+INDICATIVE)

#define get_number()         (Flags&PERSNUM)
#define get_person()         (Flags&PERSON)
#define get_person_number()  (Flags&PERSON_NUMBER)
#define get_mood()           (Flags&MOOD)
#define get_tense()          (Flags&TENSE)
#define get_tense_mood()     (Flags&TENSE_MOOD)
#define get_veridiction()    (Flags&VERIDICTION)

#define plurpersp()    (get_number() == PLURPERS)
#define singpersp()    (!plurpersp())

#define passivep()     (!get_person())

#define firstpersp()   (get_person() == PERS1)
#define secondpersp()  (get_person() == PERS2)
#define thirdpersp()   (get_person() == PERS3)

#define indicativep()  (get_mood() == INDICATIVE)
#define presindicp()   (get_tense_mood() == PRESENT_INDICATIVE)
#define imperativep()  (get_mood() == IMPERATIVE)
#define conditionalp() (get_mood() == CONDITIONAL)
#define potentialp()   (get_mood() == POTENTIAL)

#define presentp()     (get_tense() == PRESENT)
#define imperfectp()   (get_tense() == IMPERFECT)
#define perfectp()     (get_tense() == PERFECT)
#define pluperfectp()  (get_tense() == PLUPERFECT)

/* Secondary tense? (Compound tense) I.e. PERFECT or PLUPERFECT? */
#define sectensep()    (get_tense()&PERFECT)

/* Primary tense? (Simple tense) I.e. PRESENT or IMPERFECT? */
#define pritensep()    !sectensep()

#define negativep()    (get_veridiction() == NEGATIVE)
#define affirmativep() !negativep()


/* With Non-finite forms: (i.e. infinitives, participles and nouns)

   7 6 5 4 3 2 1 0

   S!C A S E!POSS.!
   I!       !SUFF.!
   N!       !     !
   G!       !     !
   /!       !     !
   P!       !     !
   L!       !     !
   U!       !     !
   R!       !     !
   A!       !     !
   L!       !     !

 */

#define POSS_SUFFIX    0007
#define NOMINAL_CASE   0170
#define NUMBER_OF_CASE 0200

#define SINGCASE       0000
#define PLURCASE       0200

#define NONFINITE    0xFF00

/* Definitions for the possessive suffixes. Note that these are actually
   analogous to person&number of the finite forms, and in the most cases
   should be used in concordance with them.
   E.g: olen ostavinani, olet ostavinasi, olemme ostavinamme, on ostavinaan
   Some macros are actually used both for poss_suff bits and pers&num bits,
   e.g. thirdpersp() in some occassions.
 */
#define PS_NI             2
#define PS_MME            3
#define PS_SI             4
#define PS_NNE            5
#define PS_NSA            6

/* If high byte is non-zero, then non-finite form is requested: */
#define INFINITIVE1   0x100 /* lukea (or lukeakse/ni/si/en/mme/nne) */
#define INFINITIVE2   0x200 /* lukiessa, lukien, luettaessa */
#define INFINITIVE3   0x300 /* lukema (same as agential adjective) */
#define INFINITIVE4   0x400 /* lukeminen */
#define INFINITIVE5   0x500 /* lukemaisilla/ni/si/an/mme/nne */
#define PART1ACT      0x600 /* Present Active Participle          lukeva */
#define PART1PAS      0x700 /* Present Passive Participle         luettava  */
#define PART2ACT      0x800 /* Perfect (Past) Active Participle   lukenut */
#define PART2PAS      0x900 /* Perfect (Past) Passive Participle  luettu */
#define AGENTIAL_NOUN 0xA00 /* lukija */
#define NEGATIVE_ADJ  0xB00 /* lukematon */
#define ABSTRACT_NOUN 0xC00 /* luku */


#define get_poss_suffix()     (Flags&POSS_SUFFIX)
#define get_nominal_case()    (Flags&NOMINAL_CASE)
#define get_number_of_case()  (Flags&NUMBER_OF_CASE)
#define get_nonfinite_flags() (Flags&NONFINITE)

#define get_case_index() (get_nominal_case()>>3)

#define plurcasep()      (get_number_of_case())
#define singcasep()      (!plurcasep())

#define nonfinitep()     get_nonfinite_flags()

#define infinitive1p()   (get_nonfinite_flags() == INFINITIVE1)
#define infinitive2p()   (get_nonfinite_flags() == INFINITIVE2)
#define infinitive3p()   (get_nonfinite_flags() == INFINITIVE3)
#define infinitive4p()   (get_nonfinite_flags() == INFINITIVE4)
#define infinitive5p()   (get_nonfinite_flags() == INFINITIVE5)
#define part1actp()      (get_nonfinite_flags() == PART1ACT)
#define part1pasp()      (get_nonfinite_flags() == PART1PAS)
#define part2actp()      (get_nonfinite_flags() == PART2ACT)
#define part2pasp()      (get_nonfinite_flags() == PART2PAS)
#define agential_nounp() (get_nonfinite_flags() == AGENTIAL_NOUN)
#define negative_adjp()  (get_nonfinite_flags() == NEGATIVE_ADJ)

#define NOMINATIVE     (0<<3)
#define GENITIVE       (1<<3)
#define PARTITIVE      (2<<3)
#define ACCUSATIVE     (3<<3)
#define TRANSLATIVE    (4<<3)
#define ESSIVE         (5<<3)
#define INESSIVE       (6<<3)
#define ELATIVE        (7<<3)
#define ILLATIVE       (8<<3)
#define ADESSIVE       (9<<3)
#define ABLATIVE      (10<<3)
#define ALLATIVE      (11<<3)
#define ABESSIVE      (12<<3)
#define INSTRUCTIVE   (13<<3)
#define COMITATIVE    (14<<3)
#define PROLATIVE     (15<<3)

#define nominativep()   (get_nominal_case() == NOMINATIVE)
#define genitivep()     (get_nominal_case() == GENITIVE)
#define partitivep()    (get_nominal_case() == PARTITIVE)
#define accusativep()   (get_nominal_case() == ACCUSATIVE)
#define translativep()  (get_nominal_case() == TRANSLATIVE)
#define essivep()       (get_nominal_case() == ESSIVE)
#define inessivep()     (get_nominal_case() == INESSIVE)
#define elativep()      (get_nominal_case() == ELATIVE)
#define illativep()     (get_nominal_case() == ILLATIVE)
#define adessivep()     (get_nominal_case() == ADESSIVE)
#define ablativep()     (get_nominal_case() == ABLATIVE)
#define allativep()     (get_nominal_case() == ALLATIVE)
#define abessivep()     (get_nominal_case() == ABESSIVE)
#define instructivep()  (get_nominal_case() == INSTRUCTIVE)
#define comitativep()   (get_nominal_case() == COMITATIVE)
#define prolativep()    (get_nominal_case() == PROLATIVE)
