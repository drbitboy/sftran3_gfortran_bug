      logical function NNCMPR (A, I, B, J, N)
c>> 1989-01-13 W. V. Snyder at JPL.  Replace CHCODE by CMPMAP.
c
c     This subprogram is designed to run on any machine that uses
c     integers in the range 0..255 to represent characters.  This
c     EXCLUDES certain antique CDC systems that use 6 and 12 bit codes
c     to represent lower case letters.
c
c     Compare two Fortran 77 character strings of length N.  Return
c     .TRUE. if they are equal, and .FALSE. if they are unequal.
c
c     Characters from the two strings are deemed to be equal if they are
c     the same character, or if they are both letters and one is upper
c     case while the other is lower case.
c
c     One string begins in the array A at location I, and the other
c     begins in the array B at location J.
c
c A       contains the first character string.
c B       contains the second character string.
c I       is the position in A at which the first string begins.
c J       is the position in B at which the second string begins.
c N       is the length of strings to compare.
c
      character A(*), B(*)
      integer I, J, N
c CMPMAP is indexed by the ordinal value of a character, that is,
c        ICHAR(C).  If two values of CMPMAP are equal, the characters
c        that indexed the two equal values are to be considered to be
c        equal.  Furthermore, the elements of CMPMAP that are indexed by
c        the ordinal values of letters are positive, while others are
c        negative.  Since CMPMAP is indexed by the ordinal value or a
c        character, it might be necessary to change the dimension of
c        CMPMAP on some antique CDC systems that use 6 or 12 bit codes
c        to represent lower case letters and some punctuation marks.
c        CMPMAP is set in BATOP2.
      INTEGER CMPMAP(0:255)
c
c     *****     Variables Used     *************************************
c
c ACH     is a character from the string in A.
c AIX     is the index into the string in A.
c BCH     is a character from the string in B.
c BIX     is the index into the string in B.
c L       is the loop counter.
c CMPMAP  is used to indicate whether two characters are to be
c         considered to be the same.  For characters that must match
c         exactly, CMPMAP(I) = -I, where I = ICHAR(C).  If two
c         characters can be the same, as for example the lower and upper
c         case instances of the same letter, simply make MAP(I) = J for
c         both of them, where J is the position of the letter in the
c         alphabet (starting at 1 for "a").  This mapping is used in
c         DDLABL to generate a case selector.
c
      character ACH, BCH
      integer AIX, BIX, L
      COMMON /C28/ CMPMAP
c
c     *****     Executable Statements     ******************************
c
      aix = i
      bix = j
      DO 20002 l = 1, n
         ach = a(aix)
         bch = b(bix)
         IF(ach.ne.bch)THEN
           IF(cmpmap(ichar(ach)).ne.cmpmap(ichar(bch)))THEN
               nncmpr = .FALSE.
               return
           END IF
         END IF
         aix=aix+1
         bix=bix+1
20002    CONTINUE
      nncmpr = .TRUE.
c
      return
c
      end                 
