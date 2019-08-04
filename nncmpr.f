      LOGICAL FUNCTION NNCMPR (A, I, B, J, N)
C     This subprogram is designed to run on any machine that uses
C     integers in the range 0..255 to represent characters.  This
C     EXCLUDES certain antique CDC systems that use 6 and 12 bit codes
C     to represent lower case letters.
C
C     Compare two Fortran 77 character strings of length N.  Return
C     .TRUE. if they are equal, and .FALSE. if they are unequal.
C
C     Characters from the two strings are deemed to be equal if they are
C     the same character, or if they are both letters and one is upper
C     case while the other is lower case.
C
C     One string begins in the array A at location I, and the other
C     begins in the array B at location J.
C
C       A  contains the first character string.
C       B  contains the second character string.
C       I  is the position in A at which the first string begins.
C       J  is the position in B at which the second string begins.
C       N  is the length of strings to compare.

      CHARACTER A(*), B(*)
      INTEGER I, J, N
C
C     *****     Variables Used     *************************************
C
C       ACH  is a character from the string in A.
C       AIX  is the index into the string in A.
C       BCH  is a character from the string in B.
C       BIX  is the index into the string in B.
C       L    is the loop counter.

      CHARACTER ACH, BCH
      INTEGER AIX, BIX, L

C CMPMAP  is used to indicate whether two characters are to be
C         considered to be the same.  For characters that must match
C         exactly, CMPMAP(I) = -I, where I = ICHAR(C).  If two
C         characters can be the same, as for example the lower and upper
C         case instances of the same letter, simply make MAP(I) = J for
C         both of them, where J is the position of the letter in the
C         alphabet (starting at 1 for "a").  This mapping is used in
C         DDLABL to generate a case selector.

C CMPMAP is indexed by the ordinal value of a character, that is,
C        ICHAR(C).  If two values of CMPMAP are equal, the characters
C        that indexed the two equal values are to be considered to be
C        equal.  Furthermore, the elements of CMPMAP that are indexed by
C        the ordinal values of letters are positive, while others are
C        negative.  Since CMPMAP is indexed by the ordinal value or a
C        character, it might be necessary to change the dimension of
C        CMPMAP on some antique CDC systems that use 6 or 12 bit codes
C        to represent lower case letters and some punctuation marks.
C        CMPMAP is set in BATOP2.

      INTEGER CMPMAP(0:255)
      COMMON /C28/ CMPMAP

C     *****     Executable Statements     ******************************

C     Copy I and J to local variables so callers I and J will not change
      AIX = I
      BIX = J

C     Loop over N characters in A and in B
      DO L = 1, N

C        Get characters at corresponding positions in A and B
         ach = a(AIX)
         bch = b(BIX)

C        Test if characters are a case-sensitive match
         IF(ach.ne.bch)THEN

C           If not a case-sensitive match, test if characters are a
C           case-insensitive match
            IF(cmpmap(ichar(ach)).ne.cmpmap(ichar(bch)))THEN

C              This character in A does not match the corresponding
C              character in B; return .FALSE. indicating no match
               NNCMPR = .FALSE.
               RETURN

            END IF
         END IF

C        The current characters match; move to next positions
         AIX=AIX+1
         BIX=BIX+1

      ENDDO

C     N characters in A starting at J match N characters in B starting
C     at J; return .TRUE. indicating a match
      NNCMPR = .TRUE.
      RETURN

      END
