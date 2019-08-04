      program BBPAS1
      implicitnone

C     ------------------------------------------------------------------
C
C SIMULATE FINDING SFTRAN STATEMENTS
C     Method of doing initial scan, for first non-blank token at or
C     after column 7, will be to look for one of 21 keywords using
C     hashing to cut down on the amount of searching.
C
C          The first two letters of the 21 keywords are members of the
C     following set of 16 letters.  These 16 letters are assigned hash
C     values as follows:
C          A=7,  C=2,  D=3,  E=4, F=5,  I=15, L=9, N=8, 0=1, P=10,
C          R=11, S=12, T=14, W=13, X=6, Y=16.
C
C          There are 16 different initial pairs of letters in the 21
C     keywords.  We assign a Case no. from 1 to 16 to these possible
C     pairs.  Each of these 16 possible pairs has a different hash value
C     obtained by summing the hash values given above for the individual
C     letters.  This information is summarized in the following table:
C
C        Hash    Case   Index in   Keyword
C        value   no.    KEYTAB()
C
C          16      1        1      ALLOCATE
C           9      2        9      CASE
C           3      3       13      COMMON
C          18      4       19      CYCLE
C           4      5       24      DO
C          19      6       26      DYNAMIC
C          13      7       33      ELSE     or ELSEIF
C          12      8       37      EN       =>  END or ENTRY
C          10      9       39      EXIT
C          20     10       43      IF
C          21     11       45      PROCEDURE
C          15     12       54      RE       =>  READ, RELEASE, or RETURN
C          17     13       56      SFIELD
C          26     14       62      ST     =>  STATIC, STOP, or STRUCTURE
C          24     15       64      WRITE
C                          69      First unused index.
C
C     This subprogram will handle any coding of characters that are
C     represented by integers in the range 0:255.  This EXCLUDES certain
C     antique CDC systems that used 6 or 12 bit codes to represent lower
C     case letters.
C
C     There is a potential portability problem lurking here, in that we
C     use an array having lower case letters in it, but the Fortran-77
C     standard does not require the processor to admit them as members
C     of character constants.
C
C          Let SUM denote the sum of the hash values, from LETVAL(), for
C     the first 2 chars of a statement.  If SUM is .le. 26 it will be
C     mapped to a Case no. using the array HACA().  If the Case no. is
C     nonzero the program will proceed to a detailed analysis of that
C     case.
C          The array, POS1() maps case nos. to indices in the array,
C     KEYTAB(), that contains the character representations of the
C     keywords.
C                            Common variables
C
C CCMORE in C04. (In/Out, integer)  Used by subr NCSCAN.
C CMPMAP in C28. (In, integer) Used to map both A..Z and a..z onto 1..26
C
C     ------------------------------------------------------------------
C                          Local variables
C
C LETVAL specifies the hash value to which a letter is to be mapped, for
C        purposes of hashing the keyword table.
C ML     is the mapping for a letter that does not appear in any
C        keyword.  ML should be large, so HASH will be large if such a
C        letter appears.
C
C     ------------------------------------------------------------------
C
      LOGICAL NCSCAN
      INTEGER CASENO, HACA(0:32), HASH
      INTEGER ICODE, LETVAL(0:26)
      INTEGER P1, POS1(16)
      CHARACTER*1 KEYTAB(68)
      integer ml
      PARAMETER (ML=66)
      INTEGER NS,MAXNS
      PARAMETER( MAXNS=1326)
      CHARACTER  STMT(1334)
      INTEGER BYTE,CC,CCMORE,LCCHR
      CHARACTER*1 COMCHR(2)
      LOGICAL MORE
      INTEGER SCAN, FIND, LOCATE
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

C     Local loop counters
      INTEGER I
      INTEGER J

C     ------------------------------------------------------------------

      COMMON /C02/ NS
      COMMON /CC02/ STMT
      COMMON /C04/ BYTE,CC,CCMORE,LCCHR,MORE
      COMMON /CC04/ COMCHR
      COMMON /C20/ SCAN, FIND, LOCATE
      COMMON /C28/ CMPMAP

C     ------------------------------------------------------------------

      DATA HACA /  0,  0,  0,  3,  5,  0,  0,  0,  0,  2,  9,
     *             0,  8,  7,  0, 12,  1, 13,  4,  6, 10,
     *            11,  0,  0, 15,  0, 14,  0,  0,  0,  0,  0,  0/

      DATA  KEYTAB /'A','L','L','O','C','A','T','E',
     * 'C','A','S','E',    'C','O','M','M','O','N',
     * 'C','Y','C','L','E',   'D','O',   'D','Y','N','A','M','I','C',
     * 'E','L','S','E',   'E','N',   'E','X','I','T',   'I','F',
     * 'P','R','O','C','E','D','U','R','E',   'R','E',
     * 'S','F','I','E','L','D',   'S','T',   'W','R','I','T','E'   /

      DATA LETVAL /ML,7,ML,2,3,4,5,ML,ML,15,ML,ML,9,ML,8,1,10,ML,11,12
     *,            14,ML,ML,13,6,16,ML/

      DATA POS1 / 1, 9, 13,19,24,26,33,37,39,43,45,54,56,62,64,69/

C     ------------------------------------------------------------------

C     Synthetic data for typical 25-character line from FORTRAN source:
C
C     PARAMETER ( P = 1 )
C234567890123456789012345

C     Fill with 1309 spaces to 1334 characters:

      DATA STMT / ' ', ' ', ' ', ' ', ' ', ' '
     &          , 'P', 'A', 'R', 'A', 'M', 'E', 'T', 'E', 'R'
     &          , ' ', '(', ' ', 'P', ' ', '=', ' ', '1', ' ', ')'
     &          , 1309*' '
     &          /

C     Set parameters that locate the first token i.e. PARAMETER in that
C     line
      DATA BYTE / 0 /
      DATA CC / 7 /
      DATA CCMORE / 7 /
      DATA MORE / .TRUE. /
      DATA NS / 25 /

C     Set SFTRAN3 comment
      DATA COMCHR / '!', ' ' /
      DATA LCCHR / 1 /

C     Set FIND argument that will be passed to NCSCAN
      DATA FIND / 2 /

C     ------------------------------------------------------------------

C     Initialize CMPMAP; used both for case-insensitive comparisons and
C     for hash calculation
      CALL BATOP2

C     Build hash from first two characters of token in STMT as position
C     CCMORE i.e. from STMT(CCMORE) and STMT(CCMORE+1)
      HASH = 0
      DO I = 1, 0, -1
         ICODE = MAX(CMPMAP(ICHAR(STMT(CCMORE+I))),0)
         HASH = HASH + LETVAL(ICODE)
      ENDDO

      IF (HASH.GE.ML) HASH = 0

C     PArameter:  P=10; A=7; HASH=10+7=17
      IF(HASH .NE. 17) THEN
         STOP 'HASH is not 17'
      ENDIF

C     At this point, 2 <= HASH <= 32 if valid, else HASH = 0.
C     N.B. in this test case, HASH=17 and CASENO should be 13
      CASENO = HACA(HASH)

C     HASH of 17 => HACA(17) => 13 (SField:  S=12; F=5; HASH=12+5=17)
      IF(CASENO .NE. 13) THEN
         STOP 'CASENO = HACA(HASH) is not 13'
      ENDIF

C     Get position of first character of token, in this case SFIELD, in
C     KEYTAB, that has same HASH as PA[RAMETER]
      P1 = POS1(CASENO)

C     Compare word SFIELD from KEYTAB(POS1(CASENO):POS1(CASENO+1)-1) to
C     PARAME, which is STMT(CCMORE:CCMORE+6-1) at beginning of PARAMETER
      IF( .NOT. NCSCAN(FIND, KEYTAB(P1), POS1(CASENO+1) - P1) )THEN

         print'(1x,20hOKAY:  NCSCAN says [,6a1,1h],5h.NE.[,6a1,1h])'
     &   ,(STMT(CCMORE+I-1), I=1,6)
     &   ,(KEYTAB(P1+J-1), J=1,6)

      ELSE

         print'(1x,20hFAIL:  NCSCAN says [,6a1,1h],5h.EQ.[,6a1,1h])'
     &   ,(STMT(CCMORE+I-1), I=1,6)
     &   ,(KEYTAB(P1+J-1), J=1,6)
         STOP 'NCSCAN indicates MATCH when there should be none'

      END IF

      END
