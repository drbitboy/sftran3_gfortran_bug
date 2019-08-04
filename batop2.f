      SUBROUTINE BATOP2
      implicitnone
      integer I
      character*26 LCLETS, UCLETS
C CMPMAP is indexed by the ordinal value of a character, that is,
C        ICHAR(C).  If two values of CMPMAP are equal, the characters
C        that indexed the two equal values are to be considered to be
C        equal.  Furthermore, the elements of CMPMAP that are indexed by
C        the ordinal values of letters are positive, while others are
C        negative.  This all assumes ASCII i.e. 256-character set
      INTEGER CMPMAP(0:255)
      COMMON /C28/ CMPMAP
      data LCLETS /'abcdefghijklmnopqrstuvwxyz'/
      data UCLETS /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/

C     ------------------------------------------------------------------

C     Initialize all values to negative of the index
C     N.B. letters will be overridden below
      DO 20001 I = 0,255
         CMPMAP(I) = -I
20001    CONTINUE

C     Initialize english alphabet letters to their alphabetic positions,
C     so mapped values for lower- and upper-case versions of the same
C     letter will match
      DO 20005 I = 1, 26
         CMPMAP(ichar(UCLETS(I:I))) = I
         CMPMAP(ichar(LCLETS(I:I))) = I
20005    CONTINUE

      RETURN
      END        
