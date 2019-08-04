      LOGICAL FUNCTION NCSCAN (IFUNC, STRING, N)
C     Performs scanning functions on the character string in
C     STMT() in COMMON /CC02/.
C     Does not change contents of STMT().
C     May change values of NS in /C02/ and CC, CCMORE and MORE in /C04/
C     Operation performed is selected by IFUNC. Only the case of IFUNC =
C     2 is valid here; the subr will return immediately with NCSCAN =
C     .FALSE. if MORE is false on entry.
C
C     IFUNC=2:Test for match of (STRING(I),I=1,N) with (STMT(I),I =
C             CCMORE, CCMORE+N-1). The test uses subprogram NNCMPR that
C             treats upper and lower case instances of the same letter
C             as equal. If match, set NCSCAN = true, then search
C             starting at CCMORE + N for next non-blank and set MORE
C             and CCMORE and NS as when IFUNC = 1.
C             If no match set NCSCAN = false.
C
C     Common Block    Variables used
C     ------------    --------------
C        C02          NS         End of current statement in STMT().
C                                Value of NS may be reduced by this
C                                subr if statement is found to end
C                                with blanks.
C
C        CC02         STMT()     Current statement being analysed.
C
C        C04          CC, CCMORE, LCCHR, MORE
C
C        CC04         COMCHR(2)  This is the inline comment token. Its
C                                length is 1 or 2 as given by LCCHR.
C
C     ------------------------------------------------------------------
C
C     Subprograms called: NNCMPR
C
C     ------------------------------------------------------------------
      INTEGER CCNEXT, CCSAVE
C     CHARACTER GIVEN, C1, STRING(*)
      CHARACTER STRING(*)
C     LOGICAL NNCMPR, QUOTED
      LOGICAL NNCMPR
C              DIMENSION STMT() FOR 1334 = 1326 + 1 + 6 + 1 CHARACTERS.
C                      MAXNS = 1326
      INTEGER NS,MAXNS
      PARAMETER( MAXNS=1326)
      CHARACTER  STMT(1334)
      INTEGER BYTE,CC,CCMORE,LCCHR
      CHARACTER*1 COMCHR(2)
      LOGICAL MORE
      COMMON /C02/ NS
      COMMON /CC02/ STMT
      COMMON /C04/ BYTE,CC,CCMORE,LCCHR,MORE
      COMMON /CC04/ COMCHR
C
C     MAIN LOGIC
C
C     Lobotomized for testing:  only IFUNC.EQ.2 is supported
      if (ifunc.ne.2) stop
      GO TO 30002

C     Jump target from below for RETURN
20009 GO TO 20003
20003 RETURN

C     ------------------------------------------------------------------
C
C     PROCEDURE (FIND)
C     TESTS FOR MATCH BETWEEN GIVEN STRING AND THE NEXT N BYTES
C     OF THE CURRENT STATEMENT.
C
C     OUTPUTS:
C        IF (MORE .AND. FIND) THEN
C           NCSCAN = .TRUE.
C           CC .=. BYTE WHERE SEARCH FOR MATCH BEGAN IN CURRENT STATEMEN
C           MORE, CCMORE, & NS AS PER PROC (SCAN FOR NON-BLANK)
C        ELSE
C           NCSCAN = .FALSE.
C           CC, MORE, CCMORE, & NS UNCHANGED
C        ENDIF
C
30002 NCSCAN = MORE .AND. (CCMORE+N-1.LE.NS)
     *              .AND. NNCMPR(STRING,1,STMT,CCMORE,N)

C     If no match -> 20014 (below) -> 20009 (below) -> 20003 (above) -> return
      IF (.NOT.(NCSCAN)) GO TO 20014

C     Start update of CCMORE and other pointers after scan for next
C     non-blank character in STMT
      CC=CCMORE
      CCNEXT=CCMORE+N

C     Setup jump back to here, then jump to scan
C     ASSIGN 20015 TO NPR005
      GO TO 30005

C     Jump back to here (NPR005 above), then 20009 -> 20003 -> return
20015 CONTINUE
20014 GO TO 20009
C
C     ------------------------------------------------------------------
C
C     PROCEDURE (SCAN FOR NON-BLANK)
C
C     GIVEN THAT CCNEXT.LE.NS, START AT STMT[CCNEXT] AND SEARCH UNTIL
C     EITHER A NON-BLANK IS FOUND OR THE STRING IS EXHAUSTED.  IF A
C     NON-BLANK IS FOUND AND IT IS NOT THE INLINE-COMMENT CHARACTER,
C     'MORE' IS SET TRUE AND 'CCMORE' IS ITS POSITION.  OTHERWISE,
C     'MORE' IS SET FALSE AND 'NS' IS SET TO THE STRING LENGTH.
C
C     INPUTS:
C        NS     = LENGTH OF 'STMT' STRING
C        CCNEXT = BYTE OF 'STMT' AT WHICH TO BEGIN SEARCH FOR NON-BLANK
C
C     DECISION TABLE:
C
C        CCNEXT  .LE. NS                *  F      T       T      T
C        STMT BLANK FROM CCNEXT ONWARD  *         T       F      F
C        1ST NON-BLANK IS COMMENT CHAR  *                 T      F
C                                       *
C        *************************************************************
C                                       *
C        MORE   . . . . . . . . . . . . * .F.    .F.     .F.    .T.
C        CCMORE = LOC OF NEXT NON-BLANK * N/C    N/C     N/C  CARD COL
C        NS     . . . . . . . . . . . . * N/C  CCNEXT-1  N/C    N/C
C
C        N/C => NO CHANGE
C
C
30005 MORE=.FALSE.
      IF (CCNEXT .GT. NS) GO TO 31005
      CCSAVE = CCNEXT
      DO 20057 CCNEXT = CCSAVE, NS
         IF (STMT(CCNEXT) .NE. ' ') GO TO 20056
20057    CONTINUE
      NS = MAX(CCSAVE-1,1)
      GO TO 31005
C
20056 IF(STMT(CCNEXT) .NE. COMCHR(1))THEN
        MORE = .TRUE.
      ELSEIF(LCCHR .EQ. 2)THEN
      IF(CCNEXT .EQ. NS)THEN
          MORE = .TRUE.
      ELSEIF(STMT(CCNEXT+1) .NE. COMCHR(2))THEN
          MORE = .TRUE.
      END IF
      END IF
      IF (MORE) CCMORE = CCNEXT
C1005 GO TO NPR005,(20012,20015,20037,20055)
31005 GO TO 20015
C
C     ------------------------------------------------------------------
C
      END
