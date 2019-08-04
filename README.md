# Test code for investigating possible bug in GFORTRAN compiling code generated by SFTRAN3 pre-processor

Usage:

    make run

    make run MYFFLAGS=

Both commands create and run two executables:  one built by G77; one built by GFORTRAN

The former command [make run] will produce executables that are both successful.

The latter command [make run MYFFLAGS=] will produce a successful executable with G77, but an unsuccessful executable with GFORTRAN.

In all cases, the code appears to be semantically correct, but in the latter case GFORTRAN generates an executable that returns .TRUE. from logical function NCSCAN when it should return .FALSE.  The flow of the program looks like the following; the key point is the [IF (.NOT.(NCSCAN)) GO TO 20014] statement, which transfers control to 20014 when NCSCAN is .FALSE., which control is then transferred to 20009, which in turn then returns control to the caller, but it returns .TRUE. instead of the correct .FALSE. value when GFORTRAN is the compiler.  Replacing the [GO TO 200014] with [RETURN] makes GFORTRAN compile an executable that runs correctly.

    LOGICAL FUNCTION NCSCAN(...)

          [...]

          GOTO 30002

    20009 RETURN

    30002 [...]

          NCSCAN = [expression that evaluates to .TRUE. or .FALSE.

          IF (.NOT.(NCSCAN)) GO TO 20014

          [...]
    20014 GO TO 20009

There are four FORTRAN source files in this repository:  bbpas1.f is the main program; ncscan.f contains the code that GFORTRAN compiles incorrectly; batop2.f and nncmpr.f are support files.  Comments in the code explain what is happening; the original sources are from the SFTRAN3 pre-processor itself, and the executable tries to pre-process a single PARAMETER statement, but the details are not important.  What is important to note is that G77 compiles an executable that works correctly, and GFORTRAN does not.


## Expected results

### Running with macro in place so GFORTRAN executable works correctly

    > make run
    g77 -x f77-cpp-input -g -O0 -finit-local-zero -fno-automatic -DARGRTN=1 bbpas1.f batop2.f ncscan.f nncmpr.f -o y_g77.e -B/usr/lib/x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/5
    gfortran -cpp -std=legacy -g -O0 -finit-local-zero -fno-automatic -DARGRTN=1 bbpas1.f batop2.f ncscan.f nncmpr.f -o y_gfortran.e

    ./y_g77.e
     OKAY:  NCSCAN says [PARAME].NE.[SFIELD]

    ./y_gfortran.e
     OKAY:  NCSCAN says [PARAME].NE.[SFIELD]

### Running with macro disabled so GFORTRAN executable fails

    > make run MYFFLAGS=
    g77 -x f77-cpp-input -g -O0 -finit-local-zero -fno-automatic  bbpas1.f batop2.f ncscan.f nncmpr.f -o y_g77.e -B/usr/lib/x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/5
    gfortran -cpp -std=legacy -g -O0 -finit-local-zero -fno-automatic  bbpas1.f batop2.f ncscan.f nncmpr.f -o y_gfortran.e

    ./y_g77.e
     OKAY:  NCSCAN says [PARAME].NE.[SFIELD]

    ./y_gfortran.e
     FAIL:  NCSCAN says [PARAME].EQ.[SFIELD]
    STOP NCSCAN indicates MATCH when there should be none


