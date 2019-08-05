      logical function zero()
      goto 00002
00001 return
00002 zero = .false.
      if (.not.zero) goto 00001
      return
      end
      program test_zero
      logical zero
      if (zero()) stop 'FAIL:  zero() returned .TRUE.'
      stop 'OKAY:  zero() returned .FALSE.'
      end
