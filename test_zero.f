      logicalfunctionzero()
      goto00002
00001 return
00002 zero=.false.
      print*,zero,' = zero from inside callee zero'
      if (.not.zero) goto00001
      end
      programtest_zero
      logicalzero
      logicallcl_zero
      lcl_zero = zero()
      print*,lcl_zero,' = lcl_zero<=zero() from caller test_zero'
      end
