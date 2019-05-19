program test_fcl_kinds

  use fcl_kinds

  implicit none

  print *, "Real kinds"
  print '(a10, i3)', "Default:", kind(0.0)
  print '(a10, i3)', "SP:", sp
  print '(a10, i3)', "DP:", dp
  print '(a10, i3)', "QP:", qp

  print *, "Int kinds"
  print '(a10, i3)', "Default:", kind(0)
  print '(a10, i3)', "i8" , i8 
  print '(a10, i3)', "i16", i16
  print '(a10, i3)', "i32", i32
  print '(a10, i3)', "i64", i64

end program test_fcl_kinds