primitive Platform
  fun bsd(): Bool => freebsd() or dragonfly() or openbsd()
  fun freebsd(): Bool => compile_intrinsic
  fun dragonfly(): Bool => compile_intrinsic
  fun openbsd(): Bool => compile_intrinsic
  fun linux(): Bool => compile_intrinsic
  fun osx(): Bool => compile_intrinsic
  fun posix(): Bool => bsd() or linux() or osx()
  fun windows(): Bool => compile_intrinsic

  fun x86(): Bool => compile_intrinsic
  fun arm(): Bool => compile_intrinsic

  fun lp64(): Bool => compile_intrinsic
  fun llp64(): Bool => compile_intrinsic
  fun ilp32(): Bool => compile_intrinsic

  fun bigendian(): Bool => compile_intrinsic
  fun littleendian(): Bool => compile_intrinsic

  fun native128(): Bool => compile_intrinsic
  fun debug(): Bool => compile_intrinsic
