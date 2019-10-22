;--------------------------------

; The name of the installer
Name "Example1" aaa

; The file to write
OutFile "example1.exe"

; The default installation directory
InstallDir $DESKTOP\Example1

; Request application privileges for Windows Vista
RequestExecutionLevel user

section a
  file ff
section end

