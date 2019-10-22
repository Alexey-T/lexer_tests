' Syntax highlighting
Function PrintNumber
  Dim Number
  Dim X      34.4e+5

  Number = 123456
  Response.Write "The number is " & number
             
  For I = 0 To Number
    X = X + &h4c
    X = X - &o8
    X = X + 1.0
  Next

  I = I + @;  
End Function

' Syntax highlighting
Function PrintNumberAA
  Dim Number
  Dim X      34.4e+5

  Number = 123456
  Response.Write "The number is " & number
             
  For I = 0 To Number
    X = X + &h4c
    X = X - &o8
    X = X + 1.0
  Next

  I = I + @;  
End Function
