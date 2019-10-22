; если в строке после then что-то есть, то это так сказать однострочный if т.е. который оканчивается в конце строки а не на Endif.

    If True Then
       If True Then
          If True Then Exit
          Else
          If True Then Exit
       EndIf
    EndIf
    