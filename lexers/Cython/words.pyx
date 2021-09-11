class Name:
  n = 0b10+0xfe+0o30+1.2e3j
  def d1():
    pass
  cdef d2():
    pass
  cpdef d3():
    pass
  ctypedef d4():
    pass
  
(global|nonlocal|gil|nogil|extern|api|public|readonly|const(volatile)|inline)
(cimport|include|extern|import|from)
(elif|else|except|finally|for|if|try|while|with|break|continue|pass|raise|return|yield|IF|ELIF|ELSE|DEF)
(and|in|is|not|or)
(as|assert|by|del)
(NULL|None|True|False|Ellipsis|NotImplemented|UNAME_SYSNAME|UNAME_NODENAME|UNAME_RELEASE|UNAME_VERSION|UNAME_MACHINE|EXIT_FAILURE|EXIT_SUCCESS|RAND_MAX)
ArithmeticError BufferError StopIteration
(basestring|bool|buffer|bytearray|bytes|classmethod|complex|dict|enumerate|file|frozenset|list|memoryview|object|open|property|reversed|set|slice|staticmethod|str|super|tuple|type)
(bint|(long)double|enum|float|struct|union|void|const|fused|(signed unsigned char|((int short long longlong
(and|api|as|assert|break|by|class|continue|(|del|elif|else|except|finally|for|from|global|if|import|in|is|lambda|nonlocal|not|or|pass|public|raise|return|try|while|with|yield)
self cls
(abs|add|and|await|call|ceil|contains|copy|dealloc|deepcopy|del|delattr|delete|delitem|dir|div|divmod|aenter enter|eq|aexit exit|floor|floordiv|format|ge|get|getattr|getattribute|getinitargs|getitem|getnewargs|getstate|gt|hash|hex|iadd|iand|idiv|ifloordiv|ilshift|imul|index|cinit init|instancecheck|invert|ior|ipow|irshift|isub|aiter iter|itruediv|ixor|le|len|lshift|lt|metaclass|missing|mod|mul|ne|neg|new|anext next|oct|or|pos|pow|prepare|radd|rand|rdiv|rdivmod|reduce|reduce_ex|repr|reversed|rfloordiv|rlshift|rmod|rmul|ror|round|rpow|rrshift|rshift|rsub|rtruediv|rxor|setattr|setitem|setstate|signatures|sub|subclasscheck|subclasshook|truediv|trunc|weakref|xor)
(__import__|abort|calloc|malloc|realloc|labs llabs abs|all|any|ascii|bin|bsearch|callable|chr|compile|delattr|dir|ldiv lldiv div|divmod|eval|exec|_exit atexit exit|filter|format|free|getattr|getenv|globals|hasattr|hash|help|hex|id|input|isinstance|issubclass|iter|len|locals|max|min|next|oct|ord|pow|print|qsort|range|srand rand|repr|round|setattr|sizeof|sorted|sum|system|vars|zip)
