fun f():
  fun g(): nothing end
  g()
end

fun iter(n, thunk):
  if n <= 0 block: nothing
  else:
     thunk()
     iter(n - 1, thunk)
  end
end

iter(10000, f)  
# test the cost of inner function allocation
