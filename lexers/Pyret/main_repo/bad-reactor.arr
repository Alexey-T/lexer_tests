
reactor:
  init: 10,
  on-tick: lam(w :: Number): w + 1 end,
  stop-when: lam(x :: String): x > "a" end
end
