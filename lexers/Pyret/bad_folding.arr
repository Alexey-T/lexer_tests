fun moves-down(a-grid):
  moves-grid = rotate(a-grid)
  l = a-grid.length() - 1
  fun triple-rotate-move(m): rotate-move(rotate-move(rotate-move(m, l), l), l); 
  for lists.fold_n(n from 0, acc from [list:], row from moves-grid):
    acc + get-moves(n, row).map(triple-rotate-move)
  end
end

fun moves-right(a-grid):
  moves-grid = rotate(rotate(a-grid))
  l = a-grid.length() - 1
  fun double-rotate-move(m): rotate-move(rotate-move(m, l), l); 
  for lists.fold_n(n from 0, acc from [list:], row from moves-grid):
    acc + get-moves(n, row).map(double-rotate-move)
  end
end

fun moves-up(a-grid):
  moves-grid = rotate(rotate(rotate(a-grid)))
  l = a-grid.length() - 1
  for lists.fold_n(n from 0, acc from [list:], row from moves-grid):
    acc + get-moves(n, row).map(rotate-move(_, l))
  end
end

fun get-value(grid, row, col):
  grid.get(row).get(col)
end


fun color-of-num(n):
  if n == 0: IS.color(187, 173, 169, 1)
  else if n == 2: IS.color(238, 228, 218, 1)
  else if n == 4: IS.color(237, 224, 203, 1)
  else if n == 8: IS.color(242, 177, 121, 1)
  else if n == 16: IS.color(245, 149, 99, 1)
  else if n == 32: IS.color(246, 124, 95, 1)
  else if n == 64: IS.color(246, 94, 59, 1)
  else if n == 128: IS.color(237, 207, 114, 1)
  else if n == 256: IS.color(237, 204, 97, 1)
  else if n == 512: IS.color(249, 200, 80, 1)
  else if n == 1024: IS.color(249, 197, 63, 1)
  else if n == 2048: IS.color(249, 194, 46, 1)
  else: "yellow"
  end
end

fun draw-square(value):
  str = if value == 0: "" else: tostring(value);
  text-img = I.text(str, 24, "black") 
  I.place-image(
    text-img,
    SQUARE-MIDDLE,
    SQUARE-MIDDLE,
    I.square(SQUARE-WIDTH, "solid", color-of-num(value)))
end
