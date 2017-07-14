structure Utils = struct
  fun zip ([], _) = []
    | zip (_, []) = []
    | zip (x::xs, y::ys) = (x, y)::(zip (xs, ys))

  fun intersperse y [] = []
    | intersperse y [x] = [x]
    | intersperse y (x::xs) = x::y::(intersperse y xs)

  fun intercalate s ss =
    String.concat (intersperse s ss)
end
