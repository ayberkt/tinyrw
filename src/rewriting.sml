structure Rewriting = struct
  open Matching
  infix 3 $

  (*fun rewrite [] t = []
    | rewrite ((l, r)::R) t =
        (lift (match (l, t)) r)::rewrite R t
        handle NoUnifier => rewrite R t*)

  fun matches [] _ = []
    | matches ((lhs, rhs)::R) tm =
        let
          val msubst =
            (SOME (match (lhs, tm))
             handle NoUnifier => NONE)
        in
          case msubst of
            SOME s => (lhs, rhs)::(matches R tm)
          | NONE => matches R tm
        end

  fun prRule (l, r) =
    print ("  * " ^ toString l ^ "  --->  " ^ toString r ^ "\n")

  val prRules = List.app prRule

  fun report [] = print "No more rules match.\n"
    | report R  =
        (print ((Int.toString o List.length) R ^ " rules apply:\n");
         prRules R)

  val rules =
    [
      ( "foo" $ [V ("X", 1), V ("Y", 2), V ("Z", 3)],
        "foo" $ ["foo" $ [V ("X", 1), V ("Y", 2)], V ("Z", 3)])
    , ( "foo" $ [V ("X", 1), V ("Y", 2), V ("Z", 3)],
        "foo" $ [V ("X", 1), "foo" $ [V ("Y", 2), V ("Z", 3)]])
    ]

  fun run tm = report (matches rules tm)

  fun const tm = tm $ []

  val _ = run ("foo" $ [const "zero", const "zero", const "zero"])

end
