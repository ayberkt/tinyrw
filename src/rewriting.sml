structure Rewriting = struct
  open Matching
  infix 3 $

  exception NORM

  fun rewrite [] t = raise NORM
    | rewrite ((l, r)::R) t =
        (lift (match (l, t)) r)
         handle NoUnifier => rewrite R t

  fun norm R (V x) = V x
    | norm R (f $ ts) =
        let val u = f $ (map (norm R) ts)
        in norm R (rewrite R u) handle NORM => u end

  fun showRule (l, r) =
    toString l ^ "  --->  " ^ toString r ^ "\n"

end
