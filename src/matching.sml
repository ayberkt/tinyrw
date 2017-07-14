structure Matching = struct
  open Unify
  infix 3 $

  fun matchs ([], s) = s
    | matchs ((V x, t)::S, s) =
        if indom x s
        then
          if app s x = t
          then matchs (S, s)
          else raise NoUnifier
        else matchs (S, (x, t)::s)
    | matchs ((t, V x)::S, s) = raise NoUnifier
    | matchs ((f $ ts, g $ us)::S, s) =
        if f = g
        then matchs(zip (ts, us) @ S, s)
        else raise NoUnifier

  fun match (pat : term, obj : term) : subst =
    matchs ([(pat, obj)], [])

end
