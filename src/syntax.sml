structure Syntax = struct
  open Unify
  
  datatype command =
      Rule of term * term
    | Norm of term
end
