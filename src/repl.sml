structure REPL = struct
  open Unify
  open Syntax
  structure RW = Rewriting

  structure RewriteLrVals =
    RewriteLrValsFun(structure Token = LrParser.Token)

  structure RewriteLex =
    RewriteLexFun(structure Tokens = RewriteLrVals.Tokens)

  structure RewriteParser =
    JoinWithArg
      (structure LrParser = LrParser
       structure ParserData = RewriteLrVals.ParserData
       structure Lex = RewriteLex)

  structure REP = struct
    fun stringreader s =
      let
        val pos = ref 0
        val remainder = ref (String.size s)
        fun min(a, b) = if a < b then a else b
      in
        fn n =>
          let
            val m = min(n, !remainder)
            val s = String.substring(s, !pos, m)
            val () = pos := !pos + m
            val () = remainder := !remainder - m
          in
            s
          end
      end

    exception ParseError of Pos.t * string

    val printLn = print o (fn s => s ^ "\n")

    fun error fileName (s, pos, pos') : unit =
      raise ParseError (Pos.pos (pos fileName) (pos' fileName), s)

    val rules : (term * term) list ref = ref []

    val addRule = fn rl => rules := (!rules)@[rl]

    fun loop () =
      let
        val input = (print "> "; TextIO.inputLine TextIO.stdIn)
      in
        case input of
          NONE => 0
        | SOME str =>
            ((let
                val lexer = RewriteParser.makeLexer (stringreader (Option.valOf input)) "-"
                val (result, _) = RewriteParser.parse (1, lexer, error "-", "-")
              in
                case result of
                  Rule (lhs, rhs) =>
                    (addRule (lhs, rhs);
                     printLn ("Added rule: " ^ RW.showRule (lhs, rhs)))
                | Norm tm => (printLn o toString) (RW.norm (!rules) tm)
              end
              handle err =>
                print ("Error: " ^ exnMessage err ^ "\n\n"));
                loop ())
      end

    fun main (name, args) = loop ()

    val _ = SMLofNJ.exportFn ("repl", main)
  end

end
