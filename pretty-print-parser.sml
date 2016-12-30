(*
 * Pretty simple pretty printer.
 * A part of Standard ML Short Pieces.
 * Copyright (C) 2016 UENO Katsuhiro.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)
(*
 * PrettyPrintParser provides a parser of serialized documents (doc strings)
 * as a way of concise construction of PrettyPrintEngine.doc values.
 * For example, you can write
 *   PrettyPrintParser.parse "{This+is} a pen."
 * instead of
 *   SEQ [ALIGN (GROUP (SEQ [TEXT "This", BREAK " ", TEXT "is"])),
 *        WEAKBREAK " ", TEXT "a", WEAKBREAK " ", TEXT "pen."]
 * with this library.  See the test suite for more examples.
 *
 * Intuitively, a doc string is a string, some characters of which have
 * special meaning as follows:
 * - A sequence of white spaces means a WEAKBREAK.
 * - "+" menas BREAK " ".
 * - "\n" means NEWLINE.
 * - "\\c" is escape sequence of c.
 * - A substring enclosed with "{" and "}" is a block.  A block may be
 *   followed by multiple marks that indicate kinds of the block.
 *   A mark is either "@" (GROUP), "@!" (ALIGN), "@," (SEQ), "@2" (INDENT 2),
 *   "@L2" (ASSOC ((L,2),"(",")",...)), or "@!L2" (ASSOC ((L,2),"","",...)).
 *   If a block has either no mark or only ASSOC marks, the block has ALIGN
 *   and GROUP marks implicitly.
 *
 * The syntax of a doc string (Doc) is the following:
 *   Doc ::= Exp*
 *   Exp ::= '+'                                    BREAK " "
 *         | (' '|'\t')+                            WEAKBREAK
 *         | '\n'                                   NEWLINE
 *         | '\' '\n'                               (ignored)
 *         | '\' Char                               escape
 *         | Char+                                  STRING
 *         | Blk
 *   Blk ::= '@' ['!'] ('L'|'R'|'N') [<int>] Blk    ASSOC
 *         | '@' Int Blk                            INDENT
 *         | '@' '!' Blk                            ALIGN
 *         | '@' Blk                                GROUP
 *         | '@' ',' Blk                            SEQ
 *         | '{' Exp '}'                            block
 *
 * In addition to the parser, PrettyPrintParser provides a DSL that enables
 * users to write a doc string in which ML values are embedded.
 * The set of functions defined in PrettyPrintParser.DSL organizes this DSL.
 * "b" and "e" indicates the beginning and end of a DSL expression.
 * A DSL expression represents a doc string in which ML expressions are
 * embedded.  There are two kinds of embedding, "$" and "`", which embed
 * PrettyPrinterEngine.doc and string values, respectively.
 * The following example is a pretty printer of a recursive datatype.
 *
 *   datatype exp = INT of n | ADD of exp * exp | MUL of exp * exp
 *   local
 *     open PrettyPrintParser.DSL
 *   in
 *     fun ppexp (INT n) = b `(Int.toString n) e
 *       | ppexp (ADD (e1, e2) = b "@L1{" $(ppexp e1) "+\+ " $(ppexp e2) "}" e
 *       | ppexp (MUL (e1, e2) = b "@L2{" $(ppexp e1) "+* " $(ppexp e2) "}" e
 *   end
 *)

structure PrettyPrintParser :> sig

  (* parse the given string.
   * Parse error is recovered by regarding special characters as string. *)
  val parse : string -> PrettyPrintEngine.doc

  (* embedded DSL *)
  structure DSL :
  sig
    type 'a t
    val b : string -> 'a t -> 'a
    val e : PrettyPrintEngine.doc t
    val $ : (PrettyPrintEngine.doc -> string -> 'a t -> 'a) t
    val ` : (string -> string -> 'a t -> 'a) t
  end

end =
struct

  datatype dir = datatype PrettyPrintEngine.dir
  datatype doc = datatype PrettyPrintEngine.doc

  fun dif (b, e) = #2 (Substring.base e) - #2 (Substring.base b)
  fun span (b, e) = Substring.string (#1 (Substring.splitAt (b, dif (b, e))))

  fun string r = STRING (span r)
  fun weakbreak r = WEAKBREAK (span r)
  fun seq [x] = x | seq l = SEQ l

  datatype token =
      DOC of doc
    | BEG of (doc -> doc) list
    | END
    | ESC of substring
    | SPC
    | CHR
    | IGN
    | EOF

  fun assoc (d,l,r) s =
      case Int.scan StringCvt.DEC Substring.getc s of
        SOME (n, s) => ((true, fn e => ASSOC ((d,n),l,r,e)), s)
      | NONE => ((true, fn e => ASSOC ((d,0),l,r,e)), s)

  fun mark s =
      case Substring.getc s of
        SOME (#"L", s) => assoc (L,"(",")") s
      | SOME (#"R", s) => assoc (R,"(",")") s
      | SOME (#"N", s) => assoc (N,"(",")") s
      | SOME (#",", s) => ((false, fn x => x), s)
      | SOME (#"!", s) =>
        (case Substring.getc s of
           SOME (#"L", s) => assoc (L,"","") s
         | SOME (#"R", s) => assoc (R,"","") s
         | SOME (#"N", s) => assoc (N,"","") s
         | _ => ((false, ALIGN), s))
      | _ =>
        case Int.scan StringCvt.DEC Substring.getc s of
          SOME (n, s) => ((false, fn e => INDENT (n,e)), s)
        | NONE => ((false, GROUP), s)

  fun beg s r =
      case Substring.getc s of
        SOME (#"@", s) => (case mark s of (m, s) => beg s (m::r))
      | SOME (#"{", s) =>
        if List.all #1 r
        then (BEG (GROUP :: ALIGN :: map #2 r), s)
        else (BEG (map #2 r), s)
      | SOME (_, s) => (CHR, s)
      | NONE => (CHR, s)

  fun lex s =
      case Substring.getc s of
        SOME (#"+", s) => (DOC (BREAK " "), s)
      | SOME (#"\n", s) => (DOC NEWLINE, s)
      | SOME (#" ", s) => (SPC, s)
      | SOME (#"\t", s) => (SPC, s)
      | SOME (#"\\", s) =>
        (case Substring.getc s of
           SOME (#"\n", s) => (IGN, s)
         | SOME (_, s2) => (ESC s, s2)
         | NONE => (CHR, s))
      | SOME (#"}", s) => (END, s)
      | SOME _ => beg s nil
      | NONE => (EOF, s)

  datatype state = STR of substring | BRK of substring | RUN
  datatype out = OUT of doc | GRP of (doc -> doc) list * (substring * substring)

  fun run (r, m as STR _, _, (CHR, s)) = run (r, m, s, lex s)
    | run (r, m as BRK _, _, (SPC, s)) = run (r, m, s, lex s)
    | run (r, STR b, e, t) = run (OUT (string (b, e)) :: r, RUN, e, t)
    | run (r, BRK b, e, t) = run (OUT (weakbreak (b, e)) :: r, RUN, e, t)
    | run (r, RUN, e, (CHR, s)) = run (r, STR e, s, lex s)
    | run (r, RUN, e, (SPC, s)) = run (r, BRK e, s, lex s)
    | run (r, RUN, _, (ESC e, s)) = run (r, STR e, s, lex s)
    | run (r, RUN, _, (IGN, s)) = run (r, RUN, s, lex s)
    | run (r, RUN, _, (DOC e, s)) = run (OUT e :: r, RUN, s, lex s)
    | run (r, RUN, e, (BEG m, s)) = run (GRP (m,(e,s)) :: r, RUN, s, lex s)
    | run (r, RUN, _, (END, s)) = run (reduce r nil, RUN, s, lex s)
    | run (r, RUN, _, (EOF, s)) = r

  and reduce nil b = OUT (STRING "}") :: rev (map OUT b)
    | reduce (OUT h :: t) b = reduce t (h :: b)
    | reduce (GRP (m,_) :: t) b = OUT (foldl (fn (f,z) => f z) (seq b) m) :: t

  fun final nil b = seq b
    | final (OUT h :: t) b = final t (h :: b)
    | final (GRP (_,p) :: t) b = final t (STRING (span p) :: b)

  fun close r = final r nil

  fun input (r, s) =
      case Substring.full s of s => run (r, RUN, s, lex s)

  fun parse s = close (input (nil, s))

  structure DSL =
  struct
    type 'a t = out list -> 'a
    fun b s (k:'a t) = k (input (nil, s))
    val e = close
    fun $ r exp s (k:'a t) = k (input (OUT exp :: r, s))
    fun ` r exp s (k:'a t) = k (input (OUT (STRING exp) :: r, s))
  end

end
