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
 * PrettyPrintEngine is an implementation of Wadler's prettier printer [1]
 * with some additional primitives inspired by SMLFormat [2] and wl-pretty
 * [3], such as WEAKBREAK and ALIGN.
 *
 * The algorithm implemented in PrettyPrintEngine is similar to [4], but
 * it behaves more closely to [1] and therefore is more efficient than [4]
 * in the sense that it avoids scanning group body twice if the group fits
 * in the current line.  This allows us to implement efficient WEAKBREAK,
 * which may incur tons of possible layouts.
 *
 * [1] Philip Wadler, A Prettier Printer, The Fun of Programming,
 *     Chapter 11, pp. 223--244, 2003.
 * [2] SMLFormat, http://www.pllab.riec.tohoku.ac.jp/smlsharp/
 * [3] wl-pprint, https://hackage.haskell.org/package/wl-pprint
 * [4] Christian Lindig, Strictly Pretty, 2000, available from
 *     http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200
 *)

structure PrettyPrintEngine :> sig

  datatype dir = L | R | N
  type assoc = dir * int

  datatype doc =
      (* sequence (concatination) of docs *)
      SEQ of doc list
    | (* string containing no newline *)
      STRING of string
    | (* choice of a newline or string (typically a whitespace) *)
      BREAK of string
    | (* weak variant of BREAK; if all BREAKs become newlines but the output
       * still exceeds the width limit, a WEAKBREAK becomes a newline. *)
      WEAKBREAK of string
    | (* unconditional newline *)
      NEWLINE
    | (* a group of BREAKs; if a BREAK in a group becomes a newline, so do
       * all BREAKs in the same group. *)
      GROUP of doc
    | (* increase the indentation level by the given int *)
      INDENT of int * doc
    | (* set indentation level to the current column *)
      ALIGN of doc
    | (* set associativity; if assoc is weaker than the current associativiely,
       * doc is enclosed with the given parenthesizes. *)
      ASSOC of assoc * string * string * doc

  (* the width of the output.  0 means inifinity. *)
  type width = int

  (* render a doc within the width as a string. *)
  val render : width -> doc -> string

  (* output a doc by the given print function. *)
  val output : (string -> unit) -> width -> doc -> unit

  (* for debug print *)
  val docToString : doc -> string

end =
struct

  datatype dir = L | R | N
  type assoc = dir * int
  datatype doc =
      SEQ of doc list
    | STRING of string
    | BREAK of string
    | WEAKBREAK of string
    | NEWLINE
    | GROUP of doc
    | INDENT of int * doc
    | ALIGN of doc
    | ASSOC of assoc * string * string * doc
  type width = int

  (* utilities for associativity *)
  fun left (d, n) : assoc = (case d of L => L | R => N | N => N, n)
  fun right (d, n) : assoc = (case d of L => N | R => R | N => N, n)
  fun lower ((d1, n1):assoc, (d2, n2):assoc) =
      if d1 = d2 andalso d1 <> N then n1 < n2 else n1 <= n2

  (* output buffer is a reversed list of strings and whitespaces *)
  datatype output = STR of string | SPC of int | BRK

  fun spaces n = CharArray.vector (CharArray.array (n, #" "))
  fun string (STR s) = s
    | string (SPC n) = CharArray.vector (CharArray.array (n, #" "))
    | string BRK = "\n"
  fun makeLine (SPC _ :: r) = makeLine r
    | makeLine (BRK :: SPC _ :: r) = makeLine (BRK :: r)
    | makeLine r = String.concat (map string (rev r))

  (*
   * Pretty printer is an abstract machine that transforms a machine state
   * under the given width $W$.
   * A machine state $s$ is a 4-tuple $(p,k,m,t)$ consisting of
   *   $p$ : output buffer,
   *   $k$ : the length of the last line in the output,
   *   $m$ : mode and preserved state for backtracking, and
   *   $t$ : input stack.
   *
   * $m$ is one of the following:
   * - OUT
   *     BREAKs become newlines.
   * - TRY (WEAK, $s$)
   *     Same as OUT but a backtracking state is saved for a WEAKBREAK.
   * - TRY (GRP $t$, $s$)
   *     Trying to pack the current group into the current line within $W$.
   * - TRY (FIX, $s$)
   *     Trying to determine whether or not a group fits to the current line.
   *     If the machine reaches a BREAK within $W$, it goes to OUT mode.
   *
   * An input item in $t$ is the triple of a doc and its two contexts which
   * are the indentation level and associativity.
   *)
  type input = doc * int * assoc
  datatype mode = GRP of input list | FIX | WEAK
  datatype mode_stack = OUT | TRY of mode * state
  withtype state = output list * int * mode_stack * input list

  fun run w (p, k, TRY (GRP l, s), nil) = run w (p, k, TRY (FIX, s), l)
    | run w (p, k, m, nil) = makeLine p
    | run w (p, k, m, (SEQ nil, i, a)::t) = run w (p, k, m, t)
    | run w (p, k, m, (SEQ [x], i, a)::t) = run w (p, k, m, (x,i,a)::t)
    | run w (p, k, m, (SEQ (x::y), i, a)::t) =
      run w (p, k, m, (x, i, left a) :: (SEQ y, i, right a) :: t)
    | run w (p, k, m, (STRING x, i, a)::t) = put w x (p,k,m,t)
    | run w (p, k, m as TRY (GRP _, _), (BREAK x, i, a)::t) = put w x (p,k,m,t)
    | run w (p, k, m, (BREAK x, i, a)::t) = brk w (p,i,m,t)
    | run w (p, k, m, (NEWLINE, i, a)::t) = brk w (p,i,m,t)
    | run w (p, k, m, (INDENT(j,x), i, a)::t) = run w (p,k,m,(x,i+j,a)::t)
    | run w (p, k, m, (ALIGN x, i, a)::t) = run w (p,k,m,(x,k,a)::t)
    | run w (p, k, m as TRY (GRP _, _), (GROUP x, i, a)::t) =
      run w (p, k, m, (x,i,a) :: t)
    | run w (p, k, m, (GROUP x, i, a)::t) =
      run w (p, k, TRY (GRP t, (p, k, m, (x,i,a)::t)), [(x,i,a)])
    | run w (p, k, m as TRY(GRP _, _), (WEAKBREAK x,i,a)::t) = put w x (p,k,m,t)
    | run w (p, k, m as TRY(FIX, _), (WEAKBREAK x,i,a)::t) = put w x (p,k,m,t)
    | run w (p, k, m, (WEAKBREAK x, i, a)::t) =
      put w x (p, k, TRY (WEAK, (p, k, OUT, (BREAK x, i, a) :: t)), t)
    | run w (p, k, m, (ASSOC(a2,l,r,x), i, a1)::t) =
      if lower (a2, a1)
      then run w (p, k, m, (STRING l,i,a1)::(x,i,a2)::(STRING r,i,a1)::t)
      else run w (p, k, m, (x,i,a2)::t)

  and brk w (p, k, TRY (GRP _, s), t) =
      run w s
    | brk (w as (_,out)) (p, k, m, t) =
      run w (SPC k :: out (makeLine (BRK :: p)), k, OUT, t)

  and put w text (p, k, m as OUT, t) =
      run w (STR text :: p, k + size text, m, t)
    | put (w as (width, _)) text (p, k, m as TRY (r, s), t) =
      if width <= 0 orelse k + size text <= width
      then run w (STR text :: p, k + size text, m, t)
      else run w s

  fun init doc = (nil, 0, OUT, [(GROUP doc, 0, (N, 0))])

  fun render width doc =
      run (width, fn x => [STR x]) (init doc)

  fun output print width doc =
      print (run (width, fn x => (print x; nil)) (init doc))

  (* for debug *)
  fun dir L = "L" | dir R = "R" | dir N = "N"
  fun assoc (a, n) = dir a ^ Int.toString n
  fun blk (r, GROUP e) = blk ("@G"::r, e)
    | blk (r, ALIGN e) = blk ("@A"::r, e)
    | blk (r, INDENT (n,e)) = blk (("@" ^ Int.toString n)::r, e)
    | blk (r, ASSOC (a,x,y,e)) = blk (("@" ^ assoc a ^ x ^ "," ^ y)::r, e)
    | blk (r, SEQ l) = String.concat (rev r) ^ "@S{" ^ seq l ^ "}"
    | blk (r, e) = String.concat (rev r) ^ "{" ^ docToString e ^ "}"
  and seq l = String.concatWith "," (map docToString l)
  and docToString (STRING s) =
      String.translate
        (fn #"@" => "\\@" | #"{" => "\\{" | #"}" => "\\}" | #"+" => "\\+"
         | #" " => "\\ " | #"\t" => "\\\t" | #"," => "\\," | c => str c)
        s
    | docToString (BREAK " ") = "+"
    | docToString (BREAK s) = "@b{" ^ String.toString s ^ "}"
    | docToString (WEAKBREAK s) =
      if CharVector.all (fn c => c = #" " orelse c = #"\t") s then s
      else "@w{" ^ String.toString s ^ "}"
    | docToString NEWLINE = "\n"
    | docToString e = blk (nil, e)

end
