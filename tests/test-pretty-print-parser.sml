(*
 * Test suite for PrettyPrintParser.
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

local
  datatype doc = datatype PrettyPrintEngine.doc
  datatype dir = datatype PrettyPrintEngine.dir
  open UnitTest
  infix ==
  val tostr = PrettyPrintEngine.docToString
  fun op == (x,y) = (print ("L:\n" ^ tostr x ^ "\nR:\n" ^ tostr y); x = y)
in
val test_PrettyPrintParser = testgroup "PrettyPrintParser"
[
  testcase "parse1"
    (fn _ =>
        PrettyPrintParser.parse
          "a{b}c"
        == SEQ [STRING "a", ALIGN (GROUP (STRING "b")), STRING "c"]),

  testcase "parse2"
    (fn _ =>
        PrettyPrintParser.parse
          "a@{b{c}+d}e"
        == SEQ [STRING "a",
                GROUP (SEQ [STRING "b", ALIGN (GROUP (STRING "c")),
                            BREAK " ", STRING "d"]),
                STRING "e"]),

  testcase "parse: empty block"
    (fn _ =>
        PrettyPrintParser.parse
          "a{}b"
        == SEQ [STRING "a", ALIGN (GROUP (SEQ nil)), STRING "b"]),

  testcase "parse: empty"
    (fn _ =>
        PrettyPrintParser.parse
          ""
        == SEQ nil),

  testcase "parse: weakbreak"
    (fn _ =>
        PrettyPrintParser.parse
          " This   is a \t\t\n \t pen.  "
        == SEQ [WEAKBREAK " ", STRING "This", WEAKBREAK "   ", STRING "is",
                WEAKBREAK " ", STRING "a", WEAKBREAK " \t\t", NEWLINE,
                WEAKBREAK " \t ", STRING "pen.", WEAKBREAK "  "]),

  testcase "parse: escape"
    (fn _ =>
        PrettyPrintParser.parse
          "a\\ b\\@c\\\\de\\\nf\\"
        == SEQ [STRING "a", STRING " b", STRING "@c", STRING "\\de",
                STRING "f\\"]),

  testcase "parse: error"
    (fn _ =>
        PrettyPrintParser.parse
          "a@N0b@N!{c}d"
        == SEQ [STRING "a@N0b@N!", ALIGN (GROUP (STRING "c")), STRING "d"]),

  testcase "parse: error beg"
    (fn _ =>
        PrettyPrintParser.parse
          "a@N0{@{b}c"
        == SEQ [STRING "a", STRING "@N0{", GROUP (STRING "b"), STRING "c"]),

  testcase "parse: error end"
    (fn _ =>
        PrettyPrintParser.parse
          "a@{b}}c"
        == SEQ [STRING "a", GROUP (STRING "b"), STRING "}", STRING "c"]),

  testcase "parse: assoc"
    (fn _ =>
        PrettyPrintParser.parse
          "a@L{b}c@R1@{d}e@N~2@,{f}g@G@{h}i"
        == SEQ [STRING "a", ASSOC ((L,0),"(",")", ALIGN (GROUP (STRING "b"))),
                STRING "c", ASSOC ((R,1),"(",")", GROUP (STRING "d")),
                STRING "e", ASSOC ((N,~2),"(",")", STRING "f"),
                STRING "g@G", GROUP (STRING "h"), STRING "i"]),

  testcase "parse:block seq"
    (fn _ =>
        PrettyPrintParser.parse
          "a@,{b\\c}d"
        == SEQ [STRING "a", SEQ [STRING "b", STRING "c"], STRING "d"]),

  testcase "parse:block assoc"
    (fn _ =>
        PrettyPrintParser.parse
          "@N0@,{a\\b}"
        == ASSOC ((N, 0), "(", ")", SEQ [STRING "a", STRING "b"])),

  testcase "parse:block assoc indent"
    (fn _ =>
        PrettyPrintParser.parse
          "@N0@2{a\\b}"
        == ASSOC ((N,0), "(", ")", INDENT (2, SEQ [STRING "a", STRING "b"]))),

  testcase "parse:block assoc guard"
    (fn _ =>
        PrettyPrintParser.parse
          "@N0@!{a\\b}"
        == ASSOC ((N,0), "(", ")", ALIGN (SEQ [STRING "a", STRING "b"]))),

  testcase "parse:block assoc group"
    (fn _ =>
        PrettyPrintParser.parse
          "@N0@{a\\b}"
        == ASSOC ((N,0), "(", ")", GROUP (SEQ [STRING "a", STRING "b"]))),
    
  testcase "parse:block assoc indent group"
    (fn _ =>
        PrettyPrintParser.parse
          "@N0@2@{a\\b}"
        == ASSOC ((N,0), "(", ")",
                  INDENT (2, GROUP (SEQ [STRING "a", STRING "b"])))),

  testcase "parse:block assoc guard group"
    (fn _ =>
        PrettyPrintParser.parse
          "@N0{a\\b}"
        == ASSOC ((N,0), "(", ")",
                  ALIGN (GROUP (SEQ [STRING "a", STRING "b"])))),

  testcase "parse:block indent"
    (fn _ =>
        PrettyPrintParser.parse
          "@2{a\\b}"
        == INDENT (2, SEQ [STRING "a", STRING "b"])),

  testcase "parse:block guard"
    (fn _ =>
        PrettyPrintParser.parse
          "@!{a\\b}"
        == ALIGN (SEQ [STRING "a", STRING "b"])),

  testcase "parse:block group"
    (fn _ =>
        PrettyPrintParser.parse
          "@{a\\b}"
        == GROUP (SEQ [STRING "a", STRING "b"])),

  testcase "parse:block indent group"
    (fn _ =>
        PrettyPrintParser.parse
          "@2@{a\\b}"
        == INDENT (2, GROUP (SEQ [STRING "a", STRING "b"]))),

  testcase "parse:block guard group"
    (fn _ =>
        PrettyPrintParser.parse
          "{a\\b}"
        == ALIGN (GROUP (SEQ [STRING "a", STRING "b"]))),

  testcase "dsl"
    (fn _ =>
        (let open PrettyPrintParser.DSL
         in b "a{b" `("+") "c" $(BREAK "d+e") "}f" e end)
        == SEQ [STRING "a",
                ALIGN (GROUP (SEQ [STRING "b", STRING "+", STRING "c",
                                   BREAK "d+e"])),
                STRING "f"]),

  endgroup
]
end
