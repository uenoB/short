(*
 * Test suite for PrettyPrintEngine.
 * A part of Standard ML Short Suites.
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
  fun op == (x,y) = (print ("L:\n" ^ x ^ "\nR:\n" ^ y); x = y)
in
val test_PrettyPrintEngine = testgroup "PrettyPrintEngine"
[
  testcase "FIX"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (SEQ [GROUP (SEQ [STRING "12345", BREAK " ", STRING "6789"]),
                STRING "0"])
        == "12345\n67890"),

  testcase "WEAKBREAK6"
    (fn _ =>
        PrettyPrintEngine.render
          6
          (SEQ [STRING "123", WEAKBREAK " ",
                GROUP (SEQ [STRING "456", BREAK " ", STRING "789"]),
                STRING "abc", WEAKBREAK " ", STRING "def"])
        == "123\n456\n789abc\ndef"),

  testcase "WEAKBREAK7"
    (fn _ =>
        PrettyPrintEngine.render
          7
          (SEQ [STRING "123", WEAKBREAK " ",
                GROUP (SEQ [STRING "456", BREAK " ", STRING "789"]),
                STRING "abc", WEAKBREAK " ", STRING "def"])
        == "123 456\n789abc\ndef"),

  testcase "WEAKBREAK9"
    (fn _ =>
        PrettyPrintEngine.render
          9
          (SEQ [STRING "123", WEAKBREAK " ",
                GROUP (SEQ [STRING "456", BREAK " ", STRING "789"]),
                STRING "abc", WEAKBREAK " ", STRING "def"])
        == "123 456\n789abc\ndef"),

  testcase "WEAKBREAK10"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (SEQ [STRING "123", WEAKBREAK " ",
                GROUP (SEQ [STRING "456", BREAK " ", STRING "789"]),
                STRING "abc", WEAKBREAK " ", STRING "def"])
        == "123 456\n789abc def"),

  testcase "WEAKBREAK17"
    (fn _ =>
        PrettyPrintEngine.render
          17
          (SEQ [STRING "123", WEAKBREAK " ",
                GROUP (SEQ [STRING "456", BREAK " ", STRING "789"]),
                STRING "abc", WEAKBREAK " ", STRING "def"])
        == "123 456\n789abc def"),

  testcase "WEAKBREAK18"
    (fn _ =>
        PrettyPrintEngine.render
          18
          (SEQ [STRING "123", WEAKBREAK " ",
                GROUP (SEQ [STRING "456", BREAK " ", STRING "789"]),
                STRING "abc", WEAKBREAK " ", STRING "def"])
        == "123 456 789abc def"),

  testcase "GRP-in-FIX"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (SEQ [GROUP (SEQ [STRING "123", BREAK " ", STRING "456"]),
                GROUP (SEQ [STRING "789", BREAK " ", STRING "0ab"])])
        == "123 456789\n0ab"),

  testcase "GRP-in-FIX2"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (SEQ [GROUP (SEQ [STRING "123", BREAK " ", STRING "456"]),
                GROUP (SEQ [STRING "7890", BREAK " ", STRING "ab"])])
        == "123\n4567890 ab"),

  testcase "ALIGN"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (SEQ [STRING "1234",
                ALIGN (SEQ [STRING "567", BREAK " ", STRING "890"])])
        == "1234567\n    890"),

  testcase "ASSOC-L1"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((L,1),"(",")", ASSOC((L,1),"(",")",STRING "a")))
        == "a"),

  testcase "ASSOC-R1"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((R,1),"(",")", ASSOC((R,1),"(",")",STRING "a")))
        == "a"),

  testcase "ASSOC-N1"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((N,1),"(",")", ASSOC((N,1),"(",")",STRING "a")))
        == "(a)"),

  testcase "ASSOC-LR1"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((L,1),"(",")", ASSOC((R,1),"(",")",STRING "a")))
        == "(a)"),

  testcase "ASSOC-L3"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((L,1),"(",")", SEQ [ASSOC((L,1),"(",")",STRING "a"),
                                      ASSOC((L,1),"(",")",STRING "b"),
                                      ASSOC((L,1),"(",")",STRING "c")]))
        == "a(b)(c)"),

  testcase "ASSOC-R3"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((R,1),"(",")", SEQ [ASSOC((R,1),"(",")",STRING "a"),
                                      ASSOC((R,1),"(",")",STRING "b"),
                                      ASSOC((R,1),"(",")",STRING "c")]))
        == "(a)(b)c"),

  testcase "ASSOC-N3"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((N,1),"(",")", SEQ [ASSOC((N,1),"(",")",STRING "a"),
                                      ASSOC((N,1),"(",")",STRING "b"),
                                      ASSOC((N,1),"(",")",STRING "c")]))
        == "(a)(b)(c)"),

  testcase "ASSOC-L2L1"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((L,2),"(",")", SEQ [ASSOC((L,1),"(",")",STRING "a"),
                                      ASSOC((L,1),"(",")",STRING "b"),
                                      ASSOC((L,1),"(",")",STRING "c")]))
        == "(a)(b)(c)"),

  testcase "ASSOC-R2R1"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((R,2),"(",")", SEQ [ASSOC((R,1),"(",")",STRING "a"),
                                      ASSOC((R,1),"(",")",STRING "b"),
                                      ASSOC((R,1),"(",")",STRING "c")]))
        == "(a)(b)(c)"),

  testcase "ASSOC-N2N1"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((N,2),"(",")", SEQ [ASSOC((N,1),"(",")",STRING "a"),
                                      ASSOC((N,1),"(",")",STRING "b"),
                                      ASSOC((N,1),"(",")",STRING "c")]))
        == "(a)(b)(c)"),

  testcase "ASSOC-LR"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (ASSOC ((L,1),"(",")", SEQ [ASSOC((R,1),"(",")",STRING "a"),
                                      ASSOC((R,1),"(",")",STRING "b"),
                                      ASSOC((R,1),"(",")",STRING "c")]))
        == "(a)(b)(c)"),

  testcase "indent-newline"
    (fn _ =>
        PrettyPrintEngine.render
          10
          (INDENT (2, SEQ [NEWLINE, NEWLINE]))
        == "\n\n"),

  endgroup
]
end
