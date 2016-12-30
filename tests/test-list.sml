(*
 * Test cases for List structure (as an example of UnitTest)
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
  open UnitTest

  val called = ref nil
  fun check x = called := !called @ [x]
  val called2 = ref nil
  fun check2 x = called2 := !called2 @ [x]

  fun cmp (x, y) = if x > y then GREATER else if x < y then LESS else EQUAL
in

val test_List = testgroup "List"
[
  setup
    (fn _ => (called := nil; called2 := nil)),

  testcase "null: nil"
    (fn _ => List.null nil = true),

  testcase "null: cons"
    (fn _ => List.null [1] = false),

  testcase "length: nil"
    (fn _ => List.length nil = 0),

  testcase "length: cons"
    (fn _ => List.length [1,2,3] = 3),

  testcase "@: nil,nil"
    (fn _ => List.@ (nil, nil) = nil),

  testcase "@: nil,cons"
    (fn _ => List.@ (nil, [1,2,3]) = [1,2,3]),

  testcase "@: cons,nil"
    (fn _ => List.@ ([1,2,3], nil) = [1,2,3]),

  testcase "@: cons,cons"
    (fn _ => List.@ ([1,2,3], [4,5,6]) = [1,2,3,4,5,6]),

  testcase "hd: nil"
    (fn _ => (List.hd nil; false) handle Empty => true),

  testcase "hd: cons"
    (fn _ => List.hd [1,2,3] = 1),

  testcase "tl: nil"
    (fn _ => (List.tl nil; false) handle Empty => true),

  testcase "tl: cons"
    (fn _ => List.tl [1,2,3] = [2,3]),

  testcase "last: nil"
    (fn _ => (List.last nil; false) handle Empty => true),

  testcase "last: cons"
    (fn _ => List.last [1,2,3] = 3),

  testcase "getItem: nil"
    (fn _ => List.getItem nil = NONE),

  testcase "getItem: cons"
    (fn _ => List.getItem [1,2,3] = SOME (1, [2,3])),

  testcase "nth: nil negative"
    (fn _ => (List.nth (nil, ~1); false) handle Subscript => true),

  testcase "nth: nil zero"
    (fn _ => (List.nth (nil, 0); false) handle Subscript => true),

  testcase "nth: nil overflow"
    (fn _ => (List.nth (nil, 1); false) handle Subscript => true),

  testcase "nth: cons negative"
    (fn _ => (List.nth ([1,2,3], ~1); false) handle Subscript => true),

  testcase "nth: cons zero"
    (fn _ => List.nth ([1,2,3], 0) = 1),

  testcase "nth: cons last"
    (fn _ => List.nth ([1,2,3], 2) = 3),

  testcase "nth: cons overflow"
    (fn _ => (List.nth ([1,2,3], 3); false) handle Subscript => true),

  testcase "take: nil negative"
    (fn _ => (List.take (nil, ~1); false) handle Subscript => true),

  testcase "take: nil zero"
    (fn _ => List.take (nil, 0) = nil),

  testcase "take: nil overflow"
    (fn _ => (List.take (nil, 1); false) handle Subscript => true),

  testcase "take: cons negative"
    (fn _ => (List.take ([1,2,3], ~1); false) handle Subscript => true),

  testcase "take: cons zero"
    (fn _ => List.take ([1,2,3], 0) = nil),

  testcase "take: cons mid"
    (fn _ => List.take ([1,2,3], 2) = [1,2]),

  testcase "take: cons last"
    (fn _ => List.take ([1,2,3], 3) = [1,2,3]),

  testcase "take: cons overflow"
    (fn _ => (List.take ([1,2,3], 4); false) handle Subscript => true),

  testcase "drop: nil negative"
    (fn _ => (List.drop (nil, ~1); false) handle Subscript => true),

  testcase "drop: nil zero"
    (fn _ => List.drop (nil, 0) = nil),

  testcase "drop: nil overflow"
    (fn _ => (List.drop (nil, 1); false) handle Subscript => true),

  testcase "drop: cons negative"
    (fn _ => (List.drop ([1,2,3], ~1); false) handle Subscript => true),

  testcase "drop: cons zero"
    (fn _ => List.drop ([1,2,3], 0) = [1,2,3]),

  testcase "drop: cons mid"
    (fn _ => List.drop ([1,2,3], 2) = [3]),

  testcase "drop: cons last"
    (fn _ => List.drop ([1,2,3], 3) = nil),

  testcase "drop: cons overflow"
    (fn _ => (List.drop ([1,2,3], 4); false) handle Subscript => true),

  testcase "rev: nil"
    (fn _ => List.rev nil = nil),

  testcase "rev: cons"
    (fn _ => List.rev [1,2,3] = [3,2,1]),

  testcase "concat: nil"
    (fn _ => List.concat nil = nil),

  testcase "concat: cons one"
    (fn _ => List.concat [[1,2,3]] = [1,2,3]),

  testcase "concat: cons many"
    (fn _ => List.concat [[1,2,3],[4],nil,[5,6]] = [1,2,3,4,5,6]),

  testcase "revAppend: nil nil"
    (fn _ => List.revAppend (nil, nil) = nil),

  testcase "revAppend: nil cons"
    (fn _ => List.revAppend (nil, [1,2,3]) = [1,2,3]),

  testcase "revAppend: cons nil"
    (fn _ => List.revAppend ([1,2,3], nil) = [3,2,1]),

  testcase "revAppend: cons cons"
    (fn _ => List.revAppend ([1,2,3], [4,5,6]) = [3,2,1,4,5,6]),

  testcase "app: nil"
    (fn _ => List.app check nil = () andalso !called = nil),

  testcase "app: cons"
    (fn _ => List.app check [1,2,3] = () andalso !called = [1,2,3]),

  testcase "map: nil"
    (fn _ => List.map (fn x => (check x; [x])) nil = nil
             andalso !called = nil),

  testcase "map: cons"
    (fn _ => List.map (fn x => (check x; [x])) [1,2,3] = [[1],[2],[3]]
             andalso !called = [1,2,3]),

  testcase "mapParitial: nil"
    (fn _ => List.mapPartial
               (fn x => (check x; if x mod 2 = 0 then NONE else SOME [x])) nil
             = nil
             andalso !called = nil),

  testcase "mapParitial: cons"
    (fn _ => List.mapPartial
               (fn x => (check x; if x mod 2 = 0 then NONE else SOME [x]))
               [1,2,3,4,5]
             = [[1],[3],[5]]
             andalso !called = [1,2,3,4,5]),

  testcase "find: nil"
    (fn _ => List.find (fn x => (check x; true)) nil = NONE
             andalso !called = nil),

  testcase "find: cons false"
    (fn _ => List.find (fn x => (check x; false)) [1,2,3] = NONE
             andalso !called = [1,2,3]),

  testcase "find: cons true"
    (fn _ => List.find (fn x => (check x; true)) [1,2,3] = SOME 1
             andalso !called = [1]),

  testcase "find: cons mid"
    (fn _ => List.find (fn x => (check x; x = 2)) [1,2,3] = SOME 2
             andalso !called = [1,2]),

  testcase "find: cons last"
    (fn _ => List.find (fn x => (check x; x = 3)) [1,2,3] = SOME 3
             andalso !called = [1,2,3]),

  testcase "filter: nil"
    (fn _ => List.filter (fn x => (check x; false)) nil = nil
             andalso !called = nil),

  testcase "filter: nil true"
    (fn _ => List.filter (fn x => (check x; true)) nil = nil
             andalso !called = nil),

  testcase "filter: cons false"
    (fn _ => List.filter (fn x => (check x; false)) [1,2,3,4,5] = nil
             andalso !called = [1,2,3,4,5]),

  testcase "filter: cons true"
    (fn _ => List.filter (fn x => (check x; true)) [1,2,3,4,5] = [1,2,3,4,5]
             andalso !called = [1,2,3,4,5]),

  testcase "filter: cons cond"
    (fn _ => List.filter (fn x => (check x; x mod 2 = 0)) [1,2,3,4,5] = [2,4]
             andalso !called = [1,2,3,4,5]),

  testcase "partition: nil"
    (fn _ => List.partition (fn x => (check x; true)) nil = (nil, nil)
             andalso !called = nil),

  testcase "partition: cons false"
    (fn _ => List.partition (fn x => (check x; false)) [1,2,3,4]
             = (nil, [1,2,3,4])
             andalso !called = [1,2,3,4]),

  testcase "partition: cons true"
    (fn _ => List.partition (fn x => (check x; true)) [1,2,3,4]
             = ([1,2,3,4], nil)
             andalso !called = [1,2,3,4]),

  testcase "partition: cons cond"
    (fn _ => List.partition (fn x => (check x; x mod 2 = 0)) [1,2,3,4]
             = ([2,4], [1,3])
             andalso !called = [1,2,3,4]),

  testcase "foldl: nil"
    (fn _ => List.foldl (fn (x,z) => (check2 (x,z); [x]::z)) [[0]] nil = [[0]]
             andalso !called2 = nil),

  testcase "foldl: cons"
    (fn _ => List.foldl (fn (x,z) => (check2 (x,z); [x]::z)) [[0]] [1,2,3]
             = [[3],[2],[1],[0]]
             andalso !called2 = [(1,[[0]]),(2,[[1],[0]]),(3,[[2],[1],[0]])]),

  testcase "foldr: nil"
    (fn _ => List.foldr (fn (x,z) => (check2 (x,z); [x]::z)) [[0]] nil = [[0]]
             andalso !called2 = nil),

  testcase "foldr: cons"
    (fn _ => List.foldr (fn (x,z) => (check2 (x,z); [x]::z)) [[0]] [1,2,3]
             = [[1],[2],[3],[0]]
             andalso !called2 = [(3,[[0]]),(2,[[3],[0]]),(1,[[2],[3],[0]])]),

  testcase "exists: nil"
    (fn _ => List.exists (fn x => (check x; true)) nil = false
             andalso !called = nil),

  testcase "exists: cons false"
    (fn _ => List.exists (fn x => (check x; false)) [1,2,3] = false
             andalso !called = [1,2,3]),

  testcase "exists: cons true"
    (fn _ => List.exists (fn x => (check x; true)) [1,2,3] = true
             andalso !called = [1]),

  testcase "exists: cons mid"
    (fn _ => List.exists (fn x => (check x; x = 2)) [1,2,3] = true
             andalso !called = [1,2]),

  testcase "exists: cons last"
    (fn _ => List.exists (fn x => (check x; x = 3)) [1,2,3] = true
             andalso !called = [1,2,3]),

  testcase "all: nil"
    (fn _ => List.all (fn x => (check x; false)) nil = true
             andalso !called = nil),

  testcase "all: cons false"
    (fn _ => List.all (fn x => (check x; false)) [1,2,3] = false
             andalso !called = [1]),

  testcase "all: cons true"
    (fn _ => List.all (fn x => (check x; true)) [1,2,3] = true
             andalso !called = [1,2,3]),

  testcase "all: cons mid"
    (fn _ => List.all (fn x => (check x; x <> 2)) [1,2,3] = false
             andalso !called = [1,2]),

  testcase "all: cons last"
    (fn _ => List.all (fn x => (check x; x <> 3)) [1,2,3] = false
             andalso !called = [1,2,3]),

  testcase "tabulate: negative"
    (fn _ => ((List.tabulate (~1, fn x => (check x; x)); false)
              handle Size => true)
             andalso !called = nil),

  testcase "tabulate: zero"
    (fn _ => List.tabulate (0, fn x => (check x; x)) = nil
             andalso !called = nil),

  testcase "tabulate: positive"
    (fn _ => List.tabulate (5, fn x => (check x; [x])) = [[0],[1],[2],[3],[4]]
             andalso !called = [0,1,2,3,4]),

  (* The evaluation order of compare function is not specified. *)
  testcase "collate: nil, nil"
    (fn _ => List.collate cmp (nil, nil) = EQUAL),

  testcase "collate: nil, cons"
    (fn _ => List.collate cmp (nil, [1,2,3]) = LESS),

  testcase "collate: cons, nil"
    (fn _ => List.collate cmp ([1,2,3], nil) = GREATER),

  testcase "collate: cons, cons equal"
    (fn _ => List.collate cmp ([1,2,3], [1,2,3]) = EQUAL),

  testcase "collate: cons, cons less"
    (fn _ => List.collate cmp ([1,2,3], [1,2,4]) = LESS),

  testcase "collate: cons, cons greater"
    (fn _ => List.collate cmp ([1,2,4], [1,2,3]) = GREATER),

  testcase "collate: cons, cons less short"
    (fn _ => List.collate cmp ([1,2], [1,2,3]) = LESS),

  testcase "collate: cons, cons greater short"
    (fn _ => List.collate cmp ([2], [1,2,3]) = GREATER),

  testcase "collate: cons, cons less long"
    (fn _ => List.collate cmp ([1,1,1,1,1], [1,2,3]) = LESS),

  testcase "collate: cons, cons greater long"
    (fn _ => List.collate cmp ([1,2,3,4], [1,2,3]) = GREATER)

]

end
