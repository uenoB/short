(*
 * test suite for WordPatriciaMap.
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

  fun sw k = "0wx" ^ Word.fmt StringCvt.HEX k
  fun si v = "0x" ^ Int.fmt StringCvt.HEX v
  fun so f NONE = "NONE" | so f (SOME x) = f x
  fun s2 (x,y) = "(" ^ x ^ "," ^ y ^ ")"
  fun s3 (x,y,z) = "(" ^ x ^ "," ^ y ^ "," ^ z ^ ")"
  fun sl f l = "[" ^ String.concatWith "," (map f l) ^ "]"
  fun skv (k,v) = s2 (sw k, si v)
  fun skvv (k,x,y) = s3 (sw k, si x, si y)
  fun svv (x,y) = s2 (si x, si y)
  fun skvvo (k,x,y) = s3 (sw k, so si x, so si y)
  fun svvo (x,y) = s2 (so si x, so si y)

  (* sample data which covers all branches in merge *)
  val src = [
                  (* 0|______  0|______  B,B  m1=m2, (p1,m1)=(p2,m1)        *)
                  (* 00|_____  00|_____                                     *)
                  (* 000|____  000|____                                     *)
                  (* 00000|__  00000|__                                     *)
                  (* 0000000|  0000001|  B,B  m1=m2, (p1,m1)<(p2,m1)        *)
    (0x00, 0x02), (* 00000000  00000010                                     *)
    (0x01, 0x03), (* 00000001  00000011                                     *)
                  (* 0000011|  0000010|  B,B  m1=m2, (p1,m1)>(p2,m1)        *)
    (0x06, 0x04), (* 00000110  00000100                                     *)
    (0x07, 0x05), (* 00000111  00000101                                     *)
                  (* 0001|___  0001|___                                     *)
                  (* 000101|_  0001000|  B,B  m1<m2  (p1,m1)>(p2,m1)        *)
    (0x14, 0x10), (* 00010100  00010000                                     *)
    (0x16, 0x11), (* 00010110  00010001                                     *)
                  (* 000110|_  0001110|  B,B  m1<m2  (p1,m1)<(p2,m1)        *)
    (0x18, 0x1c), (* 00011000  00011100                                     *)
    (0x1a, 0x1d), (* 00011010  00011101                                     *)
                  (* 001|____  001|____                                     *)
                  (* 0010|___  0010|___                                     *)
                  (* 00100|__  001000|_  B,B  m1<m2, (p1,m1)=(p2,m1), p1>p2 *)
    (0x23, ~1  ), (* 00100011            L,B  (k1,m2)=(p2,m2), k1>p2        *)
    (0x24, ~1  ), (* 00100100                                               *)
    (~1  , 0x20), (*           00100000                                     *)
    (~1  , 0x22), (*           00100010  L,L  k1>k2                         *)
                  (* 00101|__  001011|_  B,B  m1<m2, (p1,m1)=(p2,m1), p1<p2 *)
    (0x28, ~1  ), (* 00101000                                               *)
    (0x2c, ~1  ), (* 00101100            L,B  (k1,m2)=(p2,m2), k1<p2        *)
    (~1  , 0x2d), (*           00101101  L,L  k1<k2                         *)
    (~1  , 0x2f), (*           00101111                                     *)
                  (* 0011|___  0011|___                                     *)
    (0x31, ~1  ), (* 00110001  0011000|  L,B  (k1,m2)=(p2,m2), k1=p2        *)
    (~1  , 0x30), (*           00110000                                     *)
    (~1  , 0x31), (*           00110001  L,L  k1=k2                         *)
                  (* 00111|__  00111|__                                     *)
    (0x3a, ~1  ), (* 00111010  0011100|  L,B  (k1,m2)>(p2,m2)               *)
    (~1  , 0x38), (*           00111000                                     *)
    (~1  , 0x39), (*           00111001                                     *)
    (0x3c, ~1  ), (* 00111100  0011111|  L,B  (k1,m2)<(p2,m2)               *)
    (~1  , 0x3e), (*           00111110                                     *)
    (~1  , 0x3f), (*           00111111                                     *)
                  (* 01|_____  01|_____                                     *)
                  (* 0101|___  0101|___                                     *)
                  (* 0101000|  010101|_  B,B  m1>m2  (p1,m2)<(p2,m2)        *)
    (0x50, 0x54), (* 01010000  01010100                                     *)
    (0x51, 0x56), (* 01010001  01010110                                     *)
                  (* 0101110|  010110|_  B,B  m1>m2  (p1,m2)>(p2,m2)        *)
    (0x5c, 0x58), (* 01011100  01011000                                     *)
    (0x5d, 0x5a), (* 01011101  01011010                                     *)
                  (* 011|____  011|____                                     *)
                  (* 0110|___  0110|___  B,B  m1>m2, (p1,m2)=(p2,m2), p1<p2 *)
                  (* 011000|_  01100|__  B,L  (p1,m1)=(k2,m1), p1<k2        *)
    (~1  , 0x63), (*           01100011                                     *)
    (~1  , 0x64), (*           01100100                                     *)
    (0x60, ~1  ), (* 01100000            L,L  k1<k2                         *)
    (0x62, ~1  ), (* 01100010            B,B  m1>m2, (p1,m2)=(p2,m2), p1>p2 *)
    (~1  , ~1  ), (* 011011|_  01101|__                                     *)
    (~1  , 0x68), (*           01101000  B,L  (p1,m1)=(k2,m1), p1>k2        *)
    (~1  , 0x6c), (*           01101100  L,L  k1>k2                         *)
    (0x6d, ~1  ), (* 01101101                                               *)
    (0x6f, ~1  ), (* 01101111                                               *)
                  (* 0111|___  0111|___                                     *)
    (~1  , 0x71), (* 0111000|  01110001  B,L  (p1,m1)=(k2,m1), p1=k2        *)
    (0x70, ~1  ), (* 01110000                                               *)
    (0x71, ~1  ), (* 01110001            L,L  k1=k2                         *)
                  (* 01111|__  01111|__                                     *)
    (~1  , 0x7a), (* 0111100|  01111010  B,L  (p1,m1)<(k2,m1)               *)
    (0x78, ~1  ), (* 01111000                                               *)
    (0x79, ~1  ), (* 01111001                                               *)
    (~1  , 0x7c), (* 0111111|  01111100  B,L  (p1,m1)>(k2,m1)               *)
    (0x7e, ~1  ), (* 01111110                                               *)
    (0x7f, ~1  )  (* 01111111                                               *)
  ]

  val (keys1, keys2) = ListPair.unzip src
  val keys1 = List.filter (fn x => x >= 0) keys1
  val keys2 = List.filter (fn x => x >= 0) keys2
  val src1 = List.map (fn x => (Word.fromInt x, x * 16 + 1)) keys1
  val src2 = List.map (fn x => (Word.fromInt x, x * 16 + 2)) keys2
  fun fromList l = List.foldl WordPatriciaMap.insert' WordPatriciaMap.empty l
  val m1 = fromList src1
  val m2 = fromList src2
  val empty = WordPatriciaMap.empty

  (* isomorphic operations on lists *)
  fun merge f (nil, nil) = nil
    | merge f (nil, (k2,v2)::t2) = f (k2, NONE, SOME v2) @ merge f (nil, t2)
    | merge f ((k1,v1)::t1, nil) = f (k1, SOME v1, NONE) @ merge f (t1, nil)
    | merge f (l1 as (k1:word,v1)::t1, l2 as (k2,v2)::t2) =
      if k1 = k2 then f (k1, SOME v1, SOME v2) @ merge f (t1, t2)
      else if k1 < k2 then f (k1, SOME v1, NONE) @ merge f (t1, l2)
      else f (k2, NONE, SOME v2) @ merge f (l1, t2)

  fun union f =
      merge (fn (k, SOME x, SOME y) => [(k, f (k,x,y))]
              | (k, SOME x, NONE) => [(k,x)]
              | (k, NONE, SOME y) => [(k,y)]
              | _ => [])

  fun intersect f =
      merge (fn (k, SOME x, SOME y) => [(k, f (k,x,y))] | _ => [])

  fun sort (f:'a -> word) nil = nil
    | sort f (h::t) = sort f (List.filter (fn x => f x < f h) t) @ [h]
                      @ sort f (List.filter (fn x => f x >= f h) t)

  (* callback check *)
  val checkList = ref nil : (int * int) list ref
  fun check f x = (checkList := !checkList @ [x]; f x)
  val check2List = ref nil : (word * int) list ref
  fun check2 f x = (check2List := !check2List @ [x]; f x)
  val checkoList = ref nil : (int option * int option) list ref
  fun checko f x = (checkoList := !checkoList @ [x]; f x)
  val checkiList = ref nil : (word * int * int) list ref
  fun checki f x = (checkiList := !checkiList @ [x]; f x)
  val checkioList = ref nil : (word * int option * int option) list ref
  fun checkio f x = (checkioList := !checkioList @ [x]; f x)
in

val test_WordPatriciaMap = testgroup "WordPatriciaMap"
[

(*
TODO:
  val insert : 'a map * key * 'a -> 'a map
  val insert' : (key * 'a) * 'a map -> 'a map
  val find : 'a map * key -> 'a option
  val lookup : 'a map * key -> 'a
  val inDomain : 'a map * key -> bool
  val remove : 'a map * key -> 'a map * 'a
  val first : 'a map -> 'a option
  val firsti : 'a map -> (key * 'a) option
  val app : ('a -> unit) -> 'a map -> unit
  val map : ('a -> 'b) -> 'a map -> 'b map
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val filter : ('a -> bool) -> 'a map -> 'a map
  val mapPartial : ('a -> 'b option) -> 'a map -> 'b map
  val mapPartiali : (key * 'a -> 'b option) -> 'a map -> 'b map
  val exists : ('a -> bool) -> 'a map -> bool
  val existsi : (key * 'a -> bool) -> 'a map -> bool
  val all : ('a -> bool) -> 'a map -> bool
  val alli : (key * 'a -> bool) -> 'a map -> bool
*)

  setup (fn _ => checkList := nil),
  setup (fn _ => check2List := nil),
  setup (fn _ => checkiList := nil),
  setup (fn _ => checkoList := nil),
  setup (fn _ => checkioList := nil),

  testcase "listKeys:E"
    (fn _ => WordPatriciaMap.listKeys empty = []),

  testcase "listKeys:1"
    (fn _ => WordPatriciaMap.listKeys m1 = List.map #1 src1),

  testcase "listKeys:2"
    (fn _ => WordPatriciaMap.listKeys m2 = List.map #1 src2),

  testcase "listItems:E"
    (fn _ => WordPatriciaMap.listItems empty = []),

  testcase "listItems:1"
    (fn _ => WordPatriciaMap.listItems m1 = List.map #2 src1),

  testcase "listItems:2"
    (fn _ => WordPatriciaMap.listItems m2 = List.map #2 src2),

  testcase "listItemsi:E"
    (fn _ => WordPatriciaMap.listItemsi empty = []),

  testcase "listItemsi:1"
    (fn _ => WordPatriciaMap.listItemsi m1 = src1),

  testcase "listItemsi:2"
    (fn _ => WordPatriciaMap.listItemsi m2 = src2),

  testcase "numItems:E"
    (fn _ => WordPatriciaMap.numItems empty = 0),

  testcase "numItems:1"
    (fn _ => WordPatriciaMap.numItems m1 = length src1),

  testcase "numItems:2"
    (fn _ => WordPatriciaMap.numItems m2 = length src2),

  testcase "isEmpty:E"
    (fn _ => WordPatriciaMap.isEmpty empty = true),

  testcase "isEmpty:1"
    (fn _ => WordPatriciaMap.isEmpty m1 = false),

  testcase "singleton:numItems"
    (fn _ =>
        WordPatriciaMap.numItems
          (WordPatriciaMap.singleton (0w0,0))
        = 1),

  testcase "singleton:listItemsi"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.singleton (0w0,0))
        = [(0w0, 0)]),

  testcase "appi:E"
    (fn _ =>
        WordPatriciaMap.appi (check2 ignore) empty = ()
        andalso !check2List = []),

  testcase "appi:1"
    (fn _ =>
        WordPatriciaMap.appi (check2 ignore) m1 = ()
        andalso !check2List = src1),

  testcase "mapi:E"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mapi (check2 ignore) empty)
        = []
        andalso !check2List = []),

  testcase "mapi:1"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mapi (check2 (fn (k,x) => x * 13)) m1)
        = map (fn (k,x) => (k, x * 13)) src1
        andalso !check2List = src1),

  testcase "foldli:E"
    (fn _ =>
        WordPatriciaMap.foldli
          (fn (k,v,z) => (check2 ignore (k,v); (k,v)::z))
          nil
          empty
        = []
        andalso !check2List = []),

  testcase "foldli:1"
    (fn _ =>
        WordPatriciaMap.foldli
          (fn (k,v,z) => (check2 ignore (k,v); (k,v)::z))
          nil
          m1
        = rev src1
        andalso !check2List = src1),

  testcase "foldri:E"
    (fn _ =>
        WordPatriciaMap.foldri
          (fn (k,v,z) => (check2 ignore (k,v); (k,v)::z))
          nil
          empty
        = []
        andalso !check2List = []),

  testcase "foldri:1"
    (fn _ =>
        WordPatriciaMap.foldri
          (fn (k,v,z) => (check2 ignore (k,v); (k,v)::z))
          nil
          m1
        = src1
        andalso !check2List = rev src1),

  testcase "filteri:E"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.filteri
             (check2 (fn (k,v) => v mod 2 = 0))
             empty)
        = []
        andalso !check2List = []),

  testcase "filteri:1"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.filteri
             (check2 (fn (k,v) => v mod 2 = 0))
             m1)
        = List.filter (fn (k,v) => v mod 2 = 0) src1
        andalso !check2List = src1),

  testcase "unionWithi:EE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.unionWithi (checki #2) (empty, empty))
        = []
        andalso !checkiList = []),

  testcase "unionWithi:ET"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.unionWithi (checki #2) (m1, empty))
        = src1
        andalso !checkiList = []),

  testcase "unionWithi:TE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.unionWithi (checki #2) (empty, m2))
        = src2
        andalso !checkiList = []),

  testcase "unionWithi:TT"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.unionWithi (checki #2) (m1, m2))
        = union #2 (src1, src2)
        andalso
        !checkiList
        = merge (fn (k, SOME x, SOME y) => [(k,x,y)] | _ => []) (src1, src2)),

  testcase "unionWith:EE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.unionWith (check #1) (empty, empty))
        = []
        andalso !checkiList = []),

  testcase "unionWith:ET"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.unionWith (check #1) (m1, empty))
        = src1
        andalso !checkiList = []),

  testcase "unionWith:TE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.unionWith (check #1) (empty, m2))
        = src2
        andalso !checkiList = []),

  testcase "unionWith:TT"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.unionWith (check #1) (m1, m2))
        = union #2 (src1, src2)
        andalso
        !checkList
        = merge (fn (k, SOME x, SOME y) => [(x,y)] | _ => []) (src1, src2)),

  testcase "intersectWithi:EE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.intersectWithi (checki #2) (empty, empty))
        = []
        andalso !checkiList = []),

  testcase "intersectWithi:ET"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.intersectWithi (checki #2) (m1, empty))
        = []
        andalso !checkiList = []),

  testcase "intersectWithi:TE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.intersectWithi (checki #2) (empty, m2))
        = []
        andalso !checkiList = []),

  testcase "intersectWithi:TT"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.intersectWithi (checki #2) (m1, m2))
        = intersect #2 (src1, src2)
        andalso
        !checkiList
        = merge (fn (k, SOME x, SOME y) => [(k,x,y)] | _ => []) (src1, src2)),

  testcase "intersectWith:EE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.intersectWith (check #1) (empty, empty))
        = []
        andalso !checkiList = []),

  testcase "intersectWith:ET"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.intersectWith (check #1) (m1, empty))
        = []
        andalso !checkiList = []),

  testcase "intersectWith:TE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.intersectWith (check #1) (empty, m2))
        = []
        andalso !checkiList = []),

  testcase "intersectWith:TT"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.intersectWith (check #1) (m1, m2))
        = intersect #2 (src1, src2)
        andalso
        !checkList
        = merge (fn (k, SOME x, SOME y) => [(x,y)] | _ => []) (src1, src2)),

  testcase "mergeWithi:EE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWithi (checkio #2) (empty, empty))
        = []
        andalso
        !checkioList = []),

  testcase "mergeWithi:TE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWithi (checkio #2) (m1, empty))
        = src1
        andalso
        !checkioList = merge (fn x => [x]) (src1, nil)),

  testcase "mergeWithi:ET"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWithi (checkio #3) (empty, m2))
        = src2
        andalso
        !checkioList = merge (fn x => [x]) (nil, src2)),

  testcase "mergeWithi:TT2"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWithi (checkio #2) (m1, m2))
        = src1
        andalso
        sort #1 (!checkioList) = merge (fn x => [x]) (src1, src2)),

  testcase "mergeWithi:TT3"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWithi (checkio #3) (m1, m2))
        = src2
        andalso
        sort #1 (!checkioList) = merge (fn x => [x]) (src1, src2)),

  testcase "mergeWithi:union"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWithi
             (checkio (fn (_, SOME x, _) => SOME x | (_, _, y) => y))
             (m1, m2))
        = union #2 (src1, src2)),

  testcase "mergeWithi:intersect"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWithi
             (checkio (fn (_, SOME x, SOME _) => SOME x | _ => NONE))
             (m1, m2))
        = intersect #2 (src1, src2)),

  testcase "mergeWith:EE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWith (checko #1) (empty, empty))
        = []
        andalso
        !checkoList = []),

  testcase "mergeWith:TE"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWith (checko #1) (m1, empty))
        = src1
        andalso
        !checkoList = merge (fn (k,x,y) => [(x,y)]) (src1, nil)),

  testcase "mergeWith:ET"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWith (checko #2) (empty, m2))
        = src2
        andalso
        !checkoList = merge (fn (k,x,y) => [(x,y)]) (nil, src2)),

  testcase "mergeWith:TT"
    (fn _ =>
        WordPatriciaMap.listItemsi
          (WordPatriciaMap.mergeWith (checko #1) (m1, m2))
        = src1),

  testcase "collate:EE"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (empty, empty)
        = EQUAL
        andalso !checkList = []),

  testcase "collate:TE"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (m1, empty)
        = GREATER
        andalso !checkList = []),

  testcase "collate:ET"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (empty, m2)
        = LESS
        andalso !checkList = []),

  testcase "collate:TT"
    (fn _ =>
        WordPatriciaMap.collate
          Int.compare
          (m1, m2)
        = List.collate
            (fn ((x1,y1),(x2,y2)) =>
                case Word.compare (x1,x2) of
                  EQUAL => Int.compare (y1,y2)
                | x => x)
            (src1, src2)),

  testcase "collate:TT1"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (m1, m1)
        = EQUAL
        andalso !checkList = map (fn (_,x) => (x,x)) src1),

  testcase "collate:TT2"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (m2, m2)
        = EQUAL
        andalso !checkList = map (fn (_,x) => (x,x)) src2),

  testcase "collate:LL1"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (WordPatriciaMap.singleton (0wx0, 0),
           WordPatriciaMap.singleton (0wx0, 0))
        = EQUAL
        andalso !checkList = [(0, 0)]),

  testcase "collate:LL2"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (WordPatriciaMap.singleton (0wx0, 0),
           WordPatriciaMap.singleton (0wx0, 0xffff))
        = LESS
        andalso !checkList = [(0, 0xffff)]),

  testcase "collate:LL3"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (WordPatriciaMap.singleton (0wxffff, 0),
           WordPatriciaMap.singleton (0wx0, 0xffff))
        = GREATER
        andalso !checkList = []),

  testcase "collate:LB1"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (WordPatriciaMap.singleton (0wx18, 0),
           fromList [(0wx12, 0), (0wx17, 0), (0wx18, 0)])
        = GREATER
        andalso !checkList = []),

  testcase "collate:LB2"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (WordPatriciaMap.singleton (0wx10, 0),
           fromList [(0wx10, 0), (0wx17, 0), (0wx18, 0)])
        = LESS
        andalso !checkList = []),

  testcase "collate:LB3"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (WordPatriciaMap.singleton (0wx12, 0),
           fromList [(0wx12, 0), (0wx17, 0), (0wx18, 0)])
        = LESS
        andalso !checkList = [(0, 0)]),

  testcase "collate:LB4"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (WordPatriciaMap.singleton (0wx13, 0),
           fromList [(0wx12, 0), (0wx17, 0), (0wx1f, 0)])
        = GREATER
        andalso !checkList = []),

  testcase "collate:BL1"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (fromList [(0wx12, 0), (0wx17, 0), (0wx18, 0)],
           WordPatriciaMap.singleton (0wx18, 0))
        = LESS
        andalso !checkList = []),

  testcase "collate:BL2"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (fromList [(0wx10, 0), (0wx17, 0), (0wx18, 0)],
           WordPatriciaMap.singleton (0wx10, 0))
        = GREATER
        andalso !checkList = []),

  testcase "collate:BL3"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (fromList [(0wx12, 0), (0wx17, 0), (0wx18, 0)],
           WordPatriciaMap.singleton (0wx12, 0))
        = GREATER
        andalso !checkList = [(0, 0)]),

  testcase "collate:BL4"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (fromList [(0wx12, 0), (0wx17, 0), (0wx1f, 0)],
           WordPatriciaMap.singleton (0wx13, 0))
        = LESS
        andalso !checkList = []),

  testcase "collate:BB1"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (fromList [(0wx10, 0), (0wx14, 0), (0wx17, 0)],
           fromList [(0wx14, 0), (0wx15, 0), (0wx17, 0)])
        = LESS
        andalso !checkList = []),

  testcase "collate:BB2"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (fromList [(0wx14, 0), (0wx15, 0), (0wx17, 0)],
           fromList [(0wx10, 0), (0wx14, 0), (0wx17, 0)])
        = GREATER
        andalso !checkList = []),

  testcase "collate:BB3"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (fromList [(0wx10, 0), (0wx15, 0), (0wx17, 0)],
           fromList [(0wx10, 0), (0wx14, 0), (0wx17, 0)])
        = GREATER
        andalso !checkList = [(0, 0)]),

  testcase "collate:BB4"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (fromList [(0wx10, 0), (0wx14, 1), (0wx16, 2)],
           fromList [(0wx10, 0), (0wx14, 1), (0wx17, 2)])
        = LESS
        andalso !checkList = [(0, 0), (1, 1)]),

  testcase "collate:BB5"
    (fn _ =>
        WordPatriciaMap.collate
          (check Int.compare)
          (fromList [(0wx10, 0), (0wx14, 1), (0wx17, 2)],
           fromList [(0wx10, 0), (0wx14, 1), (0wx17, 2)])
        = EQUAL
        andalso !checkList = [(0, 0), (1, 1), (2, 2)]),

  endgroup
]
end
