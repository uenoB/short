(*
 * Benchmark suite for WordPatriciaMap.
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

val r = Random.rand (0x12345678, 0x12345678)
val rsi = List.tabulate (400000, fn _ => Random.randInt r)
val rsw = List.map Word.fromInt rsi

local
  fun loop z nil = z
    | loop z (h::t) = loop (WordPatriciaMap.insert (z, h, h)) t
in
fun insertP l = loop WordPatriciaMap.empty l
end

local
  fun loop z nil = z
    | loop z (h::t) = loop (IntBinaryMap.insert (z, h, h)) t
in
  fun insertB l = loop IntBinaryMap.empty l
end

(* insert *)
fun benchP1 () = insertP rsw
fun benchB1 () = insertB rsi

(* lookup *)
val pm = insertP rsw
val bm = insertB rsi

local
  fun loop nil = ()
    | loop (h::t) = (WordPatriciaMap.lookup (pm, h); loop t)
in
  fun benchP2 () = loop rsw
end

local
  fun loop nil = ()
    | loop (h::t) = (IntBinaryMap.lookup (bm, h); loop t)
in
  fun benchB2 () = loop rsi
end

(* remove *)
local
  fun loop nil = ()
    | loop (h::t) = (WordPatriciaMap.remove (pm, h); loop t)
in
  fun benchP3 () = loop rsw
end

local
  fun loop nil = ()
    | loop (h::t) = (IntBinaryMap.remove (bm, h); loop t)
in
  fun benchB3 () = loop rsi
end

(* unionWith *)
val pms =
    let fun f z nil = z
          | f z l = f (insertP (List.take (l, 1000)) :: z) (List.drop (l, 1000))
    in f nil rsw end
val bms =
    let fun f z nil = z
          | f z l = f (insertB (List.take (l, 1000)) :: z) (List.drop (l, 1000))
    in f nil rsi end

local
  fun loop (x::y::t) = (WordPatriciaMap.unionWith #2 (x, y); loop t)
    | loop _ = ()
in
  fun benchP4 () = loop pms
end

local
  fun loop (x::y::t) = (IntBinaryMap.unionWith #2 (x, y); loop t)
    | loop _ = ()
in
  fun benchB4 () = loop bms
end


(* unionWith (accum) *)
local
  fun loop z (h::t) = loop (WordPatriciaMap.unionWith #2 (z, h)) t
    | loop z nil = z
in
  fun benchP5 () = loop WordPatriciaMap.empty pms
end

local
  fun loop z (h::t) = loop (IntBinaryMap.unionWith #2 (z, h)) t
    | loop z nil = z
in
  fun benchB5 () = loop IntBinaryMap.empty bms
end

(* intersectWith *)
local
  fun loop (x::y::t) = (WordPatriciaMap.intersectWith #2 (x, y); loop t)
    | loop _ = ()
in
  fun benchP6 () = loop pms
end

local
  fun loop (x::y::t) = (IntBinaryMap.intersectWith #2 (x, y); loop t)
    | loop _ = ()
in
  fun benchB6 () = loop bms
end





(* toplevel *)

fun loop f z 0 = z
  | loop f {sys, usr} n =
    let
      val t = Timer.startCPUTimer ()
      val _ = f ()
      val {sys=sys2, usr=usr2} = Timer.checkCPUTimer t
      val z = {sys = sys + Time.toReal sys2, usr = usr + Time.toReal usr2}
    in
      loop f z (n-1)
    end
fun bench f n =
    let
      val {sys, usr} = loop f {sys=0.0, usr=0.0} n
    in
      {sys = sys / real n, usr = usr / real n}
    end


val _ = print "WordPatriciaMap.insert: "
val _ = print (Real.toString (#usr (bench benchP1 10)) ^ "\n")
val _ = print "IntBinaryMap.insert: "
val _ = print (Real.toString (#usr (bench benchB1 10)) ^ "\n")

val _ = print "WordPatriciaMap.lookup: "
val _ = print (Real.toString (#usr (bench benchP2 10)) ^ "\n")
val _ = print "IntBinaryMap.lookup: "
val _ = print (Real.toString (#usr (bench benchB2 10)) ^ "\n")

val _ = print "WordPatriciaMap.remove: "
val _ = print (Real.toString (#usr (bench benchP3 10)) ^ "\n")
val _ = print "IntBinaryMap.remove: "
val _ = print (Real.toString (#usr (bench benchB3 10)) ^ "\n")

val _ = print "WordPatriciaMap.unionWith: "
val _ = print (Real.toString (#usr (bench benchP4 10)) ^ "\n")
val _ = print "IntBinaryMap.unionWith: "
val _ = print (Real.toString (#usr (bench benchB4 10)) ^ "\n")

val _ = print "WordPatriciaMap.unionWith (accum): "
val _ = print (Real.toString (#usr (bench benchP5 10)) ^ "\n")
val _ = print "IntBinaryMap.unionWith (accum): "
val _ = print (Real.toString (#usr (bench benchB5 10)) ^ "\n")

val _ = print "WordPatriciaMap.intersectWith: "
val _ = print (Real.toString (#usr (bench benchP6 10)) ^ "\n")
val _ = print "IntBinaryMap.intersectWith: "
val _ = print (Real.toString (#usr (bench benchB6 10)) ^ "\n")
