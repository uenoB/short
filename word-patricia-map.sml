(*
 * Big-endian unsigned integer patricia tree.
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
 * WordPatriciaMap is an implementation of the big-endian integer patricia
 * tree proposed in [1]. 
 *
 * WordPatriciaMap provides the same functionality as IntBinaryMap in SML/NJ
 * library.  As noted in [1], in comparison to IntBinaryMap, unionWith is
 * quite faster but lookup is slightly slower.
 *
 * References:
 * [1] C. Okasaki and A. Gill, Fast Mergeable Interger Maps, In Workshop on
 *     ML, pp. 77--86, 1998.
 *
 * NOTE: WordPatriciaMap assumes that word size is less than or equal to
 *       64 bits.  This is hard-coded in leadingZeroBits function.
 *)

signature PATRICIA_MAP =
sig
  (* compatible with ORD_MAP of SML/NJ library.
   * See SML/NJ library manual for details of these functions. *)
  exception NotFound
  type key
  type 'a map
  val empty : 'a map
  val isEmpty : 'a map -> bool
  val singleton : key * 'a -> 'a map
  val insert : 'a map * key * 'a -> 'a map
  val insert' : (key * 'a) * 'a map -> 'a map
  val find : 'a map * key -> 'a option
  val lookup : 'a map * key -> 'a
  val inDomain : 'a map * key -> bool
  val remove : 'a map * key -> 'a map * 'a
  val first : 'a map -> 'a option
  val firsti : 'a map -> (key * 'a) option
  val numItems : 'a map -> int
  val listItems : 'a map -> 'a list
  val listItemsi : 'a map -> (key * 'a) list
  val listKeys : 'a map -> key list
  val collate : ('a * 'a -> order) -> 'a map * 'a map -> order
  val unionWith : ('a * 'a -> 'a) -> 'a map * 'a map -> 'a map
  val unionWithi : (key * 'a * 'a -> 'a) -> 'a map * 'a map -> 'a map
  val intersectWith : ('a * 'b -> 'c) -> 'a map * 'b map -> 'c map
  val intersectWithi : (key * 'a * 'b -> 'c) -> 'a map * 'b map -> 'c map
  val mergeWith
      : ('a option * 'b option -> 'c option) -> 'a map * 'b map -> 'c map
  val mergeWithi
      : (key * 'a option * 'b option -> 'c option) -> 'a map * 'b map -> 'c map
  val app : ('a -> unit) -> 'a map -> unit
  val appi : (key * 'a -> unit) -> 'a map -> unit
  val map : ('a -> 'b) -> 'a map -> 'b map
  val mapi : (key * 'a -> 'b) -> 'a map -> 'b map
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val foldli : (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val foldri : (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val filter : ('a -> bool) -> 'a map -> 'a map
  val filteri : (key * 'a -> bool) -> 'a map -> 'a map
  val mapPartial : ('a -> 'b option) -> 'a map -> 'b map
  val mapPartiali : (key * 'a -> 'b option) -> 'a map -> 'b map
  val exists : ('a -> bool) -> 'a map -> bool
  val existsi : (key * 'a -> bool) -> 'a map -> bool
  val all : ('a -> bool) -> 'a map -> bool
  val alli : (key * 'a -> bool) -> 'a map -> bool
(*
  (* for debug *)
  val dump : int map -> unit
*)
end

structure WordPatriciaMap :> PATRICIA_MAP where type key = word =
struct

  exception NotFound

  type key = Word.word

  datatype 'a tree =
      L of key * 'a
    | B of {prefix : key, mask : word, left : 'a tree, right : 'a tree}
  datatype 'a map = E | T of 'a tree
  (*
   * In B:
   * "prefix" is the common prefix bits among all childrens followed by 0 bit.
   * "mask" indicates the prefix bits in "prefix".
   *   pppppp0111111111   (* prefix *)
   *   1111110000000000   (* mask *)
   *   pppppp0xxxxxxxxx   (* left key *)
   *   pppppp1yyyyyyyyy   (* right key *)
   * By construction, all left keys are less than or equal to "prefix" and
   * right keys are greater than "prefix"; therefore, this tree is also a
   * binary search tree.
   *
   * Arithmetic operations on masks have the following meaning:
   * - Comparing two masks means comparing the length of prefixes; greater
   *   one has longer prefix.
   * - Bitwise AND of two masks means the shorter one of the two.
   *)

  fun b {prefix, mask, left = E, right = t} = t
    | b {prefix, mask, left = t, right = E} = t
    | b {prefix, mask, left = T l, right = T r} =
      T (B {prefix=prefix, mask=mask, left=l, right=r})

  fun leadingZeroBits x =
      let
        val x = Word.orb (x, Word.>> (x, 0w1))
        val x = Word.orb (x, Word.>> (x, 0w2))
        val x = Word.orb (x, Word.>> (x, 0w4))
        val x = Word.orb (x, Word.>> (x, 0w8))
        val x = Word.orb (x, Word.>> (x, 0w16))
        val x = Word.orb (x, Word.>> (x, 0w32))  (* for 64 bit word *)
      in
        Word.notb x
      end

  fun join ((k1, t1), (k2, t2)) =
      let
        val m = leadingZeroBits (Word.xorb (k1, k2))
        val p = Word.orb (Word.andb (k1, m), Word.>> (Word.notb m, 0w1))
      in
        if k1 < k2
        then B {prefix = p, mask = m, left = t1, right = t2}
        else B {prefix = p, mask = m, left = t2, right = t1}
      end

  fun joinTree ((k1, E), (k2, t2)) = t2
    | joinTree ((k1, t1), (k2, E)) = t1
    | joinTree ((k1, T t1), (k2, T t2)) = T (join ((k1, t1), (k2, t2)))

  fun search (L (k1, v1), k2) =
      if k1 = k2 then SOME v1 else NONE
    | search (B {prefix=p1, mask=m1, left=l1, right=r1}, k2) =
      if k2 <= p1 then search (l1, k2) else search (r1, k2)

  fun notSamePrefix (key1, key2, mask) =
      Word.andb (key1, mask) <> Word.andb (key2, mask)

  (* the most general map manipulation operation *)
  fun merge f (t1 as L (k1, v1), t2 as L (k2, v2)) =
      if k1 = k2 then mergeLeaf f (k1, SOME v1, SOME v2)
      else joinTree ((k1, merge1E f t1), (k2, mergeE2 f t2))
    | merge f (t1 as B (n1 as {prefix=p1, mask=m1, ...}), t2 as L (k2, _)) =
      if notSamePrefix (p1, k2, m1)
      then joinTree ((p1, merge1E f t1), (k2, mergeE2 f t2))
      else merge1T f (n1, (k2, t2))
    | merge f (t1 as L (k1, v1), t2 as B (n2 as {prefix=p2, mask=m2, ...})) =
      if notSamePrefix (k1, p2, m2)
      then joinTree ((k1, merge1E f t1), (p2, mergeE2 f t2))
      else mergeT2 f ((k1, t1), n2)
    | merge f (t1 as B (n1 as {prefix=p1, mask=m1, left=l1, right=r1}),
               t2 as B (n2 as {prefix=p2, mask=m2, left=l2, right=r2})) =
      if notSamePrefix (p1, p2, Word.andb (m1, m2))
      then joinTree ((p1, merge1E f t1), (p2, mergeE2 f t2))
      else if m1 < m2 then merge1T f (n1, (p2, t2))
      else if m1 > m2 then mergeT2 f ((p1, t1), n2)
      else b {prefix=p1, mask=m1, left= merge f (l1,l2), right= merge f (r1,r2)}
  and merge1T f ({prefix=p1, mask=m1, left=l1, right=r1}, (k2, t2)) =
      if k2 <= p1
      then b {prefix=p1, mask=m1, left = merge f (l1,t2), right = merge1E f r1}
      else b {prefix=p1, mask=m1, left = merge1E f l1, right = merge f (r1,t2)}
  and mergeT2 f ((k1, t1), {prefix=p2, mask=m2, left=l2, right=r2}) =
      if k1 <= p2
      then b {prefix=p2, mask=m2, left = merge f (t1,l2), right = mergeE2 f r2}
      else b {prefix=p2, mask=m2, left = mergeE2 f l2, right = merge f (t1,r2)}
  and merge1E f (L (k1, v1)) = mergeLeaf f (k1, SOME v1, NONE)
    | merge1E f (B {prefix=p1, mask=m1, left=l1, right=r1}) =
      b {prefix=p1, mask=m1, left = merge1E f l1, right = merge1E f r1}
  and mergeE2 f (L (k2, v2)) = mergeLeaf f (k2, NONE, SOME v2)
    | mergeE2 f (B {prefix=p2, mask=m2, left=l2, right=r2}) =
      b {prefix=p2, mask=m2, left = mergeE2 f l2, right = mergeE2 f r2}
  and mergeLeaf f (k, v1, v2) =
      case f (k, v1, v2) of SOME v => T (L (k, v)) | NONE => E

  (* specialized version of merge.
   * fun add (t1, t2) = merge #3 (t1, L t2)
   *)
  fun add (t1 as L (k1, v1), t2 as (k2, v2)) =
      if k1 = k2 then L t2
      else join ((k1, t1), (k2, L t2))
    | add (t1 as B {prefix=p1, mask=m1, left=l1, right=r1}, t2 as (k2, _)) =
      if notSamePrefix (p1, k2, m1)
      then join ((p1, t1), (k2, L t2))
      else if k2 <= p1
      then B {prefix=p1, mask=m1, left = add (l1, t2), right = r1}
      else B {prefix=p1, mask=m1, left = l1, right = add (r1, t2)}

  (* specialized version of merge.
   * fun union f (t1, t2) = merge (f' f) (t1, t2)
   * and f' f (k, SOME x, SOME y) = SOME (f (k, x, y))
   *   | f' f (k, SOME x, NONE) = SOME x
   *   | f' f (k, NONE, y) = y
   *)
  fun union f (t1 as L (k1, v1), t2 as L (k2, v2)) =
      if k1 = k2 then L (k1, f (k1, v1, v2))
      else join ((k1, t1), (k2, t2))
    | union f (t1 as B (n1 as {prefix=p1, mask=m1, ...}), t2 as L (k2, _)) =
      if notSamePrefix (p1, k2, m1)
      then join ((p1, t1), (k2, t2))
      else union1T f (n1, (k2, t2))
    | union f (t1 as L (k1, _), t2 as B (n2 as {prefix=p2, mask=m2, ...})) =
      if notSamePrefix (k1, p2, m2)
      then join ((k1, t1), (p2, t2))
      else unionT2 f ((k1, t1), n2)
    | union f (t1 as B (n1 as {prefix=p1, mask=m1, left=l1, right=r1}),
               t2 as B (n2 as {prefix=p2, mask=m2, left=l2, right=r2})) =
      if notSamePrefix (p1, p2, Word.andb (m1, m2))
      then join ((p1, t1), (p2, t2))
      else if m1 < m2 then union1T f (n1, (p2, t2))
      else if m2 < m1 then unionT2 f ((p1, t1), n2)
      else B {prefix=p1, mask=m1, left= union f (l1,l2), right= union f (r1,r2)}
  and union1T f ({prefix=p1, mask=m1, left=l1, right=r1}, (k2, t2)) =
      if k2 <= p1
      then B {prefix=p1, mask=m1, left = union f (l1,t2), right = r1}
      else B {prefix=p1, mask=m1, left = l1, right = union f (r1,t2)}
  and unionT2 f ((k1, t1), {prefix=p2, mask=m2, left=l2, right=r2}) =
      if k1 <= p2
      then B {prefix=p2, mask=m2, left = union f (t1,l2), right = r2}
      else B {prefix=p2, mask=m2, left = l2, right = union f (t1,r2)}

  (* specialized version of merge.
   * fun inter f (t1, t2) = merge (f' f) (t1, t2)
   * and f' f (k, SOME x, SOME y) = SOME (f (k, x, y))
   *   | f' f _ = NONE
   *)
  fun inter f (t1, L (k2, v2)) =
      (case search (t1, k2) of SOME v1 => T (L (k2, f (k2,v1,v2))) | NONE => E)
    | inter f (L (k1, v1), t2) =
      (case search (t2, k1) of SOME v2 => T (L (k1, f (k1,v1,v2))) | NONE => E)
    | inter f (t1 as B (n1 as {prefix=p1, mask=m1, left=l1, right=r1}),
               t2 as B (n2 as {prefix=p2, mask=m2, left=l2, right=r2})) =
      if notSamePrefix (p1, p2, Word.andb (m1, m2)) then E
      else if m1 < m2
      then if p2 <= p1 then inter f (l1, t2) else inter f (r1, t2)
      else if m2 < m1
      then if p1 <= p2 then inter f (t1, l2) else inter f (t1, r2)
      else b {prefix=p1, mask=m1, left= inter f (l1,l2), right= inter f (r1,r2)}

  val empty = E
  fun isEmpty E = true
    | isEmpty (T _) = false
  fun singleton t = T (L t)
  fun insert (E, k, v) = T (L (k, v))
    | insert (T t, k, v) = T (add (t, (k, v)))
  fun insert' (t1, E) = T (L t1)
    | insert' (t1, T t2) = T (add (t2, t1))
  fun find (E, k) = NONE
    | find (T t, k) = search (t, k)
  fun lookup x = case find x of SOME x => x | NONE => raise NotFound
  fun inDomain x = case find x of SOME _ => true | NONE => false
  fun unionWithi f (t1, E) = t1
    | unionWithi f (E, t2) = t2
    | unionWithi f (T t1, T t2) = T (union f (t1, t2))
  fun unionWith f x = unionWithi (fn (_,x,y) => f (x,y)) x
  fun intersectWithi f (_, E) = E
    | intersectWithi f (E, _) = E
    | intersectWithi f (T t1, T t2) = inter f (t1, t2)
  fun intersectWith f x = intersectWithi (fn (_,x,y) => f (x,y)) x
  fun mergeWithi f (E, E) = E
    | mergeWithi f (T t1, E) = merge1E f t1
    | mergeWithi f (E, T t2) = mergeE2 f t2
    | mergeWithi f (T t1, T t2) = merge f (t1, t2)
  fun mergeWith f x = mergeWithi (fn (_,x,y) => f (x,y)) x

  fun remove' (t1 as L (k1, v1), k2) =
      if k1 = k2 then (E, v1) else raise NotFound
    | remove' (B {prefix=p1, mask=m1, left=l1, right=r1}, k2) =
      if k2 <= p1
      then case remove' (l1, k2) of
             (l1, v1) => (b {prefix=p1, mask=m1, left = l1, right = T r1}, v1)
      else case remove' (r1, k2) of
             (r1, v1) => (b {prefix=p1, mask=m1, left = T l1, right = r1}, v1)
  fun remove (E, k) = raise NotFound
    | remove (T t, k) = remove' (t, k)

  fun filteri' f (t as L p) = if f p then T t else E
    | filteri' f (B {prefix=p, mask=m, left=l, right=r}) =
      b {prefix=p, mask=m, left = filteri' f l, right = filteri' f r}
  fun filteri f E = E
    | filteri f (T t) = filteri' f t
  fun filter f x = filteri (fn (_,v) => f v) x

  fun mapi' f (L (k, v)) = L (k, f (k, v))
    | mapi' f (B {prefix=p, mask=m, left=l, right=r}) =
      B {prefix=p, mask=m, left = mapi' f l, right = mapi' f r}
  fun mapi f E = E
    | mapi f (T t) = T (mapi' f t)
  fun map f x = mapi (fn (_,v) => f v) x

  fun mapPartiali' f (L (k, v)) =
      (case f (k, v) of SOME v => T (L (k, v)) | NONE => E)
    | mapPartiali' f (B {prefix=p, mask=m, left=l, right=r}) =
      b {prefix=p, mask=m, left = mapPartiali' f l, right = mapPartiali' f r}
  fun mapPartiali f E = E
    | mapPartiali f (T t) = mapPartiali' f t
  fun mapPartial f x = mapPartiali (fn (_,v) => f v) x

  fun firsti' (L t) = t
    | firsti' (B {left, ...}) = firsti' left
  fun firsti E = NONE
    | firsti (T t) = SOME (firsti' t)
  fun first E = NONE
    | first (T t) = SOME (#2 (firsti' t))

  fun minKey (prefix, mask) = Word.andb (prefix, mask)

  fun collate' f (L t1, L t2) = collateLeaf f (t1, t2)
    | collate' f (B {prefix=p1, mask=m1, left=l1, ...}, L (t2 as (k2,_))) =
      if p1 < k2 then LESS
      else if k2 <= minKey (p1, m1) then GREATER
      else (case collateLeaf f (firsti' l1, t2) of EQUAL => GREATER | x => x)
    | collate' f (L (t1 as (k1,_)), B {prefix=p2, mask=m2, left=l2, ...}) =
      if p2 < k1 then GREATER
      else if k1 <= minKey (p2, m2) then LESS
      else (case collateLeaf f (t1, firsti' l2) of EQUAL => LESS | x => x)
    | collate' f (B {prefix=p1, mask=m1, left=l1, right=r1},
                  B {prefix=p2, mask=m2, left=l2, right=r2}) =
      if p1 < minKey (p2, m2) then LESS
      else if minKey (p1, m1) > p2 then GREATER
      else (case collate' f (l1, l2) of EQUAL => collate' f (r1, r2) | x => x)
  and collateLeaf f ((k1, v1), (k2, v2)) =
      if k1 < k2 then LESS else if k1 > k2 then GREATER else f (v1, v2)

  fun collate f (E, E) = EQUAL
    | collate f (E, T _) = LESS
    | collate f (T _, E) = GREATER
    | collate f (T t1, T t2) = collate' f (t1, t2)

  fun toList E = nil
    | toList (T t) = [t]

  fun numItems' n nil = n
    | numItems' n (L _ :: t) = numItems' (n+1) t
    | numItems' n (B {left, right, ...} :: t) = numItems' n (left::right::t)
  fun numItems x = numItems' 0 (toList x)

  fun listItemsi' f a nil = a
    | listItemsi' f a (L h :: t) = listItemsi' f (f h :: a) t
    | listItemsi' f a (B {left=l,right=r,...} :: t) = listItemsi' f a (r::l::t)
  fun listItemsi x = listItemsi' (fn x => x) nil (toList x)
  fun listKeys x = listItemsi' #1 nil (toList x)
  fun listItems x = listItemsi' #2 nil (toList x)

  fun appi' f (L t) = f t : unit
    | appi' f (B {left, right, ...}) = (appi' f left; appi' f right)
  fun appi f E = ()
    | appi f (T t) = appi' f t
  fun app f x = appi (fn (_,v) => f v) x

  fun foldli' f z nil = z
    | foldli' f z (L (k,v)::t) = foldli' f (f (k, v, z)) t
    | foldli' f z (B {left, right, ...}::t) = foldli' f z (left::right::t)
  fun foldli f z x = foldli' f z (toList x)
  fun foldl f z x = foldli (fn (_,v,z) => f (v,z)) z x

  fun foldri' f z nil = z
    | foldri' f z (L (k,v)::t) = foldri' f (f (k, v, z)) t
    | foldri' f z (B {left, right, ...}::t) = foldri' f z (right::left::t)
  fun foldri f z x = foldri' f z (toList x)
  fun foldr f z x = foldri (fn (_,v,z) => f (v,z)) z x

  fun existsi' f nil = false
    | existsi' f (L h :: t) = f h orelse existsi' f t
    | existsi' f (B {left, right, ...}::t) = existsi' f (left::right::t)
  fun existsi f x = existsi' f (toList x)
  fun exists f x = existsi (fn (_,v) => f v) x

  fun alli' f nil = true
    | alli' f (L h :: t) = f h andalso alli' f t
    | alli' f (B {left, right, ...}::t) = alli' f (left::right::t)
  fun alli f x = alli' f (toList x)
  fun all f x = alli (fn (_,v) => f v) x

  (* for debug *)
  fun bin32 w = StringCvt.padLeft #"0" 32 (Word.fmt StringCvt.BIN w)
  fun dump E = print "E\n"
    | dump (T (L (key, value))) =
      print ("L " ^ bin32 key ^ " : " ^ Int.toString value ^ "\n")
    | dump (T (B {prefix, mask, left, right})) =
      let
        val p = bin32 prefix
        val m = "0" ^ Word.fmt StringCvt.BIN (Word.>> (Word.notb mask, 0w1))
        val p1 = String.substring (p, 0, size p - size m)
        val p2 = String.extract (p, size p - size m, NONE)
        val _ = if m = p2 then () else raise Fail "dump: inconsistent"
        val p2 = CharVector.map (fn #"0" => #"|" | _ => #"_") p2
      in
        print ("B " ^ p1 ^ p2 ^ "\n");
        dump (T left);
        dump (T right)
      end

end
