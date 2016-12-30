(*
 * Pretty small unit test framework.
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
 * UnitTest is a simple framework for unit testing.
 *
 * In this framework, a test case is a function of type unit -> bool,
 * which represents a single proposition that is expected to be true.
 * UnitTest.run executes test cases and reports failed ones with their
 * messages printed during their execution by UnitTest.print.
 *
 * A test suite is organized as a possibly nested test group containing
 * multiple test cases.  A test case or test group is identified by a path
 * in which each component separated by a slash ("/") represents each test
 * group.  For example, "A/B/T" indicates test case or test group "T" in
 * test group "B" in test group "A".
 *
 * A test group may have one or more "setup" and "teardown" functions
 * that are invoked every time before and after a test case in the same
 * (or a descendant) test group is invoked, respectively.
 *
 * For convenience, the global test suite is implicitly organized.
 * Instead of making an all-in-one test group by hand, you can obtain all
 * test cases and test groups created so far by invoking UnitTest.allTests.
 *)

structure UnitTest :> sig

  (* The type of test stuff *)
  type test

  (* Create a test case and test group of the given name. *)
  val testcase : string -> (unit -> bool) -> test
  val testgroup : string -> test list -> test

  (* Create a setup and teardown function. *)
  val setup : (unit -> 'a) -> test
  val teardown : (unit -> 'a) -> test

  (* No meaning; this is just a placeholder for the end of a test group. *)
  val endgroup : test

  (* Print progress message of a test case.
   * The message will be included in the test summary if the test case fails.
   *)
  val print : string -> unit

  (* Obtain all test cases and test groups that are created so far and
   * do not belong to any test group. *)
  val allTests : unit -> test list

  (* Select tests specified by paths from the given tests. *)
  val selectByPaths : string list -> test list -> test list

  (* List the paths of all tests. *)
  val listPaths : test list -> string list

  (* Run the given tests.  The progress and test result summary are
   * printed out to standard error and standard output. *)
  val run : test list -> {ok:int, ng:int}

  (* The command line (or interactive) interface for test suites.
   * It takes a list of command line arguments.  Each argument must be a
   * path to a test case or test group to be executed.  If no argument is
   * given, all tests are executed.  If "-l" option is given, it prints
   * test paths instead of running tests.  This function returns true if
   * no error and failure occurred. *)
  val main : string list -> bool

end =
struct

  infix + ^ := =
  infixr :: @
  val op + = Int.+
  val op ^ = String.^
  val op := = General.:=
  val op @ = List.@

  fun concatMap f x = List.concat (List.map f x)

  fun quotient eq nil = nil
    | quotient eq (h::t) =
      case List.partition (fn x => eq (h, x)) t of
        (x, y) => (h :: x) :: quotient eq y

  fun unchomp s = if String.isSuffix "\n" s then s else s ^ "\n"

  fun printOut s = let open TextIO in output (stdOut, s) end
  fun printErr s = let open TextIO in output (stdErr, s); flushOut stdErr end

  fun uncaughtMsg e = "** uncaught exception " ^ General.exnName e ^ " ("
                      ^ General.exnMessage e ^ ") **"

  datatype test_item =
      GROUP of string * test list
    | TEST of string * (unit -> bool)
    | SETUP of unit -> unit
    | TEARDOWN of unit -> unit
    | EMPTY
  withtype test = {root : bool ref, test : test_item}

  val roots = ref nil : test list ref

  fun allTests () = List.rev (!roots)

  fun nameOf (GROUP (name, _)) = SOME name
    | nameOf (TEST (name, _)) = SOME name
    | nameOf _ = NONE

  fun makeTest test =
      let val t = {root = ref true, test = test}
      in roots := (t :: !roots); t end

  val endgroup = {root = ref false, test = EMPTY}
  fun setup f = makeTest (SETUP (fn () => (f (); ())))
  fun teardown f = makeTest (TEARDOWN (fn () => (f (); ())))
  fun testcase name f = makeTest (TEST (name, f))
  fun testgroup name tests =
      (List.app (fn x => #root x := false) tests;
       roots := List.filter (fn x => !(#root x)) (!roots);
       makeTest (GROUP (name, tests)))

  fun filterTest paths test =
      case #test test of
        EMPTY => nil
      | SETUP _ => [test]
      | TEARDOWN _ => [test]
      | TEST (n, _) =>
        if List.exists (fn h::nil => h = n | _ => false) paths
        then [test] else nil
      | GROUP (n, ts) =>
        case List.mapPartial (fn h::t => if h = n then SOME t else NONE
                               | _ => NONE) paths of
          nil => nil
        | paths =>
          if List.exists (fn nil => true | _ => false) paths
          then [test]
          else case filterTests paths ts of
                 nil => nil
               | ts => case List.mapPartial (fn x => nameOf (#test x)) ts of
                         nil => nil
                       | _::_ => [{root = ref false, test = GROUP (n, ts)}]
  and filterTests paths tests = concatMap (filterTest paths) tests
  fun selectByPaths names tests =
      filterTests (List.map (String.fields (fn c => c = #"/")) names) tests

  fun listPaths' path {test = EMPTY, ...} = []
    | listPaths' path {test = SETUP _, ...} = []
    | listPaths' path {test = TEARDOWN _, ...} = []
    | listPaths' path {test = TEST (name, _), ...} = [path ^ name]
    | listPaths' path {test = GROUP (name, tests), ...} =
      concatMap (listPaths' (path ^ name ^ "/")) tests
  fun listPaths tests = concatMap (listPaths' "") tests

  datatype result = DUP | OK | NG of string

  fun summarize rs =
      let
        val ok = List.length (List.filter (fn (x,OK) => true | _ => false) rs)
        val ng = List.length (List.filter (fn (x,NG _) => true | _ => false) rs)
      in
        printErr "\n";
        List.app (fn (n, NG s) => printOut ("FAIL: " ^ n ^ "\n" ^ unchomp s)
                   | (n, DUP) => printOut ("WARN: duplicate test `" ^ n ^ "'\n")
                   | _ => ())
                 rs;
        {ok = ok, ng = ng}
      end

  val msg = ref ""
  fun print s = msg := (!msg ^ s)
  fun getMessage suffix =
      case !msg of
        "" => suffix
      | s => (msg := ""; "--\n" ^ unchomp s ^ "--\n" ^ suffix)

  fun runTest env EMPTY = []
    | runTest env (SETUP _) = []
    | runTest env (TEARDOWN _) = []
    | runTest {path, setup=s, teardown=t} (GROUP (name, tests)) =
      runGroup {path = path ^ name ^ "/", setup = s, teardown = t} tests
    | runTest {path, setup, teardown} (TEST (name, f)) =
      let
        datatype try = S of bool | F of exn
        val name = path ^ name
        val _ = msg := ""
        val r = (List.app (fn f => f ()) setup; S (f ())) handle e => F e
        val _ = List.app (fn f => f () handle _ => ()) teardown
      in
        case r of
          S true => (printErr "."; msg := ""; [(name, OK)])
        | S false => (printErr "x"; [(name, NG (getMessage ""))])
        | F e => (printErr "E"; [(name, NG (getMessage (uncaughtMsg e)))])
      end

  and runGroup {path, setup, teardown} tests =
      let
        val tests = List.map #test tests
        val s = List.mapPartial (fn SETUP f => SOME f | _ => NONE) tests
        val t = List.mapPartial (fn TEARDOWN f => SOME f | _ => NONE) tests
        val env = {path = path, setup = setup @ s, teardown = t @ teardown}
      in
        List.mapPartial (fn n::_::_ => SOME (path ^ n, DUP) | _ => NONE)
                        (quotient (op =) (List.mapPartial nameOf tests))
        @ concatMap (runTest env) tests
      end

  fun run ts =
      summarize (runGroup {path = "", setup = nil, teardown = nil} ts)

  fun main args =
      let
        val (listMode, args) =
            case args of
              "-l"::t => (true, t)
            | "--"::t => (false, t)
            | _ => (false, args)
        val tests =
            case args of
              nil => allTests ()
            | _::_ => selectByPaths args (allTests ())
      in
        case tests of
          nil => (printErr "no tests.\n"; false)
        | _::_ =>
          if listMode
          then (List.app (fn x => printOut (x ^ "\n")) (listPaths tests); true)
          else let val {ok, ng} = run tests
               in printOut (Int.toString (ok + ng) ^ " tests total, "
                            ^ Int.toString ng ^ " tests failed.\n");
                  ng = 0
               end
      end

end
