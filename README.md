Standard ML Short Pieces
========================

This is a collection of miscellaneous libraries written in Standard ML.

Each library in this collection is (to be) reasonably small (typically
less than 400 lines of code), independent of each other, well-tuned,
well-tested, and licensed under MIT or BSD-style license so that users
can read the code, can pick up their favorites, and can integrate them
into their own projects.

Each .sml file constitutes a library.  The copyright notice, license
terms, description, and signature of each library are included in each
.sml file.  Test and benchmark suite for each library are in tests/
and bench/ directory, respectively.  The test suite would also be
useful for users to understand the usage of the libraries.

To run the test suite, run tests/run.sml or tests/run.mlb.

## Catalogue of Short Pieces ##

### [PrettyPrintEngine](pretty-print-engine.sml)

An implementation of Wadler's prettier printer with some additional
primitives such as WEAKBREAK and ALIGN.

### [PrettyPrintParser](pretty-print-parser.sml)

A parser for a serialized format of PrettyPrintEngine.doc as a way of
its concise construction.  It also provides a DSL that allows users
to embed ML values in documents.  It depends on PrettyPrintEngine.

### [UnitTest](unit-test.sml)

A simple framework for unit testing.
See tests/test-list.sml for an example of testing with UnitTest.
