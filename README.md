# This is a fork of arnesi utilities suite

 * http://common-lisp.net/project/bese/arnesi.html), which seems to be orphaned.
 * [API Reference](https://common-lisp.net/project/bese/docs/arnesi/html/index.html)

## Status

I would not suggest starting new projects using arnesi as a
dependency.  Most of the code is available in more complete libraries
elsewhere.  While arnesi was a good building block 10 years ago, with
quicklisp, alexandria, and boat loads of newer libraries, there are
better more targeted solutions for most of the functions provided here

Arnesi is mostly provided to keep the rest of the bese.it libraries
working (eg: ucw)

## Plans:

 * This library is in indefinite maintenance and will not be added to
 * Bug reports, fixes and test cases for current behavior are all
   accepted

## Suggested replacements

Many of the utilities found in arnesi have been fleshed out into a more full
libraries or replaced by libraries with more features  Arnesi is mostly provided
as support for legacy apps making use of its functions (UCW).

* General Utilities:
  + Alexandria: https://common-lisp.net/project/alexandria/
* Anaphoric macros (awhen aif acond etc)
  + Alexandria and Anaphora: https://common-lisp.net/project/anaphora/
* Accumulators/collectors:
  + collectors: https://github.com/AccelerationNet/collectors
* Logging
  + a-cl-logger: https://github.com/AccelerationNet/a-cl-logger
* CSV
  + cl-csv: https://github.com/AccelerationNet/cl-csv
* Testing/5am:
  + Lisp-unit2 https://github.com/AccelerationNet/lisp-unit2

## Changes from darcs arnesi:

 * It includes some fixes to ```call/cc``` to better handle special variables.
 * parse floats handles more types (null, number, float and string)
 * uses collectors library to implement its collectors (This library was
   additions written to the existing arnesi accumulators)
 * asdf 3 support
 * change default logging
 * exported with-accessors*
 * queue api improvements (queue-delete-if, queue-head,
   queue-last, queue-pop, handle null)


## Bug fixes

 * escape-as-uri: better null handling
 * fixes for symbol macros from drewc
 * lots of call/cc stuff


## Authors

 * 2002-2006 Edward Marco Baringer
 * 2006-? Luca Capello http://luca.pca.it <luca@pca.it>
 * 2006-2007 Attila Lendvai
 * 2006-2017 Russ Tyndall, Acceleration.net http://www.acceleration.net
