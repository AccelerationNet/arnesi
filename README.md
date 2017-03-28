# This is a fork of arnesi utilities suite

 * http://common-lisp.net/project/bese/arnesi.html), which seems to be orphaned.

## Status and Suggested replacements

Many of the utilities found in arnesi have been fleshed out into a more full
libraries or replaced by libraries with more features  Arnesi is mostly provided
as support for legacy apps making use of its functions (UCW).

 * General Utilities:
 ** Alexandria: https://common-lisp.net/project/alexandria/
 * Anaphoric macros (awhen aif acond etc)
 ** Alexandria and Anaphora: https://common-lisp.net/project/anaphora/
 * Accumulators/collectors:
 ** collectors: https://github.com/AccelerationNet/collectors
 * Logging
 ** a-cl-logger: https://github.com/AccelerationNet/a-cl-logger
 * CSV
 ** cl-csv: https://github.com/AccelerationNet/cl-csv
 * Testing/5am:
 ** Lisp-unit2 https://github.com/AccelerationNet/lisp-unit2

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
