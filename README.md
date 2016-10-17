fn.el -- Functional utilities for Emacs Lisp.
-----

__fn.el__ provides essential an anonymous function facility focused on concision and readability

------------------------------------------------------------

## Installation

Place `fn.el` in any directory on your `load-path` and:

    (require 'fn)

------------------------------------------------------------

## API

* [fn](#fn-rest-body) `(&rest body)`
* [fn:](#fn-rest-body) `(&rest body)`

### fn `(&rest BODY)`

Return a function defined by BODY.

Intended for inline use where concision is desired.  If creating a function to
bind as a function value, use `lambda' or `-lambda'.

Return a function defined by BODY.

The definition BODY may use anaphoric parameters to refer to the arguments. For
a single-argument function, use `<>` or `it`. For a multiple-argument function,
use `<1>` to refer to the first argument, `<2>` to refer to the second, and so
on up to `<9>`.

If applied to a literal, creates a constant function, or equivalently, a thunk
(since it can be called with any number of arguments).

    (-map (fn (* <> <>)) (number-sequence 0 10))
    ;; (0 1 4 9 16 25 36 49 64 81 100)

    (-map (fn (/ (-sum <>)
                 (length <>)))
          '((3.0 4.0 5.0 5.0 10.0)
            (1.0 2.0 2.0 2.0)
            (1 5)))
    ;; (5.4 1.75 3)
    ;; find average of each list

    (-filter (fn (zerop (mod <> 3)))
             (number-sequence 1 10))
    ;; (3 6 9)
    
### fn: `(&rest BODY)`

Return a function defined by (BODY).

Intended for inline use where concision is desired.  If creating a function to
bind as a function value, use `lambda' or `-lambda'.

Return a function defined by BODY.

The definition BODY may use the anaphoric parameters `<>`, `it` and `<1>` to refer to
the first argument, `<2>` to refer to the second, and so on up to `<9>`.

    (-map (fn: * <> <>) (number-sequence 0 10))
    ;; (0 1 4 9 16 25 36 49 64 81 100)

    (-filter (fn: > it 0)
            '(-5 2 0 0 3 -1 0 4))
    ;; (2 3 4)
