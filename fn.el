;;; fn.el --- Concise anonymous functions for Emacs Lisp   -*- lexical-binding: t -*-

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: functional
;; Version: 0.1.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (dash "2.12.1") (dash-functional "1.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; fn.el provides essential an anonymous function facility focused on concision
;; and readability.

;;; Code:

(require 'cl-lib)
(require 'dash-functional)



(defmacro fn (&rest body)
  "Return a function defined by BODY.

Intended for inline use where concision is desired.  If creating a function to
bind as a function value, use `lambda' or `-lambda'.

The definition BODY may use anaphoric parameters to refer to the arguments. For
a single-argument function, use <> or it. For a multiple-argument function, use
<1> to refer to the first argument, <2> to refer to the second, and so on up to
<9>.

If applied to a literal, creates a constant function, or equivalently, a thunk (
since it can be called with any number of arguments).

Examples:

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

  (funcall (fn 7))
  ;; 7"
  (declare (debug 'body))
  (let* ((argsym       (make-symbol "ARGS"))
         (symbol-vars  '(<> it))
         (digit-vars   '(<1> <2> <3> <4> <5> <6> <7> <8> <9>))
         (symbols      (eval (backquote (-flatten ',body))))
         (digit-vars-used     (-intersection digit-vars symbols))
         (symbol-vars-used    (-intersection symbol-vars symbols))
         (highest-digit-used  (-last-item (sort digit-vars-used
                                                #'string-lessp)))
         (highest-digit-var-idx  (1+ (or (-elem-index highest-digit-used
                                                      digit-vars)
                                         -1)))
         bindings)
    (when (member '<rest> symbols)
      (!cons (list '<rest>
                   `(seq-drop ,argsym ,highest-digit-var-idx))
             bindings))
    (--map (!cons (list  it
                         `(nth 0 ,argsym))
                  bindings)
           symbol-vars-used)
    (--map (!cons (list  it
                         `(nth ,(-elem-index it digit-vars) ,argsym))
                  bindings)
           digit-vars-used)
    `(lambda (&rest ,argsym)
       (let (,@bindings)
         ,@body))))



(defmacro fn: (&rest body)
  "Return a function defined by (BODY).

Intended for inline use where concision is desired.  If creating a function to
bind as a function value, use `lambda' or `-lambda'.

The definition BODY may use the anaphoric parameters <>, it or <1> to refer to
the first argument, <2> to refer to the second, and so on up to <9>.

Examples:

  (-map (fn: * <> <>) (number-sequence 0 10))
  ;; (0 1 4 9 16 25 36 49 64 81 100)

  (-filter (fn: > it 0)
           '(-5 2 0 0 3 -1 0 4))
  ;; (2 3 4)"
  (declare (debug 'body))
  (let* ((argsym       (make-symbol "ARGS"))
         (symbol-vars  '(<> it))
         (digit-vars   '(<1> <2> <3> <4> <5> <6> <7> <8> <9>))
         (symbols      (eval (backquote (-flatten ',body))))
         (digit-vars-used        (-intersection digit-vars symbols))
         (symbol-vars-used       (-intersection symbol-vars symbols))
         (highest-digit-used     (-last-item (sort digit-vars-used
                                                   #'string-lessp)))
         (highest-digit-var-idx  (1+ (or (-elem-index highest-digit-used
                                                      digit-vars)
                                         -1)))
         bindings)
    (when (member '<rest> symbols)
      (!cons (list '<rest>
                   `(seq-drop ,argsym ,highest-digit-var-idx))
             bindings))
    (--map (!cons (list  it
                         `(nth 0 ,argsym))
                  bindings)
           symbol-vars-used)
    (--map (!cons (list  it
                         `(nth ,(-elem-index it digit-vars) ,argsym))
                  bindings)
           digit-vars-used)
    `(lambda (&rest ,argsym)
       (let (,@bindings)
         (,@body)))))



(provide 'fn)

;;; fn.el ends here
