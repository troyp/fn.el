;;;  -*- lexical-binding: t -*-

(require 'fn)
(require 'ert)

(cl-macrolet ((should-equal
               (expr keyword result)
               (progn
                 (unless (eq keyword :result) (error "expected :result"))
                 `(should (equal ,expr ,result)))))

  (ert-deftest test-fn-docstring-examples ()
    "Test `fn' and `fn:' docstring examples."

    ;; fn docstring
    (should-equal (-map (fn (* <> <>))
                        '(0 1 2 3 4 5 6 7 8 9 10))
                  :result '(0 1 4 9 16 25 36 49 64 81 100))
    (should-equal (-map (fn (/ (-sum <>)
                               (length <>)))
                        '((3.0 4.0 5.0 5.0 10.0)
                          (1.5 2.5 2.0)
                          (1 5)))
                  :result '(5.4 2.0 3))
    (should-equal (-filter (fn (zerop (mod <> 3)))
                           '(1 2 3 4 5 6 7 8 9 10))
                  :result '(3 6 9))
    (should-equal (funcall (fn (-map #'list <rest>)) 1 2 3 4)
                  :result
                  '((1) (2) (3) (4)))

    ;; fn: docstring
    (should-equal (-map (fn: * <> <>) (number-sequence 0 10))
                  :result '(0 1 4 9 16 25 36 49 64 81 100))
    (should-equal (-filter (fn: > <> 0)
                           '(-5 2 0 0 3 -1 0 4))
                  :result '(2 3 4))

    )

  (ert-deftest test-fn-unit-tests/fn ()
    "Test `fn'."

    ;; thunk/constant function
    (should-equal
     (funcall (fn 7))
     :result 7)
    (should-equal
     (funcall (fn 7) 2)
     :result 7)

    ;; single argument
    (should-equal
     (funcall (fn (+ <> 7)) 2)
     :result 9)
    ;; ... with superfluous arguments
    (should-equal
     (funcall (fn (+ <> 7)) 2 3 4)
     :result 9)
    ;; ... in last place
    (should-equal
     (funcall (fn (- 9 <>)) 3)
     :result 6)
    ;; ... with numbered placeholder
    (should-equal
     (funcall (fn (+ <1> 7)) 2)
     :result 9)
    ;; ... single function argument
    (should-equal
     (funcall (fn (funcall <> 16))
              #'sqrt)
     :result 4.0)

    ;; two arguments
    (should-equal
     (funcall (fn (- <1> <2>)) 11 5)
     :result 6)
    ;; ... applied in reverse order
    (should-equal
     (funcall (fn (- <2> <1>)) 11 5)
     :result -6)
    ;; ... with superfluous arguments
    (should-equal
     (funcall (fn (- <1> <2>)) 11 5 8 1)
     :result 6)

    )

  (ert-deftest test-fn-unit-tests/fn: ()
    "Test `fn:'."

    ;; nullary application
    (should-equal
     (funcall (fn: *))
     :result 1)

    ;; single argument
    (should-equal
     (funcall (fn: + <> 7) 2)
     :result 9)
    ;; ... with superfluous arguments
    (should-equal
     (funcall (fn: + <> 7) 2 3 4)
     :result 9)
    ;; ... in last place
    (should-equal
     (funcall (fn: - 9 <>) 3)
     :result 6)
    ;; ... with numbered placeholder
    (should-equal
     (funcall (fn: + <1> 7) 2)
     :result 9)
    ;; ... single function argument
    (should-equal
     (funcall (fn: funcall <> 16)
              #'sqrt)
     :result 4.0)

    ;; two arguments
    (should-equal
     (funcall (fn: - <1> <2>) 11 5)
     :result 6)
    ;; ... applied in reverse order
    (should-equal
     (funcall (fn: - <2> <1>) 11 5)
     :result -6)
    ;; ... with superfluous arguments
    (should-equal
     (funcall (fn: - <1> <2>) 11 5 8 1)
     :result 6)

    )

  (ert-deftest test-fn-variable-capture ()
    "Test `fn'."

    ;; variable capture: fn
    (should-equal
     (let ((ARGS 8))
       (funcall (fn (* 3 <>)) ARGS))
     :result 24)
    ;; variable capture: fn:
    (should-equal
     (let ((ARGS 8))
       (funcall (fn: * 3 <>) ARGS))
     :result 24)

    )

  )

(defun fn---test-all ()
  (interactive)
  (ert-run-tests-batch "^test-fn" ))
