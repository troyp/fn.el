;;;  -*- lexical-binding: t -*-

(require 'fn)
(require 'ert)

(cl-flet
    ((should-equal (a b)
                   (should (equal a b)))
     (should-eql (a b)
                 (should (eql a b)))
     (should= (a b)
              (should (= a b))))

  (ert-deftest test-fn ()
    "Test `fn'."

    (should-equal (-map (fn (* <> <>))
                        '(0 1 2 3 4 5 6 7 8 9 10))
                  ;; result:
                  '(0 1 4 9 16 25 36 49 64 81 100))

    (should-equal (-map (fn (/ (-sum <>)
                               (length <>)))
                        '((3.0 4.0 5.0 5.0 10.0)
                          (1.0 2.0 2.0)
                          (1 5)))
                  ;; result:
                  '(5.4 1.6666666666666667 3))

    (should-equal (-filter (fn (zerop (mod <> 3)))
                           '(1 2 3 4 5 6 7 8 9 10))
                  ;; result:
                  '(3 6 9))

    (should=
     (funcall (fn 7))
     ;; result:
     7)

    (should=
     (funcall (fn 7) 2)
     ;; result:
     7)

    ;; variable capture
    (should=
     (let ((ARGS 8))
       (funcall (fn (* 3 <>)) ARGS))
     ;; result:
     24)

    )

  (ert-deftest test-fn-bindl ()
    "Test `fn-bindl'."

    (should-equal
     (mapcar (fn-bindl '- 10)
             '(1 2 3 4 5 6 7 8 9 10))
     ;; result:
     '(9 8 7 6 5 4 3 2 1 0))

    )

  (ert-deftest test-fn-bindr ()
    "Test `fn-bindr'."

    (should-equal
     (mapcar (fn-bindl '- 10)
             '(1 2 3 4 5 6 7 8 9 10))
     ;; result:
     '(9 8 7 6 5 4 3 2 1 0))

    )

  (ert-deftest test-fn-fork ()
    "Test `fork'."

    (should-equal
     (fn-call (fn-fork 'list '+ '*) 2 3 5)
     ;; result:
     '(10 30))

    (should-eql
     (fn-call (fn-fork '/ '-sum 'length)
              '(2.0 0 2.0 3.0 5.0 1.0 8.0))
     ;; result:
     3.0)

    (should-equal
     (-map (fn-fork '+
                    (fn (expt <> 5))
                    (fn <>)
                    (fn 1))
           '(1 2 10))
     ;; result:
     '(3 35 100011))

    )

  )

(defun fn---test-all ()
  (interactive)
  (ert-run-tests-batch "^test-fn" ))
