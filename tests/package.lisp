;;;; package.lisp
;;;; Test package for cupertino.

(defpackage #:cupertino/tests
  (:use #:cl #:rove))

(in-package #:cupertino/tests)

(defmacro finishes (&body body)
  "Evaluate BODY, recording a passing assertion if it returns normally and a
failing one if it performs a non-local exit. Returns BODY's primary value so
it can be used in a binding position (mirrors FiveAM's FINISHES)."
  (let ((done (gensym "DONE")) (val (gensym "VAL")))
    `(let ((,done nil) ,val)
       (unwind-protect
            (setf ,val (progn ,@body) ,done t)
         (if ,done
             (rove:pass "finishes")
             (rove:fail "did not finish")))
       ,val)))
