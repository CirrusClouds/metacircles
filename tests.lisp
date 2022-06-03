(in-package :lisp-small)

(fiveam:def-suite lisp-small-suite)

(fiveam:in-suite lisp-small-suite)

(defun test-quasi ()
  (fiveam:run! 'lisp-small-suite))

(fiveam:test compilation
  (fiveam:is (equalp
              (eval2 '(lambda (x) x) (init-env))
              (make-closure :params '(x) :body 'x :env (init-env))))
  (fiveam:is (= (eval2 '((lambda (x) x) 3) (init-env))
                3)))
