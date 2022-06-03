(in-package :lisp-small)

(fiveam:def-suite lisp-small-suite)

(fiveam:in-suite lisp-small-suite)

(defun test-quasi ()
  (fiveam:run! 'lisp-small-suite))

(fiveam:test compilation
  (fiveam:is (equalp
              (eval2 '(lambda (x) x) (init-env))
              (make-closure :params '(x) :body 'x :env (init-env))))
  (fiveam:is (= (eval2 '((lambda (x) x) 3) (init-env)) 3))
  (fiveam:is (= (eval2 '(progn
                         (set x 5)
                         ((lambda (x) (set x 4)) 3)
                         x) (init-env))
                5))
  (fiveam:is (= (eval2 '(progn
                         (set x 5)
                         (set x 4)
                         x) (init-env))
                4))
  (fiveam:signals binding-error
    (eval2 '(progn
             ((lambda (x) x) 3)
             x) (init-env)))
  )
