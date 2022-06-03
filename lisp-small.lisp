;;;; lisp-small.lisp

(in-package #:lisp-small)

(defun init-env () '(()))

(defstruct closure params body env)

(defstruct state result env)

(defun one-of-p (predicates term)
  (some (lambda (p) (funcall p term)) predicates))

(defun lookup (id env)
  (if (consp env)
      (let ((looked-up (assoc id (car env))))
        (if looked-up
            (cdr looked-up)
            (lookup id (cdr env))))
      (error "No binding found")))

(defun update (sym val env)
  (if (assoc sym (car env))
      (cons (acons sym val (car env)) (cdr env))
      (if (cdr env)
          (cons (car env) (update sym val (cdr env)))
          (cons (acons sym val (car env)) (cdr env)))))

(defun make-function (args exp env)
  (make-closure :params args :body exp :env env))

(defun extend (args params env)
  (labels ((aux (args params)
             (cond
               ((consp args)
                (if (consp params)
                    (acons (car params) (car args)
                           (aux (cdr args) (cdr params)))
                    (error "Number of values don't match")))
               ((not args)
                (if (not params)
                    nil
                    (error "Number of values don't match")))
               (t
                (error "Edge case?")))))
    (cons (aux args params) env)))


(defun eval2 (exp env)
  (labels
      ((evaluate-atom (exp env)
         (cond
           ((symbolp exp)
            (make-state :result (lookup exp env) :env env))
           ((one-of-p (list #'numberp #'symbolp #'standard-char-p) exp)
            (make-state :result exp :env env))
           (t
            (error (format nil "Cannot evaluate atom!")))))
       
       (evaluate-list (exprs env)
         (if (consp exprs)
             (cons (evaluate (car exprs) env)
                   (evaluate-list (cdr exprs) env))
             '()))
       
       (evaluate (exp env)
         (if (atom exp)
             (evaluate-atom exp env)
             (case (car exp)
               (quote (make-state :result (cadr exp) :env env))
               (progn (reduce (lambda (acc exp)
                                (evaluate exp (state-env acc)))
                              (cdr exp)
                              :initial-value (make-state :result nil :env env)))
               (set (let ((result (evaluate (caddr exp) env)))
                      (make-state :result (cadr exp)
                                  :env (update (cadr exp) (state-result result)
                                               (state-env result)))))
               (lambda (make-state :result (make-function (cadr exp) (caddr exp) env)
                              :env env))
               (otherwise (invoke (evaluate (car exp) env)
                                  (evaluate-list (cdr exp) env))))))
       
       (invoke (closure args)
         (let ((args (mapcar #'state-result args))
               (closure (state-result closure)))
           (if (typep closure 'closure)
               (let ((result
                       (evaluate (closure-body closure)
                                 (extend args (closure-params closure) (closure-env closure)))))
                 (make-state :result (state-result result)
                             :env (rest (state-env result))))))))
    (state-result (evaluate exp env))))
