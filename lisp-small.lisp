;;;; lisp-small.lisp

(in-package #:lisp-small)

(deftype alist-list ()
  `(satisfies alist-list-p))

(serapeum:-> alist-p (list) boolean)
(defun alist-p (xs)
  (every (lambda (x) (or (equal x 'nil)
                    (consp x)))
         xs))

(serapeum:-> alist-list-p (t) boolean)
(defun alist-list-p (xs)
  (or (consp xs)
       (every (lambda (x) (alist-p x)) xs)))

(serapeum:-> init-env () alist-list)
(defun init-env () '(()))

(defstruct closure params body)

(defstruct state result (env '(()) :type alist-list))

(define-condition binding-error (error)
  ((text :initarg :text :reader text)))

(serapeum:-> one-of-p (list t) boolean)
(defun one-of-p (predicates term)
  (some (lambda (p) (funcall p term)) predicates))

(serapeum:-> lookup (symbol alist-list) t)
(defun lookup (id env)
  (if (consp env)
      (let ((looked-up (assoc id (car env))))
        (if looked-up
            (cdr looked-up)
            (lookup id (cdr env))))
      (error 'binding-error :text "No binding found")))

(serapeum:-> update (symbol t alist-list) alist-list)
(defun update (sym val env)
  (if (assoc sym (car env))
      (cons (acons sym val (car env)) (cdr env))
      (if (cdr env)
          (cons (car env) (update sym val (cdr env)))
          (cons (acons sym val (car env)) (cdr env)))))

(serapeum:-> make-function (list t) closure)
(defun make-function (args exp)
  (make-closure :params args :body exp))

(serapeum:-> extend (list list alist-list) alist-list)
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

(serapeum:-> eval2 (t alist-list) t)
(defun eval2 (exp env)
  (labels
      ((evaluate-atom (exp env)
         (cond
           ((symbolp exp)
            (make-state :result (lookup exp env) :env env))
           ((one-of-p (list #'numberp #'standard-char-p) exp)
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
               (let (evaluate (caddr exp) (extend (mapcar #'cadr (cadr exp))
                                                  (mapcar #'car (cadr exp))
                                                  env)))
               (+ (make-state
                   :result
                   (apply #'+ (mapcar #'state-result (evaluate-list (cdr exp) env)))
                   :env
                   env))
               (* (make-state
                   :result
                   (apply #'* (mapcar #'state-result (evaluate-list (cdr exp) env)))
                   :env
                   env))
               (/ (make-state
                   :result
                   (apply #'/ (mapcar #'state-result (evaluate-list (cdr exp) env)))
                   :env
                   env))
               (- (make-state
                   :result
                   (apply #'- (mapcar #'state-result (evaluate-list (cdr exp) env)))
                   :env
                   env))
               (= (make-state
                   :result
                   (apply #'= (mapcar #'state-result (evaluate-list (cdr exp) env)))
                   :env
                   env))
               (if (if (state-result (evaluate (cadr exp) env))
                       (evaluate (caddr exp) env)
                       (evaluate (cadddr exp) env)))
               (lambda (make-state :result (make-function (cadr exp) (caddr exp))
                              :env env))
               (otherwise (invoke (evaluate (car exp) env)
                                  (evaluate-list (cdr exp) env)
                                  env)))))
       
       (invoke (closure args env)
         (let ((args (mapcar #'state-result args))
               (closure (state-result closure)))
           (let ((result
                   (evaluate (closure-body closure)
                             (extend args (closure-params closure) env))))
             (make-state :result (state-result result)
                         :env (rest (state-env result)))))))
    (state-result (evaluate exp env))))
