#lang eopl

;; interpreter for the PROC language, using the procedural
;; representation of procedures.



(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      
      (const-exp (num) (num-val num))
      
      
      (var-exp (var) (apply-env env var))
      
     
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
     
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (value-of rand env)))
                  (apply-procedure proc arg)))
      
      ;;----------------------------------------------------
      ; INSERT YOUR CODE HERE
      ; Write the required expressions starting from here
      (stack-push-multi-exp (stack-exp exps)
        (let ((stk (expval->stack (value-of stack-exp env))))
          (let loop ((args exps)
                     (current stk))
            (if (null? args)
                (stack-val current)
                (let ((n (expval->num (value-of (car args) env))))
                  (loop (cdr args) (cons n current)))))))

      (stack-pop-multi-exp (stack-exp num-exp)
        (let* ((stk (expval->stack (value-of stack-exp env)))
               (n   (expval->num   (value-of num-exp env))))
          (define (pop-n s k)
            (cond
              ((null? s)
               (begin (display "pop-multi beyond stack size!\n")
                      (list)))
              ((<= k 0) s)
              (else (pop-n (cdr s) (sub1 k)))))
          (stack-val (pop-n stk n))))

      (stack-merge-exp (stack1-exp stack2-exp)
        (let ((s1 (expval->stack (value-of stack1-exp env)))
              (s2 (expval->stack (value-of stack2-exp env))))
          (define (merge a b)
            (if (null? b)
                a
                (merge (cons (car b) a) (cdr b))))
          (stack-val (merge s1 s2))))

      ;;-------------------------------------------------

      (stack-exp () (stack-val (list)))

      (stack-push-exp (stack-exp1 exp1)
                      (let ((lst (expval->stack (value-of stack-exp1 env)))
                             (n   (expval->num   (value-of exp1 env))))
                        (stack-val (cons n lst))))

      (stack-pop-exp (stk-exp)
                     (let ((stk (expval->stack (value-of stk-exp env))))
                       (if (null? stk)
                           (begin
                             (display "pop from empty stack!\n")
                             (stack-val stk))
                           (stack-val (cdr stk)))))

      (stack-peek-exp (stk-exp)
                     (let ((stk (expval->stack (value-of stk-exp env))))
                       (if (null? stk)
                           (begin
                             (display "peek into empty stack!\n")
                             (num-val 2813))
                           (num-val (car stk)))))
      
      )))

;;-----------------------------------------
; INSERT YOUR CODE HERE
; you may use this area to define helper functions
;;-----------------------------------------


;;-----------------------------------------

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))
