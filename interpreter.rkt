#lang racket

;;
;;  interpreter.rkt
;;  Racket Interpreter
;;
;;  Created by Callen Rain and Justin Cosentino on 5/15/13.
;;

(require racket/mpair)   ; for mutable pairs (you will need this)
(require racket/pretty)  ; for pretty printing (you may not need this)
(require racket/trace)   ; for tracing (you may not need this)

;; -------------------------------------------------------------------
;; Part 1: Environments
;; -------------------------------------------------------------------

;; binding abstraction
(define make-binding mcons)
(define binding-variable mcar)
(define binding-value mcdr)
(define set-binding-value! set-mcdr!)

;; frame abstraction
(define make-frame
  (lambda (vars vals)
    (cond ((and (empty-frame? vars) (empty-frame? vals)) null)
          ((null? vars) (error "make-frame::too many values"))
          ((null? vals) (error "make-frame::too many variables"))
          (else
           (adjoin-binding (make-binding (car vars) (car vals)) (make-frame (cdr vars) (cdr vals)))))))
(define empty-frame? null?)
(define first-binding car)
(define rest-of-bindings cdr)
(define adjoin-binding cons)

(define binding-in-frame
  (lambda (var frame)
    (cond ((empty-frame? frame) #f)
          ((equal? (binding-variable (first-binding frame)) var) (first-binding frame))
          (else (binding-in-frame var (rest-of-bindings frame))))))

;; environment abstraction
(define empty-env null)

(define empty-env? null?)

(define first-frame mcar)

(define rest-of-frames mcdr)

(define set-first-frame!
  (lambda (env new-frame)
    (set-mcar! env new-frame)))

(define adjoin-frame mcons)
(define extend-env
  (lambda (vars vals env)
    (adjoin-frame (make-frame vars vals) env)))
(define binding-in-env
  (lambda (var env)
    (if (empty-env? env) #f
        (let ((result (binding-in-frame var (first-frame env))))
          (if result result (binding-in-env var (rest-of-frames env)))))))

;; variable implementation
(define lookup-variable
  (lambda (var env)
    (let ((result (binding-in-env var env)))
      (if result (binding-value result) (error "lookup-variable::unbound variable" var)))))


;; -------------------------------------------------------------------
;; Allow external unit testing
;; -------------------------------------------------------------------

(provide 
 make-binding binding-variable binding-value set-binding-value!
 make-frame empty-frame? first-binding rest-of-bindings
 adjoin-binding binding-in-frame   
 empty-env empty-env? first-frame rest-of-frames set-first-frame! 
 adjoin-frame extend-env binding-in-env
 lookup-variable)



;; -------------------------------------------------------------------
;; Part 2: Evaluation and Environment Modification
;; -------------------------------------------------------------------


;; Continuing the "environment" abstraction
;;-------------------------------------------------------------------

;; The initial definition of setup-env.
;; We will change this later.
;(define setup-env
;  (lambda ()
;    (extend-env
;     '(null) '(())        ;bind the symbol null to '()
;     empty-env)))


;; Implementing "eval"
;;-------------------------------------------------------------------

;; This is a partial implementation of i-eval.
;; i-eval evaluates expressions within an environment.

(define i-eval
  (lambda (exp env)
    (cond
      ((boolean? exp) exp)
      ((number? exp) exp)
      ((string? exp) exp)
      ((char? exp) exp)
      ((quoted? exp) (text-of-quotation exp))
      ((quasiquoted? exp) (eval-quasiquote exp env))
      ((unquote? exp) (error "unquote: not in quasiquote in: " exp))
      ((definition? exp) (eval-definition exp env))
      ((variable? exp) (lookup-variable exp env))
      ((assignment? exp) (eval-assignment exp env))
      ((begin? exp) (eval-begin exp env))
      ((if? exp) (eval-if exp env))
      ((cond? exp) (eval-cond exp env))
      ((trace? exp) (eval-trace exp env))
      ((untrace? exp) (eval-untrace exp env))
      ((lambda? exp) (make-closure exp env))
      ((let? exp) (eval-let exp env))
      ((let*? exp) (eval-let* exp env))
      ((map? exp) (eval-map exp env))
      ((apply? exp) (eval-apply exp env))
      ((and? exp) (eval-and exp env))
      ((or? exp) (eval-or exp env))
      ((include? exp) (eval-include exp env))
      ((application? exp) (eval-application exp env))
      (else (error "i-eval::unknown expression type" exp)))))


(define read-eval-print-loop
  (lambda ()
    (display "INTERPRETER> ")
    (let ((user-input (read)))
      (cond ((equal? user-input 'exit) (display "INTERPRETER done."))
            ((equal? user-input 'exit!) (set! global-env (setup-env)) (display "INTERPRETER done."))
            (else
             (i-print (i-eval user-input global-env))
             (read-eval-print-loop))))))


(define repl read-eval-print-loop)


;; Implementing "quote"
;;-------------------------------------------------------------------

(define quoted?
  (lambda (exp)
    (tagged-list-length-n? exp 'quote 2)))

(define text-of-quotation
  (lambda (quoted-exp)
    (cadr quoted-exp)))



;; An important helper function: tagged-list?
;;-------------------------------------------------------------------

(define tagged-list?
  (lambda (exp tag)
    (and  (list? exp) (not (null? exp)) (equal? (car exp) tag) )))



;; Implementing "define"
;;-------------------------------------------------------------------

(define definition?
  (lambda (exp)
    (tagged-list-length-n? exp 'define 3)))

(define definition-variable
  (lambda (define-exp)
    (cadr define-exp)))

(define definition-value
  (lambda (define-exp)
    (caddr define-exp)))

(define define-variable!
  (lambda (var val env)
    (let ((frame (first-frame env)))
      (cond ((binding-in-frame var frame) (error "duplicate definition for identifier ")) 
            (else (set-first-frame! env (adjoin-binding (make-binding var val) frame)))))
    var))

(define eval-definition
  (lambda (exp env)
    (define-variable! (definition-variable exp) (i-eval (definition-value exp) env) env )))


;; Completing the implementation of variables
;;-------------------------------------------------------------------

(define variable?
  (lambda (exp)
    (symbol? exp)))


;; Implementing simple syntax checker
;;-------------------------------------------------------------------

(define tagged-list-length-n?
  (lambda (exp tag n)
    (and  (list? exp) (not (null? exp)) (equal? (car exp) tag) (equal? (length exp) n ))))



(define tagged-list-min-length-n?
  (lambda (exp tag n)
    (and  (list? exp) (not (null? exp)) (equal? (car exp) tag) (>= (length exp) n ))))

;; Once you have implemented the above two functions, go back
;; and rewrite the 'defintion?' and 'quoted?' functions to use these.


;; Implementing set!
;;-------------------------------------------------------------------

(define assignment?
  (lambda (exp)
    (tagged-list-length-n? exp 'set! 3)))

(define eval-assignment
  (lambda (exp env)
    (set-variable-value! (assignment-variable exp) (i-eval (assignment-value exp) env) env )))

(define assignment-variable
  (lambda (exp)
    (cadr exp)))

(define assignment-value
  (lambda (exp)
    (caddr exp)))

(define set-variable-value!
  (lambda (var val env)
    (let ((binding (binding-in-env var env)))
      (cond (binding (set-binding-value! binding val) )
            (else (error "set! cannot set undefined identifier: "))))
    var))


;; Extension: Quasiquotmo
;;-------------------------------------------------------------------

(define text-of-quasiquotation
  (lambda (exp)
    (cadr exp)))

(define text-of-unquote
  (lambda (exp)
    (cadr exp)))

(define unquote?
  (lambda (exp)
    (tagged-list-length-n? exp 'unquote 2)))

(define quasiquoted?
  (lambda (exp)
    (tagged-list-length-n? exp 'quasiquote 2)))

(define eval-quasiquote
  (lambda (expr env)
    (define eval-quasiquote-h
      (lambda (exp)
        (cond ((null? exp) null)
              ((unquote? exp) (i-eval (text-of-unquote exp) env))
              ((unquote? (car exp)) (cons (i-eval (text-of-unquote (car exp)) env) (eval-quasiquote-h (cdr exp))))
              (else 
               (cons (car exp) (eval-quasiquote-h (cdr exp)))))))
    (eval-quasiquote-h (text-of-quasiquotation expr))))


;; -------------------------------------------------------------------
;; Allow external unit testing
;; -------------------------------------------------------------------

(provide 
 setup-env global-env
 i-eval
 read-eval-print-loop repl
 quoted? text-of-quotation
 tagged-list?
 definition? definition-value definition-variable
 variable?
 tagged-list-length-n? tagged-list-min-length-n?
 assignment? eval-assignment assignment-variable assignment-value
 set-variable-value!
 )



;; -------------------------------------------------------------------
;; Part 3: Primitive Procedure Application
;; -------------------------------------------------------------------

;; Implementing "i-apply"
;;-------------------------------------------------------------------

(define application?
  (lambda (expr)
    (cond ((null? expr) #f)
          (else (list? expr)))))

(define operator car)

(define operands cdr)

(define eval-operands
  (lambda (operands env)
    (cond ((null? operands) null)
          (else
           (cons (i-eval (car operands) env) (eval-operands (cdr operands) env))))))

(define i-apply 
  (lambda (proc vals)
    (cond
      ((primitive-procedure? proc) (apply-primitive-procedure proc vals))
      ((closure? proc) (apply-closure proc vals))
      (else (error "i-apply::procedure type unknown:" proc)))))


;; Primitives
;;-------------------------------------------------------------------

(define make-primitive
  (lambda (name proc)
    (list 'primitive name proc)))

(define primitive-procedure?
  (lambda (expr)
    (tagged-list-length-n? expr 'primitive 3)))

(define traced-procedure?
  (lambda (expr)
    (tagged-list-length-n? expr 'trace 4)))

(define primitive-name cadr)

(define primitive-implementation caddr)

(define primitive-procedures
  (list 
   (make-primitive 'car car)
   (make-primitive 'cdr cdr)
   (make-primitive 'cons cons)
   (make-primitive 'mcons mcons)
   (make-primitive 'mcar mcar)
   (make-primitive 'mcdr mcdr)
   (make-primitive 'null? null?)
   (make-primitive 'list? list?)
   (make-primitive 'list list)
   (make-primitive 'append append)
   (make-primitive 'length length)
   (make-primitive 'set-mcar! set-mcar!)
   (make-primitive 'set-mcdr! set-mcdr!)
   (make-primitive '+ +)
   (make-primitive '- -)
   (make-primitive '* *)
   (make-primitive '/ /)
   (make-primitive '> >)
   (make-primitive '< <)
   (make-primitive '= =)
   (make-primitive '>= >=)
   (make-primitive 'number? number?)
   (make-primitive 'boolean? boolean?)
   (make-primitive 'false? false?)
   (make-primitive 'symbol? symbol?)
   (make-primitive 'equal? equal?)
   (make-primitive 'eq? eq?)
   (make-primitive 'display display)
   (make-primitive 'newline newline)
   (make-primitive 'not not)
   (make-primitive 'void void)
   (make-primitive 'void? void?)
   (make-primitive 'string? string?)
   (make-primitive 'char? char?)
   (make-primitive 'pretty-print pretty-print)
   (make-primitive 'add1 add1)
   (make-primitive 'sub1 sub1)
   (make-primitive 'cadr cadr)
   (make-primitive 'cddadr cddadr)
   (make-primitive 'cdadr cdadr)
   (make-primitive 'cddr cddr)
   (make-primitive 'cdar cdar)
   (make-primitive 'caar caar)
   (make-primitive 'caadr caadr)
   (make-primitive 'cadadr cadadr)
   (make-primitive 'caddr caddr)
   (make-primitive 'cadar cadar)
   (make-primitive 'cadddr cadddr)
   (make-primitive 'list-ref list-ref)
   (make-primitive 'pair? pair?)
   (make-primitive 'string-ref string-ref)
   (make-primitive 'string-length string-length)
   (make-primitive 'string-append string-append)
   (make-primitive 'read-line read-line)
   (make-primitive 'eof-object? eof-object?)
   (make-primitive 'open-input-file open-input-file)
   (make-primitive 'open-input-string open-input-string)
   (make-primitive 'read read)
   (make-primitive 'primitive? primitive?)
   (make-primitive 'error error)))


(define simple-map
  (lambda (fn lst)
    (cond ((null? lst) null)
          (else (cons (fn (car lst))
                      (simple-map fn (cdr lst)))))))

(define apply-primitive-procedure
  (lambda (proc vals)
    (apply (primitive-implementation proc) vals)))
           

(define display-vals
  (lambda (vals)
    (cond ((null? vals) (printf ""))
          (else (printf " ~a" (car vals)) (display-vals (cdr vals))))))

(define eval-application 
  (lambda (exp env)
    (cond ((traced-procedure? (i-eval (operator exp) env)) 
           (let ((trace-result (i-apply (cdr (i-eval (operator exp) env)) (eval-operands (operands exp) env))))
           (printf "|~a \n|~a \n" exp trace-result)
           trace-result))
    (else  
     (i-apply (i-eval (operator exp) env) (eval-operands (operands exp) env))))))

(define setup-env
  (lambda ()
    (let ((initial-env
           (extend-env
            (simple-map primitive-name primitive-procedures)
            primitive-procedures
            empty-env)))
      (define-variable! 'null '() initial-env)
      initial-env)))

(define global-env (setup-env))



;; -------------------------------------------------------------------
;; Part 4: Special Forms: begin, if, cond
;; -------------------------------------------------------------------


;; Implementing "begin"
;;-------------------------------------------------------------------

(define begin?
  (lambda (exp) 
    (tagged-list-min-length-n? exp 'begin 1)))

(define begin-expressions cdr)

(define eval-begin
  (lambda (exp env) 
    (define eval-begin-h
      (lambda (expr)
        (cond ((null? (cdr expr)) (i-eval (car expr) env))
              (else
               (begin (i-eval (car expr) env) (eval-begin-h (cdr expr))))))) 
    (cond ((null? (begin-expressions exp)) (void))
          (else
           (eval-begin-h (begin-expressions exp))))))



;; Implementing "if"
;;-------------------------------------------------------------------

(define if?
  (lambda (exp) 
    (or (tagged-list-length-n? exp 'if 3) (tagged-list-length-n? exp 'if 4))))


(define test-expression cadr)

(define then-expression caddr)

(define else-expression cadddr)

(define eval-if
  (lambda (exp env)
    (cond ((equal? (length exp) 4)
           (if (i-eval (test-expression exp) env) (i-eval (then-expression exp) env) (i-eval (else-expression exp) env)))
          (else (if (i-eval (test-expression exp) env) (i-eval (then-expression exp) env) (void))))))


;; Implementing "cond"
;;-------------------------------------------------------------------

(define cond?
  (lambda (exp) 
    (tagged-list-min-length-n? exp 'cond 1)))

(define first-cond-exp cadr)

(define rest-of-cond-exps
  (lambda (exp)
    (cons 'cond (cddr exp))))

(define eval-cond
  (lambda (exp env)
    (cond ((null? (cdr exp)) (void))
          ((and (equal? (first-cond-exp exp) 'else) (not (null? (rest-of-cond-exps exp)))) (error "incorrect placement of else"))
          ((equal? (car (first-cond-exp exp)) 'else) (eval-begin (first-cond-exp exp) env))
          ((null? (cdr (first-cond-exp exp))) (i-eval (car (first-cond-exp exp)) env))
          ((i-eval (car (first-cond-exp exp)) env) (eval-begin (first-cond-exp exp) env))
          (else
           (eval-cond (rest-of-cond-exps exp) env)))))

;; implementing trace
;; ------------------------------------------------------

(define trace?
  (lambda (exp)
    (tagged-list-min-length-n? exp 'trace 1)))

(define untrace?
  (lambda (exp)
    (tagged-list-min-length-n? exp 'untrace 1)))

(define function-list cdr)
(define first-function cadr)
(define rest-functions cddr)

(define traced? 
  (lambda (binding)
    (equal? (car (binding-value binding)) 'trace)))

(define eval-trace
  (lambda (exp env)
    (cond ((null? (function-list exp)) (void))
          (else 
           (let ((binding (binding-in-env (first-function exp) env)))
             (cond ((not binding) (display "trace::cannot find function") (eval-trace (cons 'trace (rest-functions exp)) env))
                   ((traced? binding) (eval-trace (cons 'trace (rest-functions exp)) env) (printf "(~a)" (binding-variable binding)))
                   (else 
                    (set-binding-value! binding (cons 'trace (binding-value binding))) (eval-trace (cons 'trace (rest-functions exp)) env)
                    (printf "(~a)" (binding-variable binding)))))))))

(define eval-untrace
  (lambda (exp env)
    (cond ((null? (function-list exp)) (void))
          (else 
           (let ((binding (binding-in-env (first-function exp) env)))
             (cond ((not binding) (display "trace::cannot find function") (eval-untrace (cons 'untrace (rest-functions exp)) env))
                   ((traced? binding) (set-binding-value! binding (cdr (binding-value binding))) 
                                      (eval-untrace (cons 'untrace (rest-functions exp)) env)
                                      (printf "(~a)" (binding-variable binding)))
                   (else 
                    (eval-untrace (cons 'untrace (rest-functions exp)) env)
                    (printf "(~a)" (binding-variable binding)))))))))



;; -------------------------------------------------------------------
;; Part 5: lambda and let
;; -------------------------------------------------------------------


;; Implementing "lambda" and closures
;;-------------------------------------------------------------------

(define lambda?
  (lambda (exp)
    (tagged-list-min-length-n? exp 'lambda 3)))

(define make-closure
  (lambda (lambda-exp env)
    (list 'closure lambda-exp env)))

(define closure?
  (lambda (exp)
    (tagged-list-length-n? exp 'closure 3)))

(define procedure-parameters cadadr)

(define procedure-body cddadr)

(define procedure-env caddr)

(define apply-closure
  (lambda (closure vals)
    (let ((env (extend-env (procedure-parameters closure) vals (procedure-env closure))))
      (eval-begin (cons 'begin (procedure-body closure)) env))))


;; Helper: i-print
;;-------------------------------------------------------------------

(define i-print
  (lambda (exp)
    (cond 
      ((void? exp) (newline))
      ((closure? exp )(display (cadr exp)) (newline))
      ((primitive? exp) (display (list 'primitive (primitive-name exp))) (newline))
      (else (pretty-print exp)))))


;; Implemeting "let"
;;-------------------------------------------------------------------

(define let?
  (lambda (exp)
    (tagged-list-min-length-n? exp 'let 3)))

(define let->lambda
  (lambda (exp)
    (define get-vars
      (lambda (lst)
        (cond ((null? lst) null)
              (else
               (cons (caar lst) (get-vars (cdr lst)))))))
    (define get-vals
      (lambda (lst)
        (cond ((null? lst) null)
              (else
               (cons (cadar lst) (get-vals (cdr lst)))))))
    (define get-exps
      (lambda (lst)
        (cddr lst)))
    (cons (cons 'lambda (cons (get-vars (cadr exp)) (get-exps exp))) (get-vals (cadr exp)))))



(define eval-let
  (lambda (exp env)
    (i-eval (let->lambda exp) env)))


;; Implemeting "let*"
;;-------------------------------------------------------------------

(define let*?
  (lambda (exp)
    (tagged-list-min-length-n? exp 'let* 3)))


(define let*->let
  (lambda (exp)
    (cond ((null? (cdadr exp)) (cons 'let (cons (cadr exp) (cddr exp))))
          (else
           (list 'let (list (caadr exp)) (let*->let (cons 'let* (cons (cdadr exp) (cddr exp)))))))))

(define eval-let*
  (lambda (exp env)
    (i-eval (let*->let exp) env)))



;; -------------------------------------------------------------------
;; Part 6: Meta-circularity
;; -------------------------------------------------------------------

;; Implementing "map"
;;-------------------------------------------------------------------

(define map?
  (lambda (exp)
    (tagged-list-length-n? exp 'map 3)))

(define map-procedure
  (lambda (exp)
    (cadr exp)))

(define map-list
  (lambda (exp)
    (caddr exp)))

(define eval-map
  (lambda (exp env)
    (eval-map-helper (i-eval (map-procedure exp) env)
                     (i-eval (map-list exp) env))))


(define eval-map-helper
  (lambda (proc lst)
    (cond ((null? lst) null)
          (else
          (cons (i-apply proc (list (car lst))) (eval-map-helper proc (cdr lst)))))))


;; Implementing "apply"
;;-------------------------------------------------------------------

(define apply?
  (lambda (exp)
    (tagged-list-length-n? exp 'apply 3)))

(define apply-procedure
  (lambda (exp)
    (cadr exp)))

(define apply-arguments
  (lambda (exp)
    (caddr exp)))

(define eval-apply
   (lambda (exp env)
     (i-apply (i-eval (apply-procedure exp) env) (i-eval (apply-arguments exp) env))))


;; Implementing "eval"  
;;-------------------------------------------------------------------

(define eval?
  (lambda (exp)
    (tagged-list? exp 'eval)))

(define eval-eval
  (lambda (exp env)
    '...))


;; Implementing "and" and "or" 
;;-------------------------------------------------------------------

(define and?
  (lambda (exp)
    (tagged-list? exp 'and)))

(define or?
  (lambda (exp)
    (tagged-list? exp 'or)))

(define eval-and
  (lambda (exp env)
    (define eval-and-helper
       (lambda (exp)
       (let ((result (i-eval (car exp) env)))
         (cond ((or (null? (cdr exp)) (false? result)) result)
               (else
                 (eval-and-helper (cdr exp)))))))
    (cond ((null? (cdr exp)) #t)
          (else 
              (eval-and-helper (cdr exp))))))

    
(define eval-or
  (lambda (exp env)
    (define eval-or-helper
       (lambda (exp)
       (let ((result (i-eval (car exp) env)))
         (cond ((or (null? (cdr exp)) result) result)
               (else
                 (eval-or-helper (cdr exp)))))))
   (cond ((null? (cdr exp)) #f)
          (else 
               (eval-or-helper (cdr exp))))))

;; "include": used for Meta-Circularity.
;;-------------------------------------------------------------------

(define include?
  (lambda (exp)
    (tagged-list? exp 'include)))

(define include-file
  (lambda (exp)
    (cadr exp)))

(define eval-include
  (lambda (exp env)
    (let ((input (open-input-string (file-to-string (include-file exp)))))
      (define include-helper
        (lambda ()          
          (let ((user-input (read input)))
            (cond ((eof-object? user-input) (void))
                  ((or (tagged-list? user-input 'require)
                       (tagged-list? user-input 'trace)
                       (tagged-list? user-input 'provide))
                   (include-helper))
                  (else
                   (let ((result (i-eval user-input env)))
                     (i-print result) (newline)
                     (include-helper)))))))
      (include-helper))))

(define file-to-string
  (lambda (filename)
    (let ((input (open-input-file filename))
          (contents ""))
      (define helper
        (lambda ()
          (let ((line (read-line input)))
            (cond ((eof-object? line) contents)
                  ((= (string-length line) 0) (helper))
                  ((eq? (string-ref line 0) #\#) (helper))
                  (else (set! contents (string-append contents "\n" line))
                        (helper))))))
      (helper))))
