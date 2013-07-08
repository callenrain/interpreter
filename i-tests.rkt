#lang racket

(require rackunit "interpreter.rkt")
(require racket/mpair)

(define-syntax-rule (check-fail? expr)
  (check-exn exn:fail? (lambda () expr)))

;; BINDING TESTS
(let ((sample (make-binding 'today 'monday)))
  (check-equal? (binding-variable sample) 'today)
  (check-equal? (binding-value sample) 'monday)
  (set-binding-value! sample 'tuesday)
  (check-equal? (binding-value sample) 'tuesday))
 
(let ((sample2 (make-binding 'a-pair (cons 1 2))))
  (check-equal? (binding-variable sample2) 'a-pair)
  (check-equal? (binding-value sample2) '(1 . 2))
  (set-binding-value! sample2 'pair)
  (check-equal? (binding-value sample2) 'pair))

(let ((sample3 (make-binding 'a-list (list 1 2))))
  (check-equal? (binding-variable sample3) 'a-list)
  (check-equal? (binding-value sample3) '(1 2)))
   
;; FRAME TESTS
(let ((frame (make-frame '(a b c) '(6 7 8))))
  (check-equal? (first-binding frame) (mcons 'a 6))
  (check-equal? (rest-of-bindings frame) (list (mcons 'b 7) (mcons 'c 8)))
  (check-equal? (empty-frame? frame) #f)
  (check-equal? (binding-in-frame 'a frame) (mcons 'a 6))
  (check-equal? (binding-in-frame 'c frame) (mcons 'c 8))
  (check-equal? (binding-in-frame 'x frame) #f)
  
  (set! frame (adjoin-binding (make-binding 'd 9) frame) )
  (check-equal? (binding-in-frame 'd frame) (mcons 'd 9))
  
  (check-fail? (make-frame '(x y z) '(#t #f)))
  (check-fail? (make-frame '(today tomorrow) '(tue wed thu)))
  (check-fail? (make-frame '() '(tue wed thu))))

; ENVIRONMENT TESTS
(let* ((env1 (extend-env '(a b c) '(1 2 3) empty-env))
       (env2 (extend-env '(a c d e) '(red blue green yellow) env1))
       (env3 (extend-env '(a f) '(#t #f) env2)))
  (check-equal? (binding-in-env 'c env3) (mcons 'c 'blue))
  (check-equal? (lookup-variable 'c env3) 'blue)
  (check-fail? (lookup-variable 'g env3))
  (check-equal? (empty-env? env1) #f)
  (check-equal? (first-frame env3) (list (mcons 'a #t) (mcons 'f #f)))
  (check-equal? (mcar (rest-of-frames env2)) (make-frame '(a b c) '(1 2 3)))
  (set-first-frame! env3 (make-frame '(a f) '(#f #f)))
  (check-equal? (first-frame env3) (list (mcons 'a #f) (mcons 'f #f))))


; WRITE YOUR OWN TESTS
(let ((frame (make-frame '(a b c d e f) '(6 7 8 9 10 11))))
  (check-equal? (first-binding frame) (mcons 'a 6))
  (check-equal? (rest-of-bindings frame) (list (mcons 'b 7) (mcons 'c 8) (mcons 'd 9) (mcons 'e 10) (mcons 'f 11)))
  (check-equal? (empty-frame? frame) #f)
  (check-equal? (binding-in-frame 'e frame) (mcons 'e 10))
  (check-equal? (binding-in-frame 'd frame) (mcons 'd 9))
  (check-equal? (binding-in-frame 'z frame) #f)
  (check-fail? (make-frame '(a b c d) '(#t #f)))
  (check-fail? (make-frame '(today) '(tue wed thu))))

;; additional tests were added to the BINDING, FRAME, and ENV tests above

;; -------------------------------------------------------------------
;; Part 2: Evaluation and Environment Modification
;; -------------------------------------------------------------------
;;
;;
;; A set of tests for i-eval 
;; -------------------------------------------------------------------
;;
;;
;; (check-equal? (i-eval (quote expression-to-test) global-env) expected-value)
;; (check-equal? (i-eval (quote (define x 5)) global-env) 'x)
;;
;; (i-equal? (define x 5) 'x)
;; 

(define-syntax-rule (i-equal? expr expected)
  (check-equal? (i-eval (quote expr) global-env) expected))

;; Similarly, you can use 'i-fail?':

(define-syntax-rule (i-fail? expr)
  (check-fail? (i-eval (quote expr) global-env)))


(i-equal? 5 5)
(i-equal? 5 5)
(i-equal? -3.14 -3.14)
(i-equal? #f #f)
(i-equal? "a short string" "a short string")

(i-equal? (quote this-is-quoted) 'this-is-quoted)
(i-equal? 'this-is-also-quoted 'this-is-also-quoted)
(i-equal? '(4 5 6) '(4 5 6))
(i-equal? '(3.14 #t hello) '(3.14 #t hello))


(i-equal? (define pi 3.1415) 'pi)
(i-equal? pi 3.1415)
(i-equal? (define a 18) 'a)
(i-equal? a 18)
(i-fail? (define a 32)) ; cannot redefine identifier: a
(i-equal? a 18)
(i-equal? (define b a) 'b)
(i-equal? b 18)
(i-equal? (set! b 54) 'b)
(i-equal? a 18)
(i-equal? b 54)
(i-fail? (set! c 10)) ;cannot set undefined identifier: c

(check-equal? (variable? 'a) #t)
(check-equal? (variable? #f) #f)
(i-fail? (define pi 5141.3))
(i-equal? (define true #t) 'true)
(i-equal? true #t)
(i-equal? (set! true #f) 'true)
(i-equal? true #f)
(i-fail? (set! false #f))

;; exit and exit have been manually tested

(i-fail? (set! x2 5))
(i-equal? (define x2 5) 'x2)
(i-equal? x2 5)
(i-equal? (set! x2 8) 'x2)
(i-equal? x2 8)
(i-equal? (set! x2 'hello) 'x2)
(i-equal? x2 'hello)
(i-equal? (define y2 x2) 'y2)
(i-equal? (set! x2 "testing...1...2...3...") 'x2)
(i-equal? y2 'hello)
(i-equal? x2 '"testing...1...2...3...")


;; -------------------------------------------------------------------
;; Part 3
;; -------------------------------------------------------------------

;; manually tested application?, operator, and operands

(i-equal? (define x3 8) 'x3)
(i-equal? (define y3 (+ x3 1)) 'y3)
(i-equal? (define list3 (cons x3 (cons '(x3 y3 z3) (cons y3 null)))) 'list3)
(i-equal? list3 '(8 (x3 y3 z3) 9))
(i-equal? (define z3 (+ (car list3) (car (cdr (cdr list3))))) 'z3)
(i-equal? z3 17)
(i-equal? (= (+ x3 y3) z3) #t)
(i-equal? (define w3 *) 'w3)
(i-equal? (w3 x3 y3) 72)

(i-equal? (define x31 'a) 'x31)
(i-fail? (define y31 (+ 'b 1)))
(i-equal? (set! x31 (+ 1 3)) 'x31)
(i-equal? (define y31 (* 3  (+ 4 5))) 'y31)
(i-equal? (define z31 (+ y31 x31 (* (/ 100 10) 5))) 'z31)
(i-equal? z31 81)
(i-equal? (= (* 2 x31) (+ x31 x31)) #t)
(i-equal? (set! w3 cons) 'w3)
(i-equal? (w3 x3 y3) '(8 . 9))

(i-equal? (define x (cons 1 (cons 2 null))) 'x)
(i-equal? x '(1 2))
(i-equal? (null? x) #f)
(i-equal? (null? (cdr (cdr x))) #t)
(i-equal? (= (car x) (- (car (cdr x)) 1)) #t)
(i-equal? (define y (cons (car x) x)) 'y)
(i-equal? y '(1 1 2))
(i-equal? (set! x 5) 'x)
(i-equal? (set! y (+ x 5)) 'y)
(i-equal? y 10)
(i-equal? (* x y) 50)

;; -------------------------------------------------------------------
;; Extension A
;; -------------------------------------------------------------------

(i-equal? (define aA 3) 'aA)
(i-equal? (define bA 4) 'bA)
(i-equal? '(+ aA bA) '(+ aA bA))
(i-equal? `(+ aA bA)  '(+ aA bA))
(i-equal? `(+ ,aA ,bA) '(+ 3 4))
(i-equal? `(+ `(+ ,aA ,bA) ,bA) '(+ `(+ ,aA ,bA) 4))
(i-equal? `,`,`,aA 3)

;; -------------------------------------------------------------------
;; Part 4
;; -------------------------------------------------------------------

;test begin
(i-equal? (begin (define x4 5) (+ x4 3)) 8)
(i-equal? (begin (+ x4 3) (+ x4 2)) 7)
(i-equal? (begin) (void))
(i-equal? (begin (set! x4 (* x4 2)) x4) 10)
(i-equal? x4 10)

;test if
(i-equal? (if (cons 1 2) 'true 'false) 'true)
(i-equal? (if 'not-false #t #f) #t)
(i-equal? (if (< 5 4) 'less 'not-less) 'not-less)
(i-equal? (define lst4 (cons 1 (cons 2 null))) 'lst4)
(i-equal? (if (< (car lst4) (car (cdr lst4))) (car lst4) (car (cdr lst4))) 1)
(i-equal? (* 5 (if (= (+ 3 3) (- 8 2)) 10 20)) 50)
(i-equal? (define x41 5) 'x41)
(i-equal? (if (= x41 6) (+ x41 1)) (void))
(i-equal? (if (= x41 5) (+ x41 1)) 6)

;test cond
(i-equal? (cond 
              ((= 3 5) "this is false" "so we skip this one")
              ((= 3 3) "this is true" "so we return this message")
              ((= 5 5) "this is also true" 
                       "but since an earlier test was true" 
                       "this never gets returned")) "so we return this message")
(i-equal? (cond 
              (1  "since 1 is not #f, this evaluates to be true")
              (else "and once again, we never get here")) "since 1 is not #f, this evaluates to be true")
(i-equal? (cond
              ((< (+ 1 1) 0) 'nope)
              ((< 1 0) 'still-no)
              ((= 4 5) 'no-matches))  (void))
(i-equal? (cond 
              ((< (+ 1 1) 0) 'nope)
              ((< 1 0) 'still-no)
              (else 'matched-else)) 'matched-else)
(i-equal? (define x42 5) 'x42)
(i-equal? (cond
              ((set! x42 (+ x42 1)) x42)
              (else "Note: be sure x is 6, not 7")) 6)
(i-equal? x42 6)
(i-equal? (cond
              ((+ 1 1))) 2)
(i-equal? (cond) (void))

;; -------------------------------------------------------------------
;; Part 5
;; -------------------------------------------------------------------


(i-equal? (define value41 5) 'value41)
(i-equal? ((lambda (new) (set! value41 new)) 7) 'value41)
(i-equal? value41 7)

(i-equal? (define value42 7) 'value42)
(i-equal? (define count-down42
	    (lambda (n)
	      (if (= n 0)
		  '(lift-off)
		  (cons n (count-down42 (- n 1)))))) 
	  'count-down42)
(i-equal? (count-down42 value42) '(7 6 5 4 3 2 1 lift-off))
(i-equal? value42 7)

(i-equal?
 (define make-withdrawal
   (lambda (balance)
     (lambda (amount)
       (if (> balance amount)
	   (begin
	     (set! balance (- balance amount))
	     balance)
	   "Insufficient funds")))) 'make-withdrawal)
(i-equal? (define w41 (make-withdrawal 200)) 'w41)
(i-equal? (w41 250) "Insufficient funds")
(i-equal? (w41 30) 170)
(i-equal? (w41 50) 120)
(i-equal? (define w42 (make-withdrawal 100)) 'w42)
(i-equal? (w42 70) 30)
(i-equal? (w41 50) 70)


(i-equal? (define x402 200) 'x402)
(i-equal? (define y42 100) 'y42)
(i-equal? (define f42
	    (lambda (x402)
	      (+ x402 y42))) 'f42)
(i-equal? (f42 50) 150)
(i-equal? x402 200)
(i-equal? (define g42
	    (lambda (y42)
	      (f42 y42))) 'g42)
(i-equal? (g42 50) 150)
(i-equal? y42 100)
(i-equal? (set! y42 200) 'y42)
(i-equal? (g42 50) 250)

(i-equal? (let ((a 10))
	    (let ((b (lambda (x) (+ x a))))
	      (let ((a 500))
		(b a))))
	  510)
(i-equal? (let ((f car))
	    (let ((f (lambda (y) (f y))))
	      (f '(apple banana cantaloupe))))
	  'apple)
(i-equal? (define x43 1) 'x43)
(i-equal? (let ((x43 0) (y43 2) (z43 x43))
	    (set! x43 10)
	    (+ x43 y43 z43))
	  13)
(i-equal? x43 1)
(i-equal? (let ((zzzz (* x43 -1)))
	    (if (> zzzz 0) 'positive 'negative))
	  'negative)
(i-fail? zzzz)


(i-equal? (let ((x 2) (y 3))
            (let* ((x 7)
                   (z (+ x y)))
              (* z x)))
          70)
(i-equal? (define a42 10) 'a42)
(i-equal? (define b42 5) 'b42)
(i-equal? (let ((b42 (+ a42 5))
                (a42 (+ b42 5)))
            (+ a42 b42)) 25)
(i-equal? (let* ((b42 (+ a42 5))
                 (a42 (+ b42 5)))
            (+ a42 b42)) 35)


;; Manually testing meta-cicularity, we were able to run (+ 2 1) on the
;; 4th level of meta-ness, as well as the factorial test program on the
;; lower levels. 

;; testing map

(i-equal? (map (lambda (n) (+ n 1)) '(1 2 3 4 5)) '(2 3 4 5 6))
(i-equal? (map (lambda (n) (+ n 1)) '()) '())
(i-equal? (map (lambda (n) n) '(1 2 3 4 5)) '(1 2 3 4 5))

;; testing apply

(i-equal? (apply cons '(1 2)) '(1 . 2))
(i-equal? (apply car '((1 . 2))) 1)
(i-equal? (apply list '(1)) '(1))

;; testing and and or

(i-equal? (define xbool 5) 'xbool)
(i-equal? (define ybool 20) 'ybool)
(i-equal? (or (= xbool 3) (< ybool 10) (+ xbool ybool) (< ybool xbool)) 25)
(i-equal? (or (= xbool 3) (< ybool 10) (> xbool ybool) (< ybool xbool)) #f)
(i-equal? (and (= xbool 5) (< ybool 30) (+ xbool ybool) (< xbool ybool)) #t)
(i-equal? (and (= xbool 5) (< ybool 30) (+ xbool ybool) (< xbool ybool) (+ xbool ybool)) 25)
(i-equal? (and) #t)
(i-equal? (or) #f)

#| testing trace

;; Since trace's output is simply a printf statement, i-equal
;; does not work. However, the following code has been manually
;; tested. 

> (define f (lambda (x) (+ x 1)))
> (trace f)
(f)
> (f 5)
|(f 5)
|6
6
> (trace f)
(f)
> (untrace f)
(f)
> (f 5)
6
|#
