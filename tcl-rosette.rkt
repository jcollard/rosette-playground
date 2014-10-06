#lang s-exp rosette

(require 
  rosette/query/debug
  rosette/lib/tools/render 
  rosette/lib/meta/meta)


(provide (all-defined-out)
         ! && || <=> define/debug
         #%datum #%app #%module-begin #%top-interaction
         quote (for-syntax #%datum))

(define-syntax-rule (define-circuit (id in ...) expr)
  (define (id in ...) expr))

(define-synthax (Circuit [op1 op2 ...] expr ... #:depth d)
  #:assert (>= d 0)
  ([choose op1 identity]
   [choose
    expr ...
    ([choose op2 ...]
     (Circuit [op1 op2 ...] expr ... #:depth (- d 1))
     (Circuit [op1 op2 ...] expr ... #:depth (- d 1)))]))

(define (verify-circuit impl spec)
  (define n (procedure-arity spec))
  (for ([i (expt 2 n)])
    (define bits (for/list ([j n]) (bitwise-bit-set? i j)))
    (unless (eq? (apply impl bits) (apply spec bits))
      (error "verification failed on" bits))))

;(define (! a) (if a #f #t))
;(define (&& a b) (if a b #f))
;(define (|| a b) (if a #t b))
;(define (<=> a b) (if a b (! b)))

(define-symbolic b0 b1 b2 b3 boolean?)

(define (dynamic-choose)
  (define-symbolic* v boolean?)
  v)

(define-circuit (xor x y)
  (! (<=> x y)))

(define-circuit (RBC-parity a b c d)
  (xor (<=> a b) (<=> c d)))

;(define-circuit (AIG-parity a b c d)
; (&&
;  (! (&& (! (&& (! (&& a b)) (&& (! a) (! b))))
;         (! (&& (&& (! c) (! d)) (! (&& c d))))))
;  (! (&& (&& (! (&& a b)) (! (&& (! a) (! b))))
;         (&& (! (&& (! c) (! d))) (! (&& c d)))))))


;(define/debug (AIG-parity a b c d)
; (&&
;  (! (&& (! (&& (! (&& a b)) (&& (! a) (! b))))
;         (! (&& (&& (! c) (! d)) (! (&& c d))))))
;  (! (&& (&& (! (&& a b)) (! (&& (! a) (! b))))
;         (&& (! (&& (! c) (! d))) (! (&& c d)))))))

;(define core
;  (debug [boolean?]
;         (assert (eq? (AIG-parity #t #t #t #f)
;                      (RBC-parity #t #t #t #f)))))

;(define counterexample
;  (verify (assert (eq? (RBC-parity b0 b1 b2 b3)
;                       (AIG-parity b0 b1 b2 b3)))))


;(define-circuit (AIG-parity a b c d)
; (&&
;  (! (&& (! (&& (! (dynamic-choose)) (&& (! a) (! b))))
;         (! (&& (&& (! c) (! d)) (! (&& c d))))))
;  (! (&& (&& (! (&& a b)) (! (&& (! a) (! b))))
;         (&& (! (&& (! c) (! d))) (! (&& c d)))))));

;(solve (assert (eq? (AIG-parity #t #t #t #f)
;                    (RBC-parity #t #t #t #f))))
; >> solve: no satisfying execution found

;(define-circuit (AIG-parity a b c d)
; (&&
;  (dynamic-choose)
;  (! (&& (&& (! (&& a b)) (! (&& (! a) (! b))))
;         (&& (! (&& (! c) (! d))) (! (&& c d)))))));

#|
(define-circuit (AIG-parity a b c d)
    (&&
     (dynamic-choose)
     (! (&& (&& (! (&& a b)) (! (&& (! a) (! b))))
            (&& (! (&& (! c) (! d))) (! (&& c d)))))))

(solve 
 (begin
   (assert (eq? (AIG-parity #t #t #t #f)
                (RBC-parity #t #t #t #f)))
   (assert (eq? (AIG-parity #f #f #f #f)
                (RBC-parity #f #f #f #f)))))
(model
 [ v . 0) #t]
 [ v . 1) #f])
|#

(define-circuit (AIG-parity a b c d)
    (&&
     (Circuit [! &&] a b c d #:depth 3)
     (! (&& (&& (! (&& a b)) (! (&& (! a) (! b))))
            (&& (! (&& (! c) (! d))) (! (&& c d)))))))

#|
(define model
  (synthesize
   #:forall (list b0 b1 b2 b3)
   #:guarantee (assert (eq? (AIG-parity b0 b1 b2 b3)
                            (RBC-parity b0 b1 b2 b3)))))

(generate-forms model)

'(define-circuit
  (AIG-parity a b c d)
  (&&
   (!
    (&&
     (&&
      (! (&& (! a) b))
      (! (&& (! c) d)))
     (&&
      (! (&& c (! d)))
      (! (&& a (! b))))))
   (!
    (&&
     (&&
      (! (&& a b))
      (! (&& (! a) (! b))))
     (&&
      (! (&& (! c) (! d)))
      (! (&& c d)))))))

|#
