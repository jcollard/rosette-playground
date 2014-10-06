#lang s-exp "tcl-rosette.rkt"
;#lang s-exp rosette


;(define-symbolic b0 b1 b2 b3 boolean?)

(define-circuit (xor x y)
  (! (<=> x y)))

(define-circuit (RBC-parity a b c d)
  (xor (<=> a b) (<=> c d)))

(define-circuit (AIG-parity a b c d)
  (&&
   (! (&& (! (&& (! (&& a b)) (&& (! a) (! b))))
          (! (&& (&& (! c) (! d)) (! (&& c d))))))
   (! (&& (&& (! (&& a b)) (! (&& (! a) (! b))))
          (&& (! (&& (! c) (! d))) (! (&& c d)))))))

(verify-circuit AIG-parity RBC-parity)