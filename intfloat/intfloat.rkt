#lang typed/racket/base

(require math/flonum
         (only-in racket/math exact-round exact-floor exact-ceiling))

(provide (all-defined-out)
         flonum->ordinal ordinal->flonum
         +max.0 -max.0 +min.0 -min.0 +max-subnormal.0 -max-subnormal.0)

(module+ test
  (require typed/rackunit))

;; *************************************************************************************************
;; * INTFLOATS
;; *************************************************************************************************
;; map from integer to flonum and back, ie each flonum maps to 1 integer,
;; and consecutive flonums map to consecutive integers, with 0. mapping to 0
;; the only exception to this is inf and nan. The gap between these is larger. (ie there are a
;; lot of inf's and nan's

(define +maxi.IF  9223372036854775807 #;(- (/ (expt 2 64) 2) 1))
(define -maxi.IF -9223372036854775807 #;(- intfloat-max (expt 2 64) -2))
(define +nan.IF (flonum->ordinal +nan.0))
(define +inf.IF (flonum->ordinal +inf.0))
(define -inf.IF (flonum->ordinal -inf.0))
(define -nan.IF (- -inf.IF 1))

(module+ test
  (check-equal? (ordinal->flonum +maxi.IF) +nan.0)
  (check-equal? (ordinal->flonum -maxi.IF) +nan.0)
  (check-equal? (ordinal->flonum +nan.IF) +nan.0)
  (check-equal? (ordinal->flonum -nan.IF) +nan.0)
  (check-equal? (ordinal->flonum +inf.IF) +inf.0)
  (check-equal? (ordinal->flonum -inf.IF) -inf.0))

(define (ordinal+/nan [i : Integer] [δ : Integer]) : Integer
  (define I (+ i δ))
  (cond
    [(<= -nan.IF I +nan.IF) I]
    [(< I 0)  -nan.IF]
    [else     +nan.IF]))
(define (ordinal+/inf [i : Integer] [δ : Integer]) : Integer
  (define I (+ i δ))
  (cond
    [(<= -inf.IF I +inf.IF) I]
    [(< I 0)           -inf.IF]
    [else              +inf.IF]))
(define ordinal+ ordinal+/inf)

(module+ test
  (check-equal? (ordinal->flonum (ordinal+/inf (flonum->ordinal 2.) 1))
                (flnext 2.))
  (check-equal? (ordinal->flonum (ordinal+/inf (flonum->ordinal 8.) 3))
                (flnext (flnext (flnext 8.))))
  (check-equal? (ordinal->flonum (ordinal+/inf (flonum->ordinal 1e3) -1))
                (flprev 1e3))
  (check-equal? (ordinal->flonum (ordinal+/inf (flonum->ordinal +min.0) -4))
                (flprev (flprev (flprev (flprev +min.0)))))
  (check-equal? (ordinal->flonum (ordinal+/inf (flonum->ordinal +max.0) 50))
                +inf.0)
  (check-equal? (ordinal->flonum (ordinal+/inf (flonum->ordinal -max.0) -50))
                -inf.0)
  (check-equal? (ordinal->flonum (ordinal+/nan (flonum->ordinal +max.0) 1111111111111111111111111))
                +nan.0)
  (check-equal? (ordinal+/nan (flonum->ordinal -max.0) -11111111111111111111111111111111111111)
                -nan.IF))

;; *************************************************************************************************
;; * ordinal*
;; *************************************************************************************************
;; As ordinal, but generalised to REALS, ie if ordinal A maps to flonum a
;; then A+1/2 maps to a Real that lies halfway between
;; (inexact->exact a) and (inexact->exact (flnext a))
(define +min.RIF (inexact->exact +min.0))
(define +max.RIF (inexact->exact +max.0))
(define +inf.RIF (+ +max.RIF (* 1/2 (- +max.RIF (inexact->exact (flprev +max.0))))))
(define +nan.RIF (+ +inf.RIF (- +max.RIF (inexact->exact (flprev +max.0)))))
(define -min.RIF (inexact->exact -min.0))
(define -max.RIF (inexact->exact -max.0))
(define -inf.RIF (- +inf.RIF))
(define -nan.RIF (- +nan.RIF))

(define (ordinal*->flonum [r : Real]) : Flonum
  (define R (exact-round r))
  (cond
    [(or (< R -inf.IF) (< +inf.RIF R))  +nan.0]
    [(= -inf.IF R)                      -inf.0]
    [(= +inf.IF R)                      +inf.0]
    [else (ordinal->flonum R)]))
(define (ordinal*->real [r : Real]) : Exact-Rational
  (define R (exact-round r))
  (define F (exact-floor r))
  (cond
    [(or (< R -inf.IF) (< +inf.IF R)) (if (< R 0) -nan.RIF +nan.RIF)]
    [(= -inf.IF R)                      -inf.RIF]
    [(= +inf.IF R)                      +inf.RIF]
    [else
     (define r1 (inexact->exact (ordinal->flonum F)))
     (define r2 (inexact->exact (ordinal->flonum (ordinal+ F 1))))
     (+ r1 (* (- (inexact->exact r) F) (- r2 r1)))]))

(define (real->ordinal* [f : Real]) : Exact-Rational
  (define F (exact-round f))
  (cond
    [(flonum? f) (flonum->ordinal f)]
    [(single-flonum? f) (flonum->ordinal (fl f))]
    [(<= F -max.RIF) -nan.IF]
    [(<= +max.RIF F) +nan.IF]
    [else
     (define f0 (fl f))
     (define F0 (flonum->ordinal f0))
     (define δf1 (- (inexact->exact f0) f))
     (cond
       [(= 0 δf1) F0]
       [else
        (define f1 ((if (< δf1 0) flprev flnext) f0))
        (define Δf1 (abs (- (inexact->exact ((if (< δf1 0) flprev flnext) f0)) f)))
        (+ F0 (/ δf1 Δf1))])]))

(module+ test
  (check-equal? (ordinal*->real  9221120237041090560) +nan.RIF)
  (check-equal? (ordinal*->real -9221120237041090560) -nan.RIF)
  (check-equal? (ordinal*->real +inf.IF) +inf.RIF)
  (check-equal? (ordinal*->real -inf.IF) -inf.RIF))