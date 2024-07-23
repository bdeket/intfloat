#lang typed/racket/base

(require racket/format
         math/flonum)
(require/typed racket/random [crypto-random-bytes (-> 8 Bytes)])
(require "intfloat.rkt")

(provide (except-out (all-defined-out) =>fl =<fl))

(module+ test
  (require typed/rackunit))

;; random flonum with equal weight to each flonum (relatively large nan.0 space ...)
(define (flrand) : Flonum (floating-point-bytes->real (crypto-random-bytes 8)))
(define (flabsrand) : Nonnegative-Flonum (flabs (flrand)))

(module+ test
  (check-true (flonum? (flrand)))
  (check-true (let ([A (flabsrand)]) (and (flonum? A) (or (flnan? A) (<= 0. A))))))

;; if x is an integer              , return the integer
;;         a list of one integer            a random integer in [0, x)
;;         a list of two integers           a random integer in [x0 , x1)
(define-type i-lst (U Integer (List Nonnegative-Integer) (List Integer Integer)))
(define (=>rand [x : i-lst]) : Integer
  (cond
    [(list? x)
     (cond
       [(null? (cdr x))
        (let lp ([m (car x)])
          (cond
            [(= m 0)          0]
            [(< m 4294967087) (random m)]
            [else
             (define-values (mh ml) (quotient/remainder m 4294967087))
             (+ (* 4294967087 (lp mh)) (random 4294967087))]))]
       [else
        (define A (- (cadr x) (car x)))
        (define B (min (car x) (cadr x)))
        (+ B (=>rand (list (abs A))))])]
    [else x]))

(module+ test
  (check-true (let ([A (=>rand 5)]) (= A 5)))
  (check-true (let ([A (=>rand '(2))]) (<= 0 A 1)))
  (check-true (let ([A (=>rand '(-4 -6))]) (<= -6 A -5))))

;; random flonum based on integer, out of bounds results in nan.0
(define (i=>flrand [x : i-lst]) : Flonum (ordinal*->flonum (=>rand x)))
(define (flrand-in [a : Flonum][b : Flonum]) : Flonum
  (i=>flrand (list (flonum->ordinal a) (flonum->ordinal b))))

(module+ test
  (check-true (let ([A (i=>flrand 5)]) (= A (ordinal->flonum 5))))
  (check-true (let ([A (i=>flrand '(2))]) (<= 0. A +min.0)))
  (check-true (let ([A (flrand-in 0. (flnext +min.0))]) (<= 0. A +min.0))))

(define (=>fl [lst : (Listof Integer)]) : Flonum
  (floating-point-bytes->real (apply bytes (reverse (map =>rand lst)))))

(define (=<fl [b : Flonum]) : (Listof Byte)
  (reverse (bytes->list (real->floating-point-bytes b 8))))

(define (=>>fl [s : (Option i-lst)][e : (Option i-lst)][m : (Option i-lst)]) : Flonum
  (define (check [x : i-lst][a : Integer][b : Integer][q : Index]) : Integer
    (define (err) (raise-argument-error '=>>fl
                                        (format "=>rands-spec for integer in [~a ; ~a)" a b)
                                        q s e m))
    (cond
      [(integer? x)
       (if (<= a x b) (=>rand x) (err))]
      [(null? (cdr x))
       (if (<= a (car x) b) (=>rand x) (err))]
      [else
       (if (and (<= a (car x) b) (<= a (cadr x) b)) (=>rand x) (err))]))
  (define mm (if (eq? m #f) (=>rand '(4503599627370496)) (check m 0 4503599627370496 3)))
  (define ee (if (eq? e #f) (=>rand '(2048))             (check e 0 2048 2)))
  (define ss (if (eq? s #f) (=>rand '(2))                (check s 0 2 1)))
  (define-values (eh el) (quotient/remainder ee 16))
  (define-values (m6 m5 m4 m3 m2 m1 m0)
    (let*-values ([(MM m0) (quotient/remainder mm 256)]
                  [(MM m1) (quotient/remainder MM 256)]
                  [(MM m2) (quotient/remainder MM 256)]
                  [(MM m3) (quotient/remainder MM 256)]
                  [(MM m4) (quotient/remainder MM 256)]
                  [(m6 m5) (quotient/remainder MM 256)])
      (values m6 m5 m4 m3 m2 m1 m0)))
  (=>fl (list (+ (* ss 128) eh)
              (+ (* el 16) m6)
              m5 m4 m3 m2 m1 m0)))

(define (=<<fl [b : Flonum]) : (List Integer Integer Integer)
  (define f (apply string-append (map (λ ([b : Byte]) (~r b #:base 2 #:min-width 8 #:pad-string "0"))
                                      (=<fl b))))
  (define (=> [s : String]) : Integer
    (assert (string->number (string-append "#b" s)) exact-integer?))
  (list (=> (substring f 0 1))
        (=> (substring f 1 12))
        (=> (substring f 12))))