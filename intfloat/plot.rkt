#lang typed/racket/base

(require racket/format
         math/flonum
         (only-in racket/math exact-round exact-floor exact-ceiling)
         plot/utils)
(require "intfloat.rkt")

(provide (all-defined-out))

(module+ test
  (require typed/rackunit))

(define intfloat-transform
  (make-axis-transform (invertible-function real->ordinal* ordinal*->real)))

(define (intfloat-ticks-layout [N : Positive-Integer (ticks-default-number)]) : Ticks-Layout
  (λ (MIN MAX)
    (define (chk [x : Real]) : Exact-Rational
      (cond
        [(exact? x) x]
        [else
         (define F (fl x))
         (cond
           [(flnan? F) (if (or (< MIN 0) (< MAX 0)) -nan.RIF +nan.RIF)]
           [(= F +inf.0) +inf.RIF]
           [(= F -inf.0) -inf.RIF]
           [else (inexact->exact F)])]))
    (let ([MIN (chk MIN)]
          [MAX (chk MAX)])
      (let ([MIN (min MIN MAX)]
            [MAX (max MIN MAX)])
        (define (DO1 [MIN : Nonnegative-Exact-Rational]
                     [MAX : Nonnegative-Exact-Rational]) : (Listof pre-tick)
          (define M- (max +min.RIF MIN))
          (define M+ (max +min.RIF MAX))
          (if (< (/ M+ M-) 2)
              ((linear-ticks-layout #:number N #:base 10) MIN MAX)
              (let ([A ((log-ticks-layout #:number N #:base 10) M- M+)])
                (if (= 0 MIN) (cons (pre-tick 0 #t) (cdr A)) A))))
        (cond
          [(and (<= 0 MIN) (<= 0 MAX)) ;; (< 0 MIN MAX)
           (DO1 MIN MAX)]
          [(and (<= MIN 0) (<= MAX 0))
           (foldl (λ ([t : pre-tick][L : (Listof pre-tick)]) : (Listof pre-tick)
                    (cons (struct-copy pre-tick t [value (- (pre-tick-value t))])
                          L))
                  '()
                  (DO1 (abs MAX) (abs MIN)))]
          [else
           (assert MAX positive?)
           (assert MIN negative?)
           (define d (/ MAX (- MIN)))
           (define A
             (cond
               [(<= 1 d)
                (define D (exact-round d))
                (define Q (/ (max 1 (- N 3)) (+ D 1)))
                (define R (exact-ceiling (* D Q)))
                ((log-ticks-layout #:number (max 1 R) #:base 10) +min.0 (abs MAX))]
               [else
                (define D (exact-round (/ d)))
                (define Q (/ (max 1 (- N 3)) (+ D 1)))
                (define R (exact-ceiling (* D Q)))
                ((log-ticks-layout #:number (max 1 R) #:base 10) +min.0 (abs MIN))]))
           `(,@(for*/list : (Listof pre-tick)
                 ([t (in-list (reverse A))]
                  [T (in-value (- (pre-tick-value t)))])
                 (struct-copy pre-tick t [value T]))
             ,(pre-tick 0 #t)
             ,@(filter (λ ([t : pre-tick]) (<= (pre-tick-value t) MAX)) A))])))))

(module+ test
  (check-equal? (car ((intfloat-ticks-layout) 0 +inf.RIF)) (pre-tick 0 #t))
  (let* ([A ((intfloat-ticks-layout) -inf.0 +inf.0)]
         [B (pre-tick-value (car ((intfloat-ticks-layout) -inf.0 +inf.0)))])
    (define V (map pre-tick-value A))
    (check-true (apply <= (car V) (cadr V) (cddr V)))
    (check-true (and (real? B) (exact? B) (< B 0)))))

(define intfloat-ticks-format : Ticks-Format
  (λ (MIN MAX pre)
    (map (λ ([t : pre-tick])
           (define T (pre-tick-value t))
           (cond
             [(< T -max.RIF)  "-inf"]
             [(< +max.RIF T) "+inf"]
             [(= T 1) "1"]
             [(= T 0) "0"]
             [(= T -1) "-1"]
             [else (~r T #:notation 'exponential)]))
         pre)))

(define (intfloat-ticks #:number [N : Positive-Integer (ticks-default-number)]) : ticks
  (ticks (intfloat-ticks-layout N) intfloat-ticks-format))
