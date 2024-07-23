#lang racket/base

(require math/flonum
         math/bigfloat)

(provide ->3 3println trans 3println-step)

(struct 3v (f b p))
(define (->3 f [b (bf f)]) (3v (fl f) b (bigfloat->flonum b)))

(struct 3f (f b)
  #:property prop:procedure
  (λ (this . lst)
    (->3 (apply (3f-f this) (map 3v-f lst))
         (apply (3f-b this) (map 3v-b lst)))))

(define (3println lst)
  (define (outer pick [lst lst])
    (let inner ([lst lst])
      (cond
        [(pair? lst) (cons (inner (car lst)) (inner (cdr lst)))]
        [(3v? lst)   (pick lst)]
        [else        lst])))
  (printf "FLO: ~a\n" (outer 3v-f))
  (printf "BIG: ~a\n" (outer 3v-p))
  (printf "ERR: ~a\n\n" (outer (λ (v) (flulp-error (3v-f v) (bigfloat->real (3v-b v)))))))

(define (trans lst)
  (cond
    [(list? lst) (cons (car lst) (map trans (cdr lst)))]
    [(number? lst) (->3 lst)]
    [else lst]))

(define (3println-step LST)
  (define lst (trans LST))
  (3println lst)
  (when (list? lst)
    (3println-step
     (let lp ([lst lst])
       (cond
         [(list? lst)
          (cond
            [(ormap list? lst)
             (map lp lst)]
            [else
             (apply
              (case (car lst)
                [(max) 3max] [(min) 3min]
                [(+) 3+] [(-) 3-] [(*) 3*] [(/) 3/]
                [(abs) 3abs] [(sgn) 3sgn]
                [(sqrt) 3sqrt] [(hypot) 3hypot] [(sqrt1pm1) 3sqrt1pm1]
                [(log) 3log] [(log2) 3log2] [(log1p) 3log1p] [(log1pmx) 3log1pmx]
                [(exp) 3exp] [(exp2) 3exp2] [(expm1) 3expm1] [(expsqr) 3expsqr] [(gauss) 3gauss] [(exp1p) 3exp1p]
                [(expt) 3expt] [(expt1p) 3expt1p] [(expt+) 3expt+]
                [(sin) 3sin] [(cos) 3cos] [(tan) 3tan] [(asin) 3asin] [(acos) 3acos] [(atan) 3atan]
                [(sinh) 3sinh] [(cosh) 3cosh] [(tanh) 3tanh] [(asinh) 3asinh] [(acosh) 3acosh] [(atanh) 3atanh]
                [(factorial) 3factorial]
                [else (error "undefined" (car lst))])
              (cdr lst))])]
         [else lst])))))

(define 3max (3f flmax bfmax))
(define 3min (3f flmin bfmin))

(define 3+ (3f + bf+))
(define 3- (3f - bf-))
(define 3* (3f * bf*))
(define 3/ (3f / bf/))

(define 3abs (3f flabs bfabs))
(define 3sgn (3f flsgn bfsgn))

(define 3sqrt (3f flsqrt bfsqrt))
(define 3hypot (3f flhypot bfhypot))
(define 3sqrt1pm1 (3f flsqrt1pm1 (λ (b) (bf- (bfsqrt (bf+ 1.bf b)) 1.bf))))

(define 3log (3f fllog bflog))
(define 3log2 (3f fllog2 bflog2))
;log10
(define 3log1p (3f fllog1p bflog1p))
(define 3log1pmx (3f fllog1pmx (λ (b) (bf- (bflog1p b) b))))

(define 3exp (3f flexp bfexp))
(define 3exp2 (3f flexp2 bfexp2))
;exp10
(define 3expm1 (3f flexpm1 bfexpm1))
(define 3expsqr (3f flexpsqr (λ (b) (bfexp (bfsqr b)))))
(define 3gauss (3f flgauss (λ (b) (bfexp (bf- (bfsqr b))))))
(define 3exp1p (3f flexp1p (λ (b) (bfexp (bf+ 1.bf b)))))

(define 3expt (3f flexpt bfexpt))
(define 3expt1p (3f flexpt1p (λ (b e) (bfexpt (bf+ 1.bf b) e))))
(define 3expt+ (3f flexpt+ (λ (b1 b2 e) (bfexpt (bf+ b1 b2) e))))

(define 3sin (3f flsin bfsin))
(define 3cos (3f flcos bfcos))
(define 3tan (3f fltan bftan))
(define 3asin (3f flasin bfasin))
(define 3acos (3f flacos bfacos))
(define 3atan (3f flatan bfatan))

(define 3sinh (3f flsinh bfsinh))
(define 3cosh (3f flcosh bfcosh))
(define 3tanh (3f fltanh bftanh))
(define 3asinh (3f flasinh bfasinh))
(define 3acosh (3f flacosh bfacosh))
(define 3atanh (3f flatanh bfatanh))

(define 3factorial (3f flfactorial bffactorial))