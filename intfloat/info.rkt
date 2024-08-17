#lang info

(define collection "intfloat")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "intfloat")))

(define deps
  (list "base"
        "typed-racket-lib"
        "math-lib"
        "plot-lib"
        ))

(define build-deps
  (list "racket-doc"
        "typed-racket-doc"
        "plot-doc"
        "plot-gui-lib"
        "math-doc"
        "sandbox-lib"
        "scribble-lib"
        "rackunit-typed"
        ))