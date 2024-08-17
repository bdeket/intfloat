#lang scribble/manual

@(require (for-label racket/base
                     math/flonum
                     plot
                     (only-in typed/racket/base Integer Exact-Rational Positive-Integer Flonum Nonnegative-Integer)
                     "main.rkt"
                     )
          scribble/examples
          (only-in scribble/eval interaction)
          racket/sandbox)

@title[#:tag "top"]{intfloat}

This library provides some helper functions for testing implementations of flonum functions. It defines
@itemlist[
 @item{additions to the @racket[flonum->ordinal] functions}
 @item{functions for generating random @racket[Flonum]s within specific ranges}
 @item{an @racket[axis-transform/c] and @racket[ticks?] for plotting over the complete range of flonums}
 @item{a flonum stepper}]

@section{intfloat}
@defmodule[intfloat/intfloat]

@deftogether[[@defthing[+maxi.IF Integer]
              @defthing[-maxi.IF Integer]
              @defthing[+nan.IF Integer]
              @defthing[-nan.IF Integer]
              @defthing[+inf.IF Integer]
              @defthing[-inf.IF Integer]]]
Values for the biggest/smallest ordinal that @racket[flonum->ordinal] can produce, and for @racket[+nan.0] etc. All values from @racket[+nan.IF] to @racket[+maxi.IF] are encoded as @racket[+nan.0].

@deftogether[[@defproc[(ordinal+/nan [ordinal Integer] [step Integer]) Integer]
              @defproc[(ordinal+/inf [ordinal Integer] [step Integer]) Integer]
              @defproc[(ordinal+ [ordinal Integer] [step Integer]) Integer]]]
Similar as @racket[flstep], but in ordinal space. @racket[ordinal+/nan] caps the maximum value at an ordinal for @racket[+nan.0], the @racket[ordinal+/inf] and @racket[ordinal+] do the same, but capping at @racket[+inf.0]

@deftogether[[@defthing[+nan.RIF Integer]
              @defthing[-nan.RIF Integer]
              @defthing[+inf.RIF Integer]
              @defthing[-inf.RIF Integer]
              @defthing[+max.RIF Integer]
              @defthing[-max.RIF Integer]]]
Exact representations the maximum/minimum flonum (ie @racket[+max.RIF] is equivalent to @racket[(inexact->exact +max.0)]). Infinite is the first integer for which this would result in @racket[+inf.0], and nan a step further away (a step from the next to last flonum to the last).

@deftogether[[@defproc[(ordinal*->flonum [o Real]) Flonum]
              @defproc[(ordinal*->real [o Real]) Exact-Rational]
              @defproc[(real->ordinal* [o Real]) Exact-Rational]]]
Extension of the @racket[ordinal->flonum] and reciprocal to the complete @racket[Real]s. For an exact rational that lies between two integers, @racket[ordinal*->flonum] will return the @racket[Flonum] for the closest integer, and @racket[ordinal*->real] a real that lies at the same distance between the exact representations of the previous and next integer.

@section{random}
@defmodule[intfloat/random]

@deftogether[[@defproc[(flrand) Flonum]
              @defproc[(flabsrand) Flonum]]]
Generate a random flonum. Each flonum has the same probability of being chosen, except for @racket[+nan.0] since this has more than 1 representation. The @racket[flabsrand] only generates @racket[Nonnegative-Flonum]s.

@defproc[(=>rand [x (U Integer (List Positive-Integer) (List Integer Integer))]) Integer]
If x is an integer, return that @racket[Integer]. If x is a list of 1 @racket[Positive-Integer] generate an integer in the range [0-x). If x is a list of 2 @racket[Integer]s, generate an @racket[Integer] in the range [x0-x1). Unlike @racket[(random)], there is no upper limit on the maximum value.

@defproc[(i=>flrand [x (U Integer (List Positive-Integer) (List Integer Integer))]) Flonum]
Generate a random @racket[Flonum] based on the ranges specified for x, using @racket[ordinal->flonum].

@defproc[(flonum-in [a Flonum][b Flonum]) Flonum]
Generate a random @racket[Flonum] in the range [a-b). As with other procedures in this module, each representable @racket[Flonum] has the same probability.

@deftogether[[@defproc[(=<<fl [f Flonum]) (List Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer)]
              @defproc[(=>>fl [s (U #f Integer (List Positive-Integer) (List Integer Integer)) #f]
                              [e (U #f Integer (List Positive-Integer) (List Integer Integer)) #f]
                              [m (U #f Integer (List Positive-Integer) (List Integer Integer)) #f]) Flonum]]]
Decompose a flonum in its sign bit, its exponent and mantissa. Or from 3 @racket[=>rand]-like specs, generate a flonum. If the arguments are #f, for the sign @racket[s]: @racket['(1)] is used. For the exponent @racket[e]: @racket['(2048)] is used. And for the mantissa @racket[m]: @racket['(4503599627370496)] is used.

@section{plot}
@defmodule[intfloat/plot]
@defthing[intfloat-transform axis-transform/c]
Similar result as with the @racket[log-transform] in output. But works by the internal transformation of @racket[flonum->ordinal] so that negative values, @racket[-inf.0], @racket[0.0], @racket[+inf.0] and @racket[+nan.0] also have a representation on the axis.
@deftogether[[@defproc[(intfloat-ticks-layout [number Postive-Integer (ticks-default-number)]) ticks-layout/c]
              @defthing[intfloat-ticks-format ticks-format/c]
              @defproc[(intfloat-ticks [number Postive-Integer (ticks-default-number)]) ticks?]]]
Ticks to work together with the @racket[intfloat-transform]
@interaction[#:eval (let ([E (make-base-eval)]) (E '(require "main.rkt" plot/pict plot/utils)) E)
             (parameterize ([plot-x-transform intfloat-transform]
                            [plot-x-ticks (intfloat-ticks)])
               (plot (function values)
                     #:x-min -inf.RIF #:x-max +inf.RIF))]

@deftogether[[@defproc[(ordinal-ticks-layout [number Postive-Integer (ticks-default-number)]) ticks-layout/c]
              @defthing[ordinal-ticks-format ticks-format/c]
              @defproc[(ordinal-ticks [number Postive-Integer (ticks-default-number)]) ticks?]]]
Similar as the @racket[infloat-ticks] above, but works completely in ordinal space. This has the advantage of working faster (no extreme big-integers or rationals) but the disadvantage that all supplied values need to be converted from/to ordinal space by the renderers and boundaries.

@deftogether[[@defproc[(make-o1->o1 [F (-> Flonum Flonum)]) (-> Real Real)]
              @defproc[(make-o1->r1 [F (-> Flonum Flonum)]) (-> Real Flonum)]
              @defproc[(make-o2->o1 [F (-> Flonum Flonum)]) (-> Real Real)]
              @defproc[(make-o2->r1 [F (-> Flonum Flonum)]) (-> Real Flonum)]]]
Helpers to convert flonum-functions to be usable in ordinal space. Equivalent to:
@codeblock{
 (λ (x) (flonum->ordinal (F (ordinal*->flonum x))))
 (λ (x) (F (ordinal*->flonum x)))
 (λ (x y) (flonum->ordinal (F (ordinal*->flonum x) (ordinal*->flonum y))))
 (λ (x y) (F (ordinal*->flonum x) (ordinal*->flonum y)))}


@section{flstepper}
@defmodule[intfloat/flstepper]
@defproc[(3println-step [expr (list/c Any)]) void]
Given a symbolic expression, step through the evaluation and compare it with a bigfloat-evaluation.

@examples[#:eval (parameterize ([sandbox-output 'string]
                                [sandbox-error-output 'string])
                   (make-evaluator 'racket/base #:requires '("main.rkt")))
          (3println-step '(log (- (/ 100000 99999) (/ 100000 100001))))]

@defproc[(3println [expr (list/c Any)]) void]
Given a symbolic expression, print it once once flonum space, once in bigfloat space and a third time showing the @racket[flulp-error] between the two.

@defproc[(trans [expr (list/c Any)]) (list/c Any)]
Given a symbolic expression, convert all numbers in it to a representation that can be used by @racket[3println]

@defproc[(->3 [f Real] [b Bigfloat (bf f)]) ?]
Convert a number to representation that can be used by the @racket[3println]
