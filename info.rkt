#lang info

(define collection 'multi)

(define deps
  '("base"
    "pconvert-lib"
    "unstable-lib"
    "hygienic-quote-lang"
    ))

(define build-deps
  '("rackunit-lib"
    "unstable-macro-testing-lib"
    "scribble-lib"
    "racket-doc"
    ))

(define pkg-desc "A version of quote that helps to rid programs of bad uses of quote")

(define version "0.0")

(define license 'MIT)
