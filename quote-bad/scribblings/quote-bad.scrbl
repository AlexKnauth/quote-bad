#lang scribble/manual

@(require scribble/eval
          (for-label (except-in racket/base quote #%datum)
                     quote-bad/quote-bad
                     racket/vector
                     ))

@(define (make-ev)
   (define ev (make-base-eval))
   (ev '(require constructor-style-print/lang/runtime-config))
   (ev '(configure #f))
   ev)

@title{quote - bad}

source code: @url{https://github.com/AlexKnauth/quote-bad}

@local-table-of-contents[]

@section{Replacing bad uses of quote in programs}

@defmodule[quote-bad/quote-bad]{

The @racket[quote] form can seem convenient as a shorthand, but it can
lead to many common mistakes. Most of these mistakes come from people
not understanding that a quoted form also quotes its sub-expressions.

In a file that requires this library, using @racket[quote] for
compound data will cause an error with a message telling you how to
write it without @racket[quote], and "self-quoting" compound data
literals such as vector literals will display a similar error
explaining how to write the same thing without them.

@examples[#:eval (make-ev)
  (require quote-bad/quote-bad)
  'hello
  '"this is fine"
  '#:of-course
  '(1 2 (+ 1 2))
  (list 1 2 (list '+ 1 2))
  (list 1 2 (+ 1 2))
  #(1 2 (+ 1 2))
  (vector-immutable 1 2 (list '+ 1 2))
  (vector-immutable 1 2 (+ 1 2))
]

Unfortunately, this doesn't do anything about printing (yet), so
depending on your settings, values like lists, vectors, etc. could
still print with @racket[quote].

Note that this does not do anything to @racket[quasiquote], so if
there any expressions that really do make sense as quoted, you can
quasiquote them instead.
}

@defform[(quote datum)]{
Like racket's quote form, except that for compound literal data it
displays an error message explaining how to write the expression
without @racket[quote].
}

@defform[(#%datum . datum)]{
A form implicitly inserted for literals. This version expands to
@racket[quote] for simple atomic data such as numbers, strings, and
booleans, but for self-quoting compound data such as vector literals,
it displays an error message explaining how to write the expression
without using self-quoting literals.
}

@section{Changing the printing style to avoid printing bad uses of quote}

@defmodule[constructor-style-print #:lang]{

A meta-language like @racket[at-exp] that changes the printing style
for programs that use it in the main file. The lang line @hash-lang[]
@racketmodname[constructor-style-print] @racketmodname[racket]
declares a language like @racketmodname[racket], except that values
are printed using constructor-style printing instead of with
@racket[quote].

Instead of @racket['(1 2 3)], a list would print as
@racket[(list 1 2 3)]. Instead of @racket['#(1 2 3)], a mutable vector
would print as @racket[(vector 1 2 3)], and an immutable vector would
print as @racket[(vector-immutable 1 2 3)].

But most importantly, a nested structure like @racket['(1 2 (+ 1 2))]
would print as @racket[(list 1 2 (list '+ 1 2))], showing the nested
uses of the constructors.

@examples[#:eval (make-ev)
  (require racket/vector)
  'hello
  (list 1 2 (+ 1 2))
  '(1 2 (+ 1 2))
  (vector-immutable 1 2 (+ 1 2))
  #(1 2 (+ 1 2))
  (vector 1 2 (+ 1 2))
  (vector-copy #(1 2 (+ 1 2)))
]}

