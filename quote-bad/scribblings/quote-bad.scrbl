#lang scribble/manual

@(require scribble/eval
          (for-label (except-in racket/base quote #%datum)
                     quote-bad/quote-bad
                     ))

@title{quote - bad}

source code: @url{https://github.com/AlexKnauth/quote-bad}

@defmodule[quote-bad/quote-bad]{

The @racket[quote] form can seem convenient as a shorthand, but it can
lead to many common mistakes. Most of these mistakes come from people
not understanding that a quoted form also quotes its sub-expressions.

In a file that requires this library, using @racket[quote] for
compound data will cause an error with a message telling you how to
write it without @racket[quote], and "self-quoting" compound data
literals such as vector literals will display a similar error
explaining how to write the same thing without them.

@examples[
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

Unfortunately, this doesn't do anything about printing (yet).

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

