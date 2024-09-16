quote - bad
===
A version of quote that helps to rid programs of bad uses of quote.

documentation: http://docs.racket-lang.org/quote-bad/index.html

The `quote` form can seem convenient as a shorthand, but it can
lead to many common mistakes. Most of these mistakes come from people
not understanding that a quoted form also quotes its sub-expressions.

In a file that requires this library, using `quote` for
compound data will cause an error with a message telling you how to
write it without `quote`, and "self-quoting" compound data
literals such as vector literals will display a similar error
explaining how to write the same thing without them.

Examples:

```racket
> (require quote-bad/quote-bad)
> 'hello
'hello
> '"this is fine"
"this is fine"
> '#:of-course
'#:of-course
> '(1 2 (+ 1 2))
;. quote: Don't use quote for this. Instead you can use
;(list 1 2 (list '+ 1 2))
; in: (quote (1 2 (+ 1 2)))
> (list 1 2 (list '+ 1 2))
(list 1 2 (list '+ 1 2))
> (list 1 2 (+ 1 2))
(list 1 2 3)
> #(1 2 (+ 1 2))
;. #%datum: Don't use self-quoting compound literals that implicitly quote sub-expressions.
;Instead you can use
;(vector-immutable 1 2 (list '+ 1 2))
; in: (#%datum . #(1 2 (+ 1 2)))
> (vector-immutable 1 2 (list '+ 1 2))
(vector-immutable 1 2 (list '+ 1 2))
> (vector-immutable 1 2 (+ 1 2))
(vector-immutable 1 2 3)
```

