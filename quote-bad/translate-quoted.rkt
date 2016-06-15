#lang racket/base

(provide translate-quoted
         atomic-literal-data?
         )

(require racket/list
         racket/match
         racket/set
         unstable/struct
         )
(module+ test
  (require rackunit))

;; translate-quoted : Stx -> S-Expr
(define (translate-quoted stuff)
  (cond
    [(syntax? stuff) (translate-quoted (or (syntax->list stuff) (syntax-e stuff)))]
    [(symbol? stuff) (list 'quote stuff)] ; the two places this recommends quote
    [(keyword? stuff) (list 'quote stuff)]
    [(atomic-literal-data? stuff) stuff]
    [(list? stuff) (list* 'list (map translate-quoted stuff))]
    [(cons? stuff) (translate-quoted/cons stuff)]
    [(mpair? stuff) (translate-quoted/mcons stuff)]
    [(vector? stuff) (translate-quoted/vector stuff)]
    [(hash? stuff) (translate-quoted/hash stuff)]
    [(hash-set? stuff) (translate-quoted/hash-set stuff)]
    [(box? stuff) (translate-quoted/box stuff)]
    [(prefab-struct-key stuff) (translate-quoted/prefab-struct stuff)]
    [else '....]))

;; atomic-literal-data? : Any -> Boolean
(define (atomic-literal-data? stuff)
  (or (boolean? stuff)
      (number? stuff)
      (char? stuff)
      (symbol? stuff)
      (string? stuff)
      (bytes? stuff)
      (keyword? stuff)
      (regexp? stuff)
      (byte-regexp? stuff)
      ))

;; hash-set? : Any -> Boolean
(define (hash-set? v)
  (or (set? v) ; only immutable
      (set-mutable? v) ; only non-weak-mutable
      (set-weak? v) ; only weak-mutable
      ))

;; translate-quoted-cons-s-expr : (Cons Stx Stx) -> S-Expr
(define (translate-quoted/cons stuff)
  (match stuff
    [(cons a (and b (not (? cons?))))
     (list 'cons (translate-quoted a) (translate-quoted b))]
    [(list-rest as ... (and b (not (? cons?))))
     (append (list 'list*)
             (map translate-quoted as)
             (list (translate-quoted b)))]))

;; translate-quoted-mcons-s-expr : (MPairof Stx Stx) -> S-Expr
(define (translate-quoted/mcons stuff)
  ;; no mlist, just nested mcons calls
  (list 'mcons (translate-quoted (mcar stuff)) (translate-quoted (mcdr stuff))))

;; translate-quoted-vector-s-expr : (Vectorof Stx) -> S-Expr
(define (translate-quoted/vector stuff)
  (define vector-proc-args (map translate-quoted (vector->list stuff)))
  (cond [#true ;(immutable? stuff) ; Vector literals should be immutable once expanded
         (list* 'vector-immutable vector-proc-args)]
        [else
         (list* 'vector vector-proc-args)]))

;; translate-quoted-hash-s-expr : (Hashof Stx Stx) -> S-Expr
(define (translate-quoted/hash stuff)
  (cond [(immutable? stuff)
         (translate-quoted-immutable-hash-s-expr stuff)]
        [(hash-weak? stuff)
         (translate-quoted-weak-hash-s-expr stuff)]
        [else
         (translate-quoted-mutable-hash-s-expr stuff)]))

;; translate-quoted-immutable-hash-s-expr : (Immutable-Hashof Stx Stx) -> S-Expr
(define (translate-quoted-immutable-hash-s-expr stuff)
  (define hash-proc-args
    (append* (for/list ([(k v) (in-hash stuff)])
               (list (translate-quoted k)
                     (translate-quoted v)))))
  (cond
    [(hash-equal? stuff)
     (list* 'hash hash-proc-args)]
    [(hash-eqv? stuff)
     (list* 'hasheqv hash-proc-args)]
    [(hash-eq? stuff)
     (list* 'hasheq hash-proc-args)]
    [else '....]))

;; translate-quoted-mutable-hash-s-expr : (Mutable-Hashof Stx Stx) -> S-Expr
(define (translate-quoted-mutable-hash-s-expr stuff)
  (list 'make-hash '....))

;; translate-quoted-weak-hash-s-expr : (Weak-Hashof Stx Stx) -> S-Expr
(define (translate-quoted-weak-hash-s-expr stuff)
  (list 'make-weak-hash '....))

;; translate-quoted-hash-set-s-expr : (Hash-Setof Stx) -> S-Expr
(define (translate-quoted/hash-set stuff)
  (cond
    [(set? stuff)
     (translate-quoted-immutable-hash-set-s-expr stuff)]
    [(set-mutable? stuff)
     (translate-quoted-mutable-hash-set-s-expr stuff)]
    [(set-weak? stuff)
     (translate-quoted-weak-hash-set-s-expr stuff)]
    [else '....]))

;; translate-quoted-immutable-hash-set-s-expr : (Immutable-Hash-Setof Stx) -> S-Expr
(define (translate-quoted-immutable-hash-set-s-expr stuff)
  (define set-proc-args (set->list stuff))
  (cond
    [(set-equal? stuff)
     (list* 'set set-proc-args)]
    [(set-eqv? stuff)
     (list* 'seteqv set-proc-args)]
    [(set-eq? stuff)
     (list* 'seteq set-proc-args)]
    [else '....]))

;; translate-quoted-mutable-hash-set-s-expr : (Mutable-Hash-Setof Stx) -> S-Expr
(define (translate-quoted-mutable-hash-set-s-expr stuff)
  (list 'mutable-set '....))

;; translate-quoted-weak-hash-set-s-expr : (Weak-Hash-Setof Stx) -> S-Expr
(define (translate-quoted-weak-hash-set-s-expr stuff)
  (list 'weak-set '....))

;; translate-quoted-box-s-expr : (Boxof Stx) -> S-Expr
(define (translate-quoted/box stuff)
  (cond [#true ;(immutable? stuff) ; Box literals should be immutable once expanded
         (list 'box-immutable (translate-quoted (unbox stuff)))]
        [else
         (list 'box (translate-quoted (unbox stuff)))]))

;; translate-quoted-prefab-struct-s-expr : Prefab-Struct -> S-Expr
(define (translate-quoted/prefab-struct stuff)
  (list* 'make-prefab-struct
         (translate-quoted (prefab-struct-key stuff))
         (map translate-quoted (struct->list stuff))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define t translate-quoted)

  (test-case "simple atomic data"
    (check-equal? (t 'abc) ''abc)
    (check-equal? (t #true) #true)
    (check-equal? (t #false) #false)
    (check-equal? (t 2) 2)
    (check-equal? (t 4.5) 4.5)
    (check-equal? (t "abc") "abc")
    (check-equal? (t #"abc") #"abc")
    (check-equal? (t '#:abc) ''#:abc)
    )
  (test-case "constructed data"
    (check-equal? (t '()) '(list))
    (check-equal? (t '(a b c (d) (e (f)) ((g))))
                  '(list 'a 'b 'c (list 'd) (list 'e (list 'f)) (list (list 'g))))
    (check-equal? (t '(#:abc)) '(list '#:abc))
    (check-equal? (t '(a . b)) '(cons 'a 'b))
    (check-equal? (t '([a . b] [c . d] [e . f]))
                  '(list (cons 'a 'b) (cons 'c 'd) (cons 'e 'f)))
    (check-equal? (t '(a b . c)) '(list* 'a 'b 'c))
    (check-equal? (t '(a b . (c))) '(list 'a 'b 'c))
    (check-equal? (t '#(a b c (d) #(e (f)) ((g))))
                  '(vector-immutable 'a 'b 'c (list 'd)
                                     (vector-immutable 'e (list 'f))
                                     (list (list 'g))))
    (check-match (t '#hash([a . b] [c . d] [e . f]))
                 (or '(hash 'a 'b 'c 'd 'e 'f)
                     '(hash 'a 'b 'e 'f 'c 'd)
                     '(hash 'c 'd 'a 'b 'e 'f)
                     '(hash 'c 'd 'e 'f 'a 'b)
                     '(hash 'e 'f 'a 'b 'c 'd)
                     '(hash 'e 'f 'c 'd 'a 'b)))
    (check-equal? (t '#&a) '(box-immutable 'a))
    (check-equal? (t '#s(hello 1 2 3)) '(make-prefab-struct 'hello 1 2 3))
    )
  )
