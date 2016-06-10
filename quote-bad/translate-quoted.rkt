#lang racket/base

(provide translate-quoted-s-expr
         atomic-literal-data?
         )

(require racket/list
         racket/match
         racket/set
         unstable/struct
         )
(module+ test
  (require rackunit))

;; translate-quoted-s-expr : Stx -> S-Expr
(define (translate-quoted-s-expr stuff)
  (cond
    [(syntax? stuff) (translate-quoted-s-expr (or (syntax->list stuff) (syntax-e stuff)))]
    [(symbol? stuff) (list 'quote stuff)] ; the two places this recommends quote
    [(keyword? stuff) (list 'quote stuff)]
    [(atomic-literal-data? stuff) stuff]
    [(list? stuff) (list* 'list (map translate-quoted-s-expr stuff))]
    [(cons? stuff) (translate-quoted-cons-s-expr stuff)]
    [(mpair? stuff) (translate-quoted-mcons-s-expr stuff)]
    [(vector? stuff) (translate-quoted-vector-s-expr stuff)]
    [(hash? stuff) (translate-quoted-hash-s-expr stuff)]
    [(hash-set? stuff) (translate-quoted-hash-set-s-expr stuff)]
    [(box? stuff) (list 'box-immutable (translate-quoted-s-expr (unbox stuff)))]
    [(prefab-struct-key stuff) (translate-quoted-prefab-struct-s-expr stuff)]
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
  (set? v))

;; translate-quoted-cons-s-expr : (Cons Stx Stx) -> S-Expr
(define (translate-quoted-cons-s-expr stuff)
  (match stuff
    [(cons a (and b (not (? cons?))))
     (list 'cons (translate-quoted-s-expr a) (translate-quoted-s-expr b))]
    [(list-rest as ... (and b (not (? cons?))))
     (append (list 'list*)
             (map translate-quoted-s-expr as)
             (list (translate-quoted-s-expr b)))]))

;; translate-quoted-mcons-s-expr : (MPairof Stx Stx) -> S-Expr
(define (translate-quoted-mcons-s-expr stuff)
  ;; no mlist, just nested mcons calls
  (list 'mcons (translate-quoted-s-expr (mcar stuff)) (translate-quoted-s-expr (mcdr stuff))))

;; translate-quoted-vector-s-expr : (Vectorof Stx) -> S-Expr
(define (translate-quoted-vector-s-expr stuff)
  (define vector-proc-args (map translate-quoted-s-expr (vector->list stuff)))
  (cond [(immutable? stuff)
         (list* 'vector-immutable vector-proc-args)]
        [else
         (list* 'vector vector-proc-args)]))

;; translate-quoted-hash-s-expr : (Hashof Stx Stx) -> S-Expr
(define (translate-quoted-hash-s-expr stuff)
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
               (list (translate-quoted-s-expr k)
                     (translate-quoted-s-expr v)))))
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
(define (translate-quoted-hash-set-s-expr stuff)
  (define set-proc-args (set->list stuff))
  (cond
    [(set-equal? stuff)
     (list* 'set set-proc-args)]
    [(set-eqv? stuff)
     (list* 'seteqv set-proc-args)]
    [(set-eq? stuff)
     (list* 'seteq set-proc-args)]
    [else '....]))

;; translate-quoted-prefab-struct-s-expr : Prefab-Struct -> S-Expr
(define (translate-quoted-prefab-struct-s-expr stuff)
  (list* 'make-prefab-struct
         (translate-quoted-s-expr (prefab-struct-key stuff))
         (map translate-quoted-s-expr (struct->list stuff))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define t translate-quoted-s-expr)

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
