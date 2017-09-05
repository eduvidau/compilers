#lang racket

(require "interp.rkt")
(provide flatten)

(define (arg? exp)
  (or (symbol? exp) (fixnum? exp)))

(define (simple? exp)
  (or (arg? exp)
      (match exp
        [`(+ ,e1 ,e2) (and (arg? e1) (arg? e2))]
        [`(-  ,e) (arg? e)]
        [`(read) #t])))

(define start
  1)

(define (new-var)
  (let ((x
         (string->symbol (string-append "temp." (number->string start)))))
    (begin
      (set! start (add1 start))
      x)))

;(define (flatten-exp exp assg var)
;  (match exp
;    [`,e #:when (arg? e) (values exp assg var)]
;    [`(+ ,e1 ,e2) (let [(x (new-var))]
;                    (flatten-exp x
;                                 (cons (cons x  `((+ ,e1 ,e2)))  assg)
;                                 `(,x . ,var)))]))
;(define (fun x y z)
;  `(,x ,y ,z))

(define (flatten-exp exp)
  (match exp
    [`,e #:when(arg? e) e]
    [`(+ ,e1 ,e2) #:when(and (arg? e1) (arg? e2)) (let [(x (new-var))]
                                                    `(let [(,x ,exp)]
                                                       ,(flatten-exp x)))]
    [`(- ,e) #:when (arg? e) (let [(x (new-var))]
                               `(let [(,x ,exp)]
                                  ,(flatten-exp x)))]
    [`(read) (let [(x (new-var))]
               `(let [(,x (read))]
                  ,x))]
    [`(let [(,x ,e)] ,b) #:when (simple? e) 
                         `(let [(,x ,e)]
                            ,(flatten-exp b ))]
    [`(+ ,s ,e) #:when (arg? s)
                (bottom-finder (flatten-exp e) `(+ ,s _))]
    [`(+ ,e ,s) #:when (arg? s)
                (bottom-finder (flatten-exp e) `(+ _ ,s))]
    [`(+ ,e1 ,e2) 
     (bottom-finder (flatten-exp e2) `(+ ,e1 _))]
    [`(- ,e) (bottom-finder (flatten-exp e) `(- _))]
    [`(let [(,v ,e)] ,b)
     (bottom-finder (flatten-exp e) `(let [(,v _)]
                                       ,b))]
    ))



(define (bottom-finder let hole)
  (match let
    ;    [`(let [(,x ,e)] ,b) #:when (arg? b) ,,, need to improve
    ;                         (match hole
    ;                           [`(let [(,var _)]
    ;                               ,bb)
    ;                            `(let [(,var ,b)]
    ;                                  ,(flatten-exp bb))])]
    [`(let [(,x ,e)] ,b)
     `(let [(,x ,e)]
        ,(bottom-finder b hole))]
    [`,e #:when (arg? e)
         (match hole
           [`(+ ,s _) (flatten-exp `(+ ,s ,e))]
           [`(+ _ ,s) (flatten-exp `(+ ,e ,s))]
           [`(- _) (flatten-exp `(- ,e))]
           [`(let [(,x _)]
               ,b)
            `(let [(,x ,e)]
               ,(flatten-exp b))])]))

(define (flat->c exp assignments vars)
  (match exp
    [`,e #:when (arg? e) (cons vars (append (reverse assignments) (cons `(return ,e) '())))]
    [`(let [(,x ,e)],b)
     (flat->c b (cons `(assign ,x ,e) assignments) (cons x vars))]))

(define (flatten program)
  (match program
    [`(program ,exp)
     (cons 'program (flat->c (flatten-exp exp) '() '()))]))