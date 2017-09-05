#lang racket
(require racket/fixnum)
(require "interp.rkt")
(provide uniquify)

;pre-uniquify: R1 -> R1
(define (uniquify p)
  (match p
    [`(program ,exp) `(program ,(uniq-exp exp '()))]
    [else (error "not an R1 program")]))

;uniq: Exp -> Exp
(define (uniq-exp exp env)
  (match exp
    [`,n #:when (fixnum? n) n]
    [`,v #:when (symbol? v) (look-up v env)]
    ['(read) exp]
    [`(- ,e) `(- ,(uniq-exp e env))]
    [`(+ ,e1 ,e2) `(+ ,(uniq-exp e1 env) ,(uniq-exp e2 env))]
    [`(let [(,v ,e1)] ,e2)
     (let [(n (gensym v))]
       `(let [(,n ,(uniq-exp e1 env))]
          ,(uniq-exp e2 (cons `(,v . ,n) env))))]))

(define (look-up var env)
  (cond
    [(null? env) (error "var not found")]
    [(equal? (caar env) var) (cdar env)]))
     

    




  