#lang racket #| CSC324 Fall 2020: Exercise8 |#

(require "mk.rkt")
(provide evalo)

#|
(evalo expr env val)
  expr: An expression 
  env:  The environment, represented as an association list
  val:  The value of the expression

  This is a relational interpreter for a small, Racket-like language
  consisting of:
    * quoted literals
    * the "list" primitive
    * identifiers
    * function expressions
    * function application

  This relation holds if `expr` evaluates to `val` under the environment `env`.

  This relational interpreter is based on Byrd et al. (2017) A Unified Approach
  to Solving Seven Programming Problems (Functional Pearl)
|#
(define (evalo expr env val) 
  (conde
    ((fresh (v)                           ; literal values
       (== `(quote ,v) expr)
       (absento 'closure v)     
       (== v val)))
    ((fresh (a*)                          ; the "list" primitive
       (== `(list . ,a*) expr)
       (absento 'closure a*)
       (proper-listo a* env val)))
    ((symbolo expr) (lookupo expr env val)) ; identifiers
    ((fresh (rator rand x body env^ a)    ; function calls
       (== `(,rator ,rand) expr)
       (evalo rator env `(closure ,x ,body ,env^))
       (evalo rand env a)
       (evalo body `((,x . ,a) . ,env^) val)))
    ((fresh (x body)                      ; function expressions
       (== `(lambda (,x) ,body) expr)
       (symbolo x)
       (== `(closure ,x ,body ,env) val)))))

#|
(proper-listo expr env val)
  expr: An expression 
  env:  The environment, represented as an association list
  val:  The value of the expression

  Helper function for the relational interpreter used to compute the
  value of a list expression.
|#
(define (proper-listo expr env val)
    (conde
      ((== '() expr)
       (== '() val))
      ((fresh (a d t-a t-d)
         (== `(,a . ,d) expr)
         (== `(,t-a . ,t-d) val)
         (evalo a env t-a)
         (proper-listo d env t-d)))))

#|
(lookupo key assoc value)
  key:   A term representing a key in the association list 
  assoc: An association list.
  value: A term representing a value in the association list 

  The relation holds if (key . value) is an element in the
  association list.
|#
(define (lookupo key assoc value)
  (fresh (fkey fval rest)
    (== (cons (cons fkey fval) rest) assoc)
    (conde ((== key fkey)
            (== value fval))
           ((=/= key fkey)
            (lookupo key rest value)))))


