#lang racket #| CSC324 Fall 2020: Exercise 6 |# 

;-------------------------------------------------------------------------------
; This expression exports functions so they can be imported into other files.
; Don't change it!
(provide my-or* my-and* lazy-curry Walrus my-class my-class-getter)

(module+ test
  ; Import the testing library
  (require rackunit))

;-------------------------------------------------------------------------------
; * Task 1: Macro Practice *
;-------------------------------------------------------------------------------

#|
(my-or* p ...)
  p ... : booleans

  A generalization of my-or to take in an arbitrary number
  of boolean arguments.
  It should behave the same as the built-in `or` (for boolean arguments).

  This macro should be *recursive* (i.e., use itself in a template).
  We've illustrated the basic pattern-matching forms below.
|#
; this is a pattern based macro
(define-syntax my-or*
  (syntax-rules ()
    [(_)         #f] ; empty element = false
    [(_ a)       a] ; single element = return element
    [(_ a b ...) (if (not a) (my-or* b ...) #t)]))
                ;(if test-expr then-expr else-expr)

#|
(my-and* p ...)
  p ... : booleans

  A generalization of my-and to take in an arbitrary number
  of boolean arguments.
  It should behave the same as the built-in `and` (for boolean arguments).
|#
(define-syntax my-and*
  (syntax-rules ()
    [(_)         #t]
    [(_ a)       a] ; single element
    [(_ a b ...) (if a (my-and* b ...) #f)]))
                ;(if test-expr then-expr else-expr)

#|
(lazy-curry fn args ...)
  fn args ... : procedure

  takes a function and some argument expressions

  The argument returns a new procedure in terms of
  the remaining arguments to the function

  This macro should not evaluate the argument expressions
  until the returned procedure is called.
|#
(define-syntax lazy-curry 
  (syntax-rules ()
    ;[(_ fn)         fn]
    
    [(_ fn args ...)   (lambda xs (apply fn (append (list args ...) xs)))]))

(module+ test
   (define (h x y z) (+ (* x y) z))
   (define g8 (lazy-curry h))
   (test-equal? "g8" (g8 3 4 5) 17))

(module+ test
   (define (f x y z) (+ (* x y) z))
   (define g1 (lazy-curry f 3))
   (define g2 (lazy-curry f 3 4))
   (test-true "g1 and g2 are functions" (and (procedure? g1) (procedure? g2)))
   (test-equal? "g1" (g1 4 5) 17)
   (test-equal? "g2" (g2 5) 17)
  )

;-------------------------------------------------------------------------------
; * Task 2: Working with objects *
;-------------------------------------------------------------------------------

(define-syntax my-class
  (syntax-rules (method)
    [(my-class <class-name>
       (<attr> ...)
       (method (<method-name> <param> ...) <body>)
       ...)
     (define (<class-name> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (quote <attr>)) <attr>]
               ...
               [(equal? msg (quote <method-name>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"])))]))


; TODO define the class Walrus here
; bucket-empty?: returns a boolean
;                describing whether the Walrus’ bucket is empty
;
; same-name? other: take a single parameter of type Walrus,
;                   and checks if the other Walrus has the same name
;
; equal? other: take a single parameter of type Walrus, and checks if
;               the other Walrus has the same name and age.
;               The other Walrus may have a different list of items in its bucket.
;
; catch item: take a single parameter of type string, and returns
;             a new Walrus that as the new item inserted to
;             the beginning of the list of bucket items.
;
; have-birthday: returns a new Walrus that is the same as the old Walrus,
;                but with the age incremented by one, and
;                the string “cake” inserted to the beginning of the list of its bucket items.

(my-class Walrus (name age bucket)
          (method (bucket-empty?) (empty? bucket))
          (method (same-name? other) (equal? name (other 'name)))
          (method (equal? other) (if (equal? name (other 'name)) (equal? age (other 'age)) #f))
          (method (catch item) (Walrus name age (append (list item) bucket)))
          (method (have-birthday) (Walrus name (+ age 1) (append (list "cake") bucket)))
          ) 


(module+ test
  (define alice (Walrus "alice" 4 '()))
  (define bob (Walrus "alice" 4 '()))

  (test-true "alice and bob are both functions"
              (and (procedure? alice) (procedure? bob)))
  (test-true "alice and bob have same names"
              ((alice 'same-name?) bob))
  (test-true "alice and bob have the same age"
              ((alice 'equal?) bob))
  ; TODO: add more tests here!
)

;-------------------------------------------------------------------------------
; * Task 3: Accessor function in `my-class` *
;-------------------------------------------------------------------------------

#|
(my-class-getter <Class> (<attr> ...)
  (method (<method-name> <param> ...) <body>) ...)

  This macro accepts the *exact* same pattern as my-class from above.

  In addition to defining the constructor, my-class-getter defines
  *one new accessor function per attribute of the class*, using the name
  of the attribute as the name of the function.

  Implementation notes:
    - Our starter code has wrapped the `define` from lecture inside a `begin`.
      This is required so that you can add more `define`s after the one for the
      constructor.
|#
(define-syntax my-class-getter
  (syntax-rules (method) ; method = key word
    [(my-class-getter <class-name>
                      (<attr> ...)
                      (method (<method-name> <param> ...) <body>)
                      ...)
     (begin
       (define (<class-name> <attr> ...)
         (lambda (msg)
           (cond [(equal? msg (quote <attr>)) <attr>]
                 ...
                 [(equal? msg (quote <method-name>))
                  (lambda (<param> ...) <body>)]
                 ...
                 [else "Unrecognized message!"])))
       
       (define (<attr> <class-name>)
         (<class-name> (quote <attr>)))
       ...)
     
     ]))
(module+ test
  ; We use `local` to create a local scope for our definitions.
  ; Run these tests when you're ready!
  (local
      [(my-class-getter Point (x y))]
      (test-true "x and y are functions" (and (procedure? x) (procedure? y)))
    
      (test-equal? "test"
                   (let ([w (Point 5 6)])
                     (list (w 'x) (w 'y)))
                   (list 5 6))
    
      (test-equal? "x and y are accessors"
                   (let ([p (Point 2 3)])
                     (list (x p) (y p)))
                   (list 2 3)))
)



