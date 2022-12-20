#lang racket #| CSC324 Fall 2020: Project 2 |#

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide typeof typeo lookupo)

;-------------------------------------------------------------------------------
; * Task 1: A Simple Type Inferencer *
;-------------------------------------------------------------------------------

#|
(typeof expr typeenv)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types

  Returns the type of the expression expr: 'num, 'str, 'bool, 'error
|#
(define/match (typeof expr typeenv)

  ; Constants
  [((? number?) _) 'num]
  [((? string?) _) 'str]
  [((? boolean?) _) 'bool]
  [((list '= a b) _) 'bool] 

  ; Builtins
  [((list '+ a b) _) (if (check-num a b typeenv) 'num 'error)] 
  [((list '- a b) _) (if (check-num a b typeenv) 'num 'error)]
  [((list '* a b) _) (if (check-num a b typeenv) 'num 'error)]
  [((list '/ a b) _) (if (check-num a b typeenv) 'num 'error)]
  
  [((list '> a b) _) (if (check-num a b typeenv) 'bool 'error)]
  [((list '= a b) _) (if (check-num a b typeenv) 'bool 'error)]
  [((list '>= a b) _) (if (check-num a b typeenv) 'bool 'error)]
  
  [((list '++ a b) _) (if (check-str a b typeenv) 'str 'error)]

  [((list '! a) _) (if (eq? 'bool (lookup a typeenv)) 'bool 'error)]
  [((list 'num->str a) _) (if (eq? 'num (lookup a typeenv)) 'str 'error)]
  [((list 'len a) _) (if (eq? 'str (lookup a typeenv)) 'num 'error)]

  ; Function Calls
  [((cons fn args) _) (let* ([fntype (typeof fn typeenv)]
                             [argtypes (typeof (first args) typeenv)]
                             [fnargtypes (if (pair? fntype) (first (first fntype)) fntype)])
                        (if (eq? fnargtypes argtypes) (first (rest fntype)) 'error))]

  [(symbol? _) (lookup expr typeenv)]
  
  ;error
  [(_ _) 'error]
  )

; Helper functions for Task 1

#|
(lookup key alst)
  elem: A key in the association list
  alst: An association list 

  Returns the value corresponding to the first time the key appears in the
  association list, or #f if the key does not exist.

  Examples:
  > (lookup 'b '((a . 3) (b . 4)))
  4
  > (lookup 'b '((a . 3) (b . 4) (b . 5)))
  4
  > (lookup 'c '((a . 3) (b . 4) (b . 5)))
  #f
|#
(define (lookup key lst)
  (cond [(empty? lst) 'error]
        [(equal? key (car (first lst))) (cdr (first lst))]
        [else
         (lookup key (rest lst))]))

; Add your helper functions here
(define (check-num expr1 expr2 typeenv)
  (if (eq? 'num (typeof expr1 typeenv)) (eq? 'num (typeof expr2 typeenv)) #f)
  )

(define (check-str expr1 expr2 typeenv)
  (if (eq? 'str (typeof expr1 typeenv)) (eq? 'str (typeof expr2 typeenv)) #f)
  )

;-------------------------------------------------------------------------------
; * Task 2: A Type Inferencer Relation in miniKanren
;-------------------------------------------------------------------------------

#|
(typeo expr typeenv type)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types
  type: The type of the expression

  The relational form of the `typeof` function
|#
(define (typeo expr env type)
  (conde
   ; constants: numbero, stringo, and boolo are miniKanren builtin relations
   [(numbero expr)
    (== type 'num)]
   [(stringo expr)
    (== type 'str)]
   [(boolo expr)
    (== type 'bool)]
   ; TODO

   ; identifier: symbolo is a miniKanren builtin relation
   [(symbolo expr)
    (not-builtino expr)
    (lookupo env expr type)]

   ; builtins
   [(symbolo expr)
    (builtino expr type)]

   ; function calls
   [(fresh (first rest types btypes rtypes bargs bres)
           (== expr (cons first rest))
           (type-listo expr env types)
           (== types (cons btypes rtypes))
           (== btypes (cons bargs (cons bres '())))
           (conde
            [(== bargs rtypes)
             (== type bres)]
            [(=/= bargs rtypes)
             (== type 'error)]) )]

   ; function definitions
   [(fresh (first rest ids body bin br new-env types restype argtypes fill args)
           (== expr (cons first rest)) ; break down expr to first and rest
           (== first 'lambda) ; function definition we want to check the first thing is a lambda
           (== rest (list ids body)) ; break down the rest of expr to ids and body
           (conde
            [(pairo body) ; first condition: the body is a pair of the lambda definition
             (update-envo ids env new-env) ; returns a new env 
             (== body (cons bin br)) ; bin = builtin
             (typeo bin new-env types) ; assume its a builtin this line would give us a list of types for out builtin
             (== types (list fill restype)) ; only care about restype
             (helpero ids body env argtypes) ; gets a list of argument types for the body
             (== type (list argtypes restype))]
            [(fresh (idtypes newenv)  ; second condition: the body is not a pair of the lambda definition
                    (update-envo ids env newenv) ; create new env
                    (fill-idso ids newenv idtypes) ; fill ids with types
                    (conde ; for the body if its a number, string, boolean, symbol we do the appropriate move
                     [(symbolo body)
                      (fresh (restype)
                             (typeo body newenv restype)
                             (== type (list idtypes restype)))]
                     [(numbero body)
                      (== type (list idtypes 'num))]
                     [(stringo body)
                      (== type (list idtypes 'str))]
                     [(boolo body)
                      (== type (list idtypes 'bool))]))]))]))


; Helper functions for Task 2

#|
(lookupo key alst value)
  elem: A key in the association list
  alst: An association list 
  value: The corresponding value in the association list

  The relational form of the `lookup` function
|#
; For some reason this doesn't work for non-first values
; if we swap the position of lst and key.
(define (lookupo lst key value)
  (fresh (fkey fval rest)
         (== (cons (cons fkey fval) rest) lst)
         (conde ((== key fkey)
                 (== value fval))
                ((=/= key fkey)
                 (lookupo rest key value)))))


; Add your helper functions here
(define (boolo obj)
  (conde
   [(== obj #t)]
   [(== obj #f)]))

(define (appendo xs ys xsys)
  (conde ((== xs '())
          (== ys xsys))
         ((fresh (x xsˆ xsysˆ)
                 (== xs (cons x xsˆ))
                 (== xsys (cons x xsysˆ))
                 (appendo xsˆ ys xsysˆ)))))

(define (conso x xs lst)
  (conde
   [(== lst (cons x xs))]))

(define (pairo obj)
  (fresh (first rest)
         (conso first rest obj)))

(define (type-listo exprs env lst)
  (conde
   [(== exprs '()) (== lst '())]
   [(fresh (fexpr rexprs ftype rtypes)
           (== (cons fexpr rexprs) exprs)
           (== (cons ftype rtypes) lst)
           (conde
            [(symbolo fexpr)
             (typeo fexpr env ftype)
             (type-listo rexprs env rtypes)]
            [(numbero fexpr)
             (== ftype 'num)
             (type-listo rexprs env rtypes)]
            [(stringo fexpr)
             (== ftype 'str)
             (type-listo rexprs env rtypes)]
            [(boolo fexpr)
             (== ftype 'bool)
             (type-listo rexprs env rtypes)]
            [(pairo fexpr)
             (typeo fexpr env ftype)
             (type-listo rexprs env rtypes)]))]))

(define (update-envo ids env new-env)
  (conde
   [(== ids '()) (== new-env env)] ; base case 
   [(fresh (fid rid fp rp type next-env) 
           (== ids (cons fid rid)) ; nreak down ids
           (appendo env (list (cons fid type)) next-env) 
           (update-envo rid next-env new-env))]))

;precond: body is a pair, body doesn't start with a variable 
(define (helpero ids body env types)
  (conde
   [(== ids '()) (== types '())] ; base case
   [(fresh (first rest btypes bargs bres newenv fid rid ft rt)
           (== ids (cons fid rid))
           (== body (cons first rest)) ; body = pair
           (== types (cons ft rt))
           (== btypes (cons bargs (cons bres '())))
           (typeo first env btypes) ; we know what btypes will return
           (typeo first newenv btypes) ; create a new env for any variable that are used in those types
           (typeo body newenv bres) ; we use the new env to evaluate the entire body, should give us bres
           (lookupo newenv fid ft); lookup in the new env to see the id and its type
           (helpero rid body env rt))])) ; recurse with the rest

(define (fill-idso ids env types)
  (conde
   [(== ids '()) (== types '())] ; base case
   [(fresh (fid rids ftype rtypes new-env)
           (== ids (cons fid rids))
           (== types (cons ftype rtypes))
           (update-envo ids env new-env)
           (lookupo new-env fid ftype)
           (fill-idso rids env rtypes))])) 

(define (not-builtino obj)
  (conde
   [(=/= obj '+)]
   [(=/= obj '-)]
   [(=/= obj '*)]
   [(=/= obj '/)]
   [(=/= obj '>)]
   [(=/= obj '=)]
   [(=/= obj '>=)]
   [(=/= obj '++)]
   [(=/= obj 'num->str)]
   [(=/= obj 'len)]
   [(=/= obj '!)]))

(define (builtino obj type)
  (conde
   [(== obj '+)
    (== type '((num num) num))]
   [(== obj '-)
    (== type '((num num) num))]
   [(== obj '*)
    (== type '((num num) num))]
   [(== obj '/)
    (== type '((num num) num))]
   [(== obj '>)
    (== type '((num num) bool))]
   [(== obj '=)
    (== type '((num num) bool))]
   [(== obj '>=)
    (== type '((num num) bool))]
   [(== obj '++)
    (== type '((str str) str))]
   [(== obj 'num->str)
    (== type '((num) str))]
   [(== obj 'len)
    (== type '((str) num))]
   [(== obj '!)
    (== type '((bool) bool))]))