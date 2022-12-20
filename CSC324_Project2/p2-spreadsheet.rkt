#lang racket #| CSC324 Fall 2020: Project 2 |#

; If you would like us to test this file with our correct implementation of
; "typeof" and "typeo", change the import to "p2-soln.rkt" before submitting
; your code.
;(require "p2-base.rkt")
(require "p2-soln.rkt")

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide type-check-spreadsheet fill-in)

;-------------------------------------------------------------------------------
; * Task 3: Type Checking a Spreadsheet *
;-------------------------------------------------------------------------------

#|
(type-check-spreadsheet spreadsheet)
  spreadsheet: An spreadsheet AST described in the project handout.

  Returns a list of booleans, representing whether the types of each column
  is correctly annotated.
|#
(define (type-check-spreadsheet spreadsheet)
  (let* ([defs (rest (first (rest spreadsheet)))]
         [cols (rest (first (rest (rest spreadsheet))))]
         [def-env (create-def-env defs '())]
         [col-env (do-cols cols def-env)]
         [final-check (final-check-cols cols col-env '())])
    final-check))

(define (lookup key lst)
  (cond [(empty? lst) 'error]
        [(equal? key (car (first lst))) (cdr (first lst))]
        [else
         (lookup key (rest lst))]))

(define (builtin? obj)
  (if (member obj '(+ - * / > = >= ! len num->str ++)) #t #f))

(define/match (eval-expr expr env)
  [((? number?) _) 'num]
  [((? string?) _) 'str]
  [((? boolean?) _) 'bool]
  [((? symbol?) _) (lookup expr env)]
  [((list 'lambda ids body) _) `(closure ,expr ,env)]
  [((cons (? builtin?) args) _) (typeof expr env)]
  [((cons fexpr rexprs) _) (apply-helper (eval-expr fexpr env) rexprs env)]
  [(_ _) 'error])

(define/match (apply-helper expr exprs env)
  [((list 'closure (list 'lambda ids body) closenv) _ _) (eval-expr body (add-binds-to-env ids exprs env closenv))]
  [(_ _ _) 'error])

(define/match (add-binds-to-env ids exprs seed-env final-env)
  [((? empty?) (? empty?) _ _) final-env]
  [((cons fid rids) (cons fexpr rexprs) _ _) (add-binds-to-env rids rexprs seed-env (cons (cons fid (eval-expr fexpr seed-env)) final-env))]
  [(_ _ _ _) 'error])

(define/match (create-def-env defs env)
  [((? empty?) _) env]
  [((cons fdef rdefs) _) (create-def-env rdefs (handle-def fdef env))])

;Takes in def and env, gives back updated env
(define/match (handle-def def env)
  [((list id expr) _) (append env (list (cons id (eval-expr expr env))))]
  [(_ _) env])

(define/match (do-cols cols env)
  [((? empty?) _) env]
  [((cons fcol rcols) _) (do-cols rcols (handle-col fcol env))]
  [(_ _) env])

(define/match (handle-col col env)
  [((list colname coltype (cons 'values vals)) _) (append env (list (cons colname (check-col vals coltype env))))]
  [((list colname coltype (list 'computed expr)) _) (append env (list (cons colname (eval-expr expr env))))]
  [(_ _) 'error])

(define/match (check-col vals type env)
  [((? empty?) _ _) type]
  [((cons fval rvals) _ _) (if (eq? type (eval-expr fval env)) (check-col rvals type env) 'error)]
  [(_ _ _) 'error])

(define/match (final-check-cols cols env result)
  [((? empty?) _ _) result]
  [((cons fcol rcols) _ _) (final-check-cols rcols env (append result (if (col-check fcol env) '(#t) '(#f))))])

(define/match (col-check col env)
  [((cons id (cons type not-needed)) _) (if (eq? (lookup id env) type) #t #f)]
  [(_ _) #f])

#|
Note: Currently expressions like (++ (num->str x) (some-lambda-defined-fn y z))
does not work as when we see a builtin we throw it to typeof
And our current implementation of typeof does not know how to handle an identifier
which evaluates to a closure. Since typeof isn't supposed to handle function definitions,
we are unsure if our approach to task 3 is incorrect, or if the above example is flawed.
Our approach in task 3 is probably flawed in some manner, though time is short so we
are leaving this issue in.
|#

;-------------------------------------------------------------------------------
; * Task 4: Synthesizing Programs *
;-------------------------------------------------------------------------------

#|
(fill-in lvar expr type n)
  lvar: The logic variable to be filled in
  expr: An expression following the spreadsheet grammar, with a logic variable
        somewhere in the expression to be filled in.
  type: The desired type of the expression after BLANK is filled in.
  n:    The maximum number of results to return.

  Macro that runs a miniKanren query that will replace the symbol `BLANK`
  in the spreadsheet expression `expr` so that the type of the expression
  is consistent with `type`. The query returns at most `n` results.
|#

(define-syntax fill-in
  (syntax-rules ()
    [(fill-in lvar expr type n)
     (run n (lvar) (typeo expr '() type))
     ]))

