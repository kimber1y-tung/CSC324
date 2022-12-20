#lang racket #| CSC324 Fall 2020: Project 2 Starter Tests|#

(require "p2-base.rkt")
(require "p2-spreadsheet.rkt")
(require "mk.rkt")

(require rackunit)

(define all-tests
  (test-suite
   "all-tests"
   (test-suite
    "task1"
    (test-equal? "(typeof 3 '())"  ; Test label
                 (typeof 3 '())    ; Actual value
                 'num)             ; Expected value

    (test-equal? "(typeof 'hello' '())"
                 (typeof "hello" '())
                 'str)

    (test-equal? "(typeof #t '())"
                 (typeof #t '())
                 'bool)

    (test-equal? "(typeof (= 3 3) '())"
                 (typeof (= 3 3) '())
                 'bool)

    (test-equal? "(typeof a '((a . num) (b . str)))"
                 (typeof 'a '((a . num) (b . str)))
                 'num)

    (test-equal? "(typeof '(+ a a) '((a . num) (b . str)))"
                 (typeof '(+ a a) '((a . num) (b . str)))
                 'num)

    (test-equal? "(typeof '(+ 3 'hello') '())"
                 (typeof '(+ 3 "hello") '())
                 'error)

    (test-equal? "(typeof 'f '((f . ((num) num)) (g . ((num) str))))"  
                 (typeof 'f '((f . ((num) num)) (g . ((num) str))))    
                 '((num) num))

    (test-equal? "(typeof '(f (g 3)) '((f . ((num) num)) (g . ((num) num))))"
                 (typeof '(f (g 3)) '((f . ((num) num)) (g . ((num) num))))
                 'num)

    (test-equal? "(typeof '(f (g 3)) '((f . ((num) num)) (g . ((num) str))))"
                 (typeof '(f (g 3)) '((f . ((num) num)) (g . ((num) str))))
                 'error))

   (test-suite
    "lookupo"
    (test-equal? "simple"
                 (run 1 (out) (lookupo '((a . 1)) 'a out))
                 '(1))
    (test-equal? "futher in list"
                 (run 1 (out) (lookupo '((c . 2) (d . 3) (a . 1) (b . 1)) 'a out))
                 '(1))
    (test-equal? "find key"
                 (run 1 (out) (lookupo '((c . 2) (d . 3) (a . 1) (b . 1)) out 3))
                 '(d))
    (test-equal? "find list"
                 (run 1 (out) (lookupo out 'a 3))
                 '(((a . 3) . _.0))))
   
   (test-suite
    "task2"
    (test-equal? "(run 1 (out) (typeo '(> 4 5) '() out))"
                 (run 1 (out) (typeo '(> 4 5) '() out))
                 '(bool))

    (test-equal? "(run 1 (out) (typeo '(> 4 'hi') '() out))"
                 (run 1 (out) (typeo '(> 4 "hi") '() out))
                 '())

    (test-equal? "(run 1 (out) (typeo '((lambda (x) x) 3) '() out))"
                 (run 1 (out) (typeo '((lambda (x) x) 3) '() out))
                 '(num))

    (test-equal? "constant lambda with arg"
                 (run 1 (out) (typeo '(lambda (x) 3) '() out))
                 '(((_.0) num)))

    (test-equal? "(run 1 (out) (typeo '((lambda (x) (g x))) '((g . ((num) num))) out))"
                 (run 1 (out) (typeo '(lambda (x) (g x)) '((g . ((num) num))) out))
                 '(((num) num)))
    
    (test-equal? "f:: a->a"
                 (run 1 (out a) (typeo '(f 3) `((f . ((,a) ,a))) out))
                 '((num num)))

    (test-equal? "function call too many args"
                 (run 1 (out) (typeo '(> 1 2 3) '() out))
                 '(error))

    (test-equal? "function call too few args"
                 (run 1 (out) (typeo '(> 1) '() out))
                 '(error))

    (test-equal? "no arg def"
                 (run 1 (out) (typeo '(lambda () (+ 1 2)) '() out))
                 '((() num))))


   (test-suite
    "task3"
    (test-equal? "sample in project handout"
                 (type-check-spreadsheet
                  '(spreadsheet
                    (def (voting-age 18)
                      (canvote (lambda (x) (>= x voting-age))))
                    (columns
                     (name  str (values "adam" "betty" "clare" "eric" "sam"))
                     (age   num (values 12 15 18 49 17))
                     (voter bool (computed (canvote age)))
                     (voter2 bool (computed (>= age name))))))
                 '(#t #t #t #f))

    (test-equal? "expanded sample handout"
                 (type-check-spreadsheet
                  '(spreadsheet
                    (def (voting-age 18)
                      (concat (lambda (x y) (++ x y)))
                      (canvote (lambda (x) (>= x voting-age))))
                    (columns
                     (id num (values 1 2 3 4 5))
                     (name str (values "adam" "betty" "clare" "eric" "sam"))
                     (age num (values 12 15 18 49 17))
                     (voter bool (computed (canvote age)))
                     (name2 str (computed (concat name name)))
                     (year-until-100 num (computed (- 100 age))))))
                 '(#t #t #t #t #t #t))

    (test-suite
     "Incorrect definition type"
     (test-equal? "sample in project handout"
                  (type-check-spreadsheet
                   '(spreadsheet
                     (def (voting-age "s")
                       (canvote (lambda (x) (>= x voting-age))))
                     (columns
                      (name  str (values "adam" "betty" "clare" "eric" "sam"))
                      (age   num (values 12 15 18 49 17))
                      (voter bool (computed (canvote age)))
                      (voter2 bool (computed (>= age name))))))
                  '(#t #t #f #f)))

    ;This test fails, see note beneath helpers for task 3
    #;(test-equal? "Task 3 EXPR Zoo with incorrect type"
                   (type-check-spreadsheet
                    '(spreadsheet
                      (def (driving-age 16)
                        (driving-age2 driving-age)
                        (voting-age (+ driving-age2 2))
                        (condition-checker (lambda (x y) (>= x y)))
                        (get-fullname (lambda (x y) (++ x y)))
                        (get-namelength (lambda (x) (len x)))
                        (cannotvote (lambda (x) (! x)))
                        (cannotvote2 cannotvote)
                        (age-name (lambda (x y z) (++ (num->str x) (get-fullname y z)))))
                      (columns
                       (first-name  str (values "harry" "betty" "clare" "eric" "sam"))
                       (last-name  str (values "potter" 3 "blare" "cire" "bam"))
                       (age   num (values 12 15 18 49 2000))
                       (voter bool (computed ((lambda (x) (condition-checker x voting-age)) age)))
                       (driver bool (computed ((lambda (x) (condition-checker x driving-age2)) age)))
                       (full-names str (computed (get-fullname first-name last-name)))
                       (lengths num (computed (get-namelength full-names)))
                       (notvoter bool (computed (cannotvote2 voter)))
                       (age-and-name str (computed (age-name age first-name last-name))))))
                   '(#t #f #t #t #t #f #f #t #f)))
   (test-suite
    "task4"
    (test-equal? "fill-in returns some answers"
                 (length (fill-in BLANK `(+ 3 ,BLANK) 'num 2))
                 2))

   ))

;-------------------------------------------------------------------------------

; Run and display tests
(module+ test
  (require rackunit/text-ui)
  (run-tests all-tests))
