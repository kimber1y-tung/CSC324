#lang racket #| CSC324 Fall 2020: Exercise8 |#

(require (only-in racket/control reset))
(require "stream.rkt")
(require "amb.rkt")

(provide make-change-gen
         solve-with-constraints
         with-constraints-helper
         get-constraints)

;-------------------------------------------------------------------------------
; * Task 1: Making change *
;-------------------------------------------------------------------------------
#|
(make-change n)
  n: A non-negative integer.

  Returns a choice of a list of 1's and 5's whose sum is n. The returned list
  should be in *non-increasing order*, i.e., all 5's should appear before all 1's.

  You might want to lookup the "make-list" function.
|#
(define (make-change n)
  (do/-< (make-change-helper n '())))

; Helper function for `make-change` that uses `-<`
(define (make-change-helper n lst)
  (cond
    ; If n = 0
    [(equal? n 0) '()]

    [(equal? n 1) '(1)]
    [(equal? n 5) '('(5) '(1 1 1 1 1))]

    [(equal? (apply + lst) n) (lst)]

    [else
     (let* ([prod     (apply * lst)]
            [max-next (min n (+ (/ n prod) 1))]
            [next-num (apply -< (range 1 max-next))])
       (make-change-helper n (cons next-num lst)))]))
                      



(module+ test
  (require rackunit)
  (require (only-in racket/control prompt))

  (test-equal? "make-change 0"
               (list->set (stream->list ((make-change 0))))
               (set empty))
  (test-equal? "make-change 1"
               (list->set (stream->list ((make-change 1))))
               (set (list 1))))

#;(module+ test
  (require rackunit)
  (require (only-in racket/control prompt))

  (test-equal? "make-change 0"
               (list->set (stream->list ((make-change 0))))
               (set empty))
  (test-equal? "make-change 1"
               (list->set (stream->list ((make-change 1))))
               (set (list 1)))
  (test-equal? "make-change 5"
               (list->set (stream->list ((make-change 5))))
               (set (list 5) (list 1 1 1 1 1)))
  (test-equal? "make-change 13"
               (list->set (stream->list ((make-change 13))))
               (set (list 5 5 1 1 1)
                    (list 5 1 1 1 1 1 1 1 1)
                    (list 1 1 1 1 1 1 1 1 1 1 1 1 1))))
#|
(make-change-gen coins n)
  coins: A list of distinct positive integers, sorted in decreasing order.
  n: An integer.

  Returns a choice of a list of numbers in `coins` whose sum is `n`.
  The list should be sorted in non-increasing order.
  As before, coin values may be used more than once.

  If no possible combinations in `coins` sums to n (including when n is negative),
  *call (fail)*. (This is useful inside recursive calls.)
  But return an *empty list* if n = 0. These are different cases!

  Notes:
    1. We have started the pattern-matching for you, and strongly recommend building
       off of these patterns.
    2. You might need to have the output choices come out in a different order
       than in `make-change`. That's fine!
|#

(define (make-change-gen coins n)  
  (do/-< (make-change-gen-helper coins n)))

(define (make-coin-options coin n)  
  (let* ([nums (range 0 (+ n 1))]) 
    (make-list (apply -< nums) coin)))  

(define (get-opts lst n)  
  (cond
    [(zero? n) (list)]
    [(empty? lst) (list)]
    [else (let*
              ([first-opts (make-coin-options (first lst) n)] 
               [rest-opts (get-opts (rest lst) n)]
               [res (append first-opts rest-opts)])
            res)]))

(define (make-change-gen-helper lst n) 
   (let* ([opts (get-opts lst n)]
                [choice (-< opts)])
           (if (equal? n (apply + (-< opts))) choice (fail))))





(module+ test
  (require rackunit)
  (test-equal? "make-change-gen (10 3) 13"
               (list->set (stream->list ((make-change-gen (list 10 3) 13))))
               (set (list 10 3)))

  (test-equal? "make-change-gen (5 1) 13"
               (list->set (stream->list ((make-change-gen (list 5 1) 13))))
               (set (list 5 5 1 1 1)
                    (list 5 1 1 1 1 1 1 1 1)
                    (list 1 1 1 1 1 1 1 1 1 1 1 1 1)))

  ; A note about this test. It may seem weird that the expected output is (set)
  ; and not (set 'done). The reason is that `all-choices` returns `empty` when
  ; there are no more choices (see its implementation above).
  (test-equal? "no combinations possible"
               (list->set (stream->list ((make-change-gen (list 10 3) 17))))
               (set)))

;-------------------------------------------------------------------------------
; * Task 2: Sudoku *
;-------------------------------------------------------------------------------

; We model a Sudoku board by a Racket *vector* of length 81, representing a 9x9 grid.
; Rows are stored contiguously; for example, the top row of the board is stored in
; the first 9 elements of the vector.
; Racket vectors are an array-based data structure, and provide constant-time
; indexing (unlike Racket lists).
;
; Each vector element is either a number between 1-9, representing a filled cell,
; or the number 0, representing an *empty* cell.
(define board-size 81)
(define 1-9 (list->set (range 1 10)))  ; The possible numbers.
(define (all-nine? s) (set=? s 1-9))   ; Whether the given set contains all numbers 1-9.

(define (blank? board i) (equal? (vector-ref board i) 0))  ; Whether the given cell is blank.


; Utilities for converting between a vector index and the corresponding row, column,
; and 3x3 subsquare in the Sudoku board. This numbering is all 0-indexed.
; The subsquares are numbered starting with 0 in the top-left corner,
; and increase in index first left-to-right, then top-down.
(define (to-column i) (remainder i 9))
(define (to-row i) (quotient i 9))
(define (to-subsquare i)
  (+ (quotient (to-column i) 3)
     (* 3 (quotient (to-row i) 3))))

(define (same-column? i j) (equal? (to-column i) (to-column j)))
(define (same-row? i j) (equal? (to-row i) (to-row j)))
(define (same-subsquare? i j) (equal? (to-subsquare i) (to-subsquare j)))


; Utilities for getting the set of elements in a column/row/subsquare.
(define (column board i)
  (list->set (map (lambda (j) (vector-ref board (+ i (* 9 j)))) (range 9))))

(define (row board j)
  (list->set (map (lambda (i) (vector-ref board (+ i (* 9 j)))) (range 9))))

(define (subsquare board k)
  (let* ([start-row (* 3 (quotient k 3))]
         [start-col (* 3 (remainder k 3))])
    (list->set
     (map (lambda (i)
            (vector-ref board
                        (+ (+ start-col (remainder i 3))
                           (* 9 (+ start-row (quotient i 3))))))
          (range 9)))))

; Return whether a given Sudoku board is completely solved.
; (Review the rules of Sudoku using the link on the exercise handout.)
(define (solved? board)
  (and
   ; Check columns
   (andmap (lambda (col-num) (all-nine? (column board col-num)))
           (range 9))
   ; Check rows
   (andmap (lambda (row-num) (all-nine? (row board row-num)))
           (range 9))
   ; Check subsquares
   (andmap (lambda (sub-num) (all-nine? (subsquare board sub-num)))
           (range 9))))

; Helper function for doing a non-mutating update of a board,
; analogous to list-set.
; This is pretty memory-inefficient, and is a consequence of some limitations
; of our current choice operator when mutation is concerned!
; We've provided an optional argument to turn on logging.
; This may be useful for debugging purposes, or to see how many steps your
; algorithm is taking.
(define (vector-set vec index new-item [logging #f])
  (if (or (not (integer? index)) (not (integer? new-item)))
      (error (format "Invalid index or new-item given. Index: ~a Choice: ~a" index new-item))
      (void))

  (when logging
    (displayln (format "Index: ~a Choice: ~a" index new-item)))
  
  (let* ([new-vec (make-vector (vector-length vec))])
    (vector-copy! new-vec 0 vec)
    (vector-set! new-vec index new-item)
    new-vec))

; demo board
(define demo-board '#(3 9 6 2 1 4 7 5 8
                      5 4 7 8 3 9 6 2 1
                      1 8 2 6 7 5 9 4 3
                      4 6 9 3 8 7 2 1 5
                      8 5 1 4 6 2 3 9 7
                      2 7 3 9 5 1 4 8 6
                      6 3 4 1 2 8 5 7 9
                      9 1 5 7 4 3 8 6 2
                      7 2 8 5 9 6 1 3 0))

#|
(solve-brute-force board)
  Returns a thunk containing a stream of solutions for the given Sudoku board.
  See `brute-force-helper` for details.
|#
(define (solve-brute-force board)
  (do/-< (brute-force-helper board 0)))

#|
(brute-force-helper board i)
  board: A Sudoku board, in the representation described above.
  i: The current index to consider. Precondition: 0 <= i < 81.

  Considers each board cell one at a time (recurses on `i`).
  Each time it encounters an empty cell, this algorithm creates a 
  *choice point* using `-<` for all 9 numbers that could fill the cell.
  It chooses a number, sets it in the vector, and moves on to the next cell.

  Only when the board is complete does this algorithm check if the board is solved;
  if it isn't, it calls (fail) to backtrack to the last choice point, and tries again.
|#
(define (brute-force-helper board i)
  (cond
    ; If there are no more choices to make, check if the puzzle is actually solved.
    [(>= i (vector-length board))
     (if (solved? board) board (fail))]

    ; If the current cell is occupied, move on to the next cell.
    [(not (blank? board i)) (brute-force-helper board (+ i 1))]

    ; Else, the current cell is empty. Make a new choice!
    [else
     (let* ([choice (-< 1 2 3 4 5 6 7 8 9)]
            [new-board (vector-set board i choice)])
       (brute-force-helper new-board (+ i 1)))]))

#|
`solve-with-constraints` (and its corresponding helper) are almost exactly the same
as `brute-force`. The only difference is in what choices are made; rather than 
using a static set of choices, the choices for each cell are generated dynamically
based on the current contents of the board.

Complete the helper `get-constraints` and then modify `with-constraints-helper` to
replace the (-< 1 2 3 4 5 6 7 8 9) expression. You may find the function `set->list`
helpful. You might also find it easier to call -< like this: (apply -< '(1 2 3)).
(You can change other things as well, although you shouldn't need to change much.)
|#
(define (solve-with-constraints board)
  (do/-< (with-constraints-helper board 0)))

(define (with-constraints-helper board i)
  (cond
    ; If there are no more choices to make, check if the puzzle is actually solved.
    ; (TODO: do you need to modify this?)
    [(>= i (vector-length board))
     (if (solved? board) board (fail))]

    ; If the current cell is occupied, move on to the next cell.
    ; (TODO: do you need to modify this?)
    [(not (blank? board i))
     (with-constraints-helper board (+ i 1))]

    ; Else, the current cell is empty. Make a new choice!
    ; (TODO: do you need to modify this?)
    [else
     (let* ([choice (apply -< (set->list (get-constraints board i)))]
            [new-board (vector-set board i choice)])
       (with-constraints-helper new-board (+ i 1)))]))

#|
(get-constraints board i)
  board: A Sudoku board
  i: integer?
    Precondition: i is the index of an *empty cell* in `board`.

  Returns a set of the possible numbers that can fill the empty cell.
  Starts with all 9 possible numbers, and removes the numbers that are
  in the same row, column or subsquare as the given cell.

  Note: You may assume we'll only test this function for the given precondition
  on `i`. In Task 2 you may find it useful to extend the documented behaviour
  for when `i` refers to an occupied cell in the board.
|#
(define (get-constraints board i)
  (set-subtract (set 0 1 2 3 4 5 6 7 8 9)
                (set-union (set)
                           (column board (to-column i))
                           (row board (to-row i))
                           (subsquare board (to-subsquare i)))))

;-------------------------------------------------------------------------------
; * Sudoku Demos *
;-------------------------------------------------------------------------------
#|
This section includes some code for running your algorithms on actual Sudoku boards.

You can safely ignore all of this code, expect the invocations of the algorithms at
the bottom, which start off commented-out.

We took some puzzles from https://projecteuler.net/problem=96, but added our own
(very easy) puzzle at the front. See p096_sudoku.txt in the starter code.
|#
(module+ main
  (require racket/control)
  ; A puzzle file, and a function to parse it into separate puzzles.
  (define in (open-input-file "p096_sudoku.txt" #:mode 'text))

  ; Get the next puzzle from the file.
  ; Note that this is written in an imperative style; as we'll discuss later
  ; in the course, it's much harder to get away from this style when doing I/O
  ; computations.
  (define (get-next-puzzle)
    ; Check for the header line "Grid XX". If eof is found, we've reached the end of the file.
    (if (eof-object? (read-line in))
        (void)
        (let* ([nested-list-form
                (map
                 (lambda (_)
                   ; This processes a single line, converting it from a 9-letter string into a list of integers.
                   (map (lambda (c) (- (char->integer c) (char->integer #\0)))
                        (string->list (read-line in))))
                 (range 9))])
          (list->vector (apply append nested-list-form)))))

  ; A stream of Sudoku boards.
  (define (puzzle-stream)
    (let* ([puzzle (get-next-puzzle)])
      (if (void? puzzle)
          s-null
          (s-cons puzzle (puzzle-stream)))))
  
  (define all-puzzles (puzzle-stream))
  (define easy (s-first all-puzzles))
  (define harder (s-first (s-rest all-puzzles))))

; Run the brute force algorithm on the "easy" puzzle.
; Note that we call reset-choices! so that it doesn't interfere with subsequent choices.
#;(module+ main
   (define g1 (solve-brute-force easy))
   (next! g1))

; Run the Task 1 algorithm on the "easy" puzzle.
#;(module+ main
   (define g2 (solve-with-constraints easy))
   (next! g2))

; Run the Task 1 algorithm on the "harder" puzzle.
; Constrast this with the test before and after this!
#;(module+ main
   (define g3 (solve-with-constraints harder))
   (next! g3))

