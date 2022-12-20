#lang racket #| CSC324: Stream Implementation |#

(provide s-null
         s-null?
         s-cons
         s-first
         s-rest
         s-range
         s-take
         make-stream
         stream->list)


; Empty stream value, and check for empty stream.
(define s-null 's-null)
(define (s-null? stream) (equal? stream s-null))

#|
(s-cons <first> <rest>) -> stream?
  <first>: any/c?
  <rest>: stream?
    E.g., s-null or another s-cons expression).

  Creates a stream whose first value is <first>, and whose other
  items are the ones in <rest>. Unlike a regular list, both <first>
  and <rest> are wrapped in a thunks, delaying their evaluation.

  Note: s-cons is a MACRO, not a function!
|#
(define-syntax s-cons
  (syntax-rules ()
    [(s-cons <first> <rest>)
     (cons (thunk <first>) (thunk <rest>))]))

; These two define the stream-equivalents of "first" and "rest".
; We need to use `car` and `cdr` here for a technical reason that
; isn't important for this course.
(define (s-first stream) ((car stream)))
(define (s-rest stream) ((cdr stream)))


#|
(make-stream <expr> ...) -> stream?
  <expr> ... : any/c?

  Returns a stream containing the given values.
  Note that this is also a macro. (why?)
|#
(define-syntax make-stream
  (syntax-rules ()
    [(make-stream) s-null]
    [(make-stream <first> <rest> ...)
     (s-cons <first> (make-stream <rest> ...))]))

#|
(stream->list stream) -> list?
  stream: stream?

  Returns a list containing the values in this stream.
|#
(define (stream->list stream)
  (if (s-null? stream)
      null
      (cons (s-first stream) (stream->list (s-rest stream)))))


#|
(s-range start end) -> stream?
  start: integer?
  end: integer?

  Returns a stream containing the numbers start, start+1, ..., end-1.
  Returns an empty stream if start >= end.
|#
(define (s-range start end)
  (if (>= start end)
      s-null
      (s-cons start
              (s-range (+ 1 start) end))))


#|
(s-take stream n) -> stream?
  stream: stream?
  n: (and/c integer? (not/c negative?))

  Returns a new stream that contains the first `n` elements of `stream`,
  or all of the elements of `stream` if it has fewer than `n` elements.
|#
(define (s-take stream n)
  (if (equal? n 0)
      s-null
      (s-cons (s-first stream)
              (s-take (s-rest stream) (- n 1)))))

