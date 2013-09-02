#lang racket

(define (prob2 n)
  (define (helper a b acc)
    (if (> a n) acc (helper b (+ a b) (if (even? a) (+ acc a) acc))))
  (helper 1 2 0))

(prob2 4000000)