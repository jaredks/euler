#lang racket

(define (prob1 n acc)
  (if (= n 1000) acc (prob1 (+ n 1) (if (or (= (modulo n 3) 0) (= (modulo n 5) 0)) (+ n acc) acc))))
(prob1 1 0)