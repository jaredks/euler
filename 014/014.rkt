#lang racket

(define cache (make-hash (list (cons 1 1))))

(define (collatz n)
  (or (hash-ref cache n #f) (hash-ref! cache n (add1 (collatz (if (even? n) (/ n 2) (+ (* 3 n) 1)))))))

(define (prob14 n)
  (define (helper number index max-number max-index)
    (if (= index n) max-index
        (if (> number max-number) (helper (collatz (add1 index)) (add1 index) number index)
            (helper (collatz (add1 index)) (add1 index) max-number max-index))))
  (helper 0 0 0 0))

(prob14 (expt 10 6))