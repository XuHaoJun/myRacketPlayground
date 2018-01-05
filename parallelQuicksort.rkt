#lang racket

(define quicksort
  (lambda (lst)
    (if
     (< (length lst) 2)
     lst
     (let* (
            [pivot
             (list-ref lst (random 0 (length lst)))]
            [left (quicksort (filter (lambda (n) (< n pivot)) lst))]
            [middle
             (filter (lambda (n) (= n pivot)) lst)]
            [right
             (quicksort (filter (lambda (n) (> n pivot)) lst))]
            )
       (append
        left
        middle
        right)))))

(define parallelQuicksort
  (lambda (lst)
    (if
     (< (length lst) 2)
     lst
     (let* (
            [pivot
             (list-ref lst (random 0 (length lst)))]
            [left (future (lambda () (filter (lambda (n) (< n pivot)) lst)))]
            [middle
             (future (lambda () (filter (lambda (n) (= n pivot)) lst)))]
            [right
             (future (lambda () (filter (lambda (n) (> n pivot)) lst)))]
            )
       (append
        (parallelQuicksort (touch left))
        (touch middle)
        (parallelQuicksort (touch right)))))))

(define lst (for/list ([n (in-range 10000)])
              (random 0 (* (+ n 1) 10))))


(time(parallelQuicksort lst))
