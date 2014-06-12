#lang racket

;; Creates an outcome for the game with the title 'title, 
;; and initial conditions 'conditions, with the 'outcome.
;; The initial conditions include % free, % dead, %enslaved,
;; % land taken. Shekels will be implemented later.
(define make-outcome cons)
(define title car)
(define conditions cdr)

;; Make a list of conditions with %free, %dead, %enslaved and
;; %land taken in that order.
(define make-conditions list)
 
;; The list of possible outcomes
(define outcomes (list (make-outcome "City Razed"
                                     (make-conditions 0   1   0   1  ))
                       (make-outcome "City Enslaved"
                                     (make-conditions 0   0.2 0.8 1  ))
                       (make-outcome "City Looted"
                                     (make-conditions 0.6 0.2 0.2 0.6))
                       (make-outcome "City Becomes Vassal"
                                     (make-conditions 1   0   0   1  ))
                       (make-outcome "City Paid Tribute"
                                     (make-conditions 1   0   0   0  ))))

;; Finds the closest possible outcome given the initial conditions
(define (ending end-conditions outcome-list)
  (define (distance v1 v2)
    (sqrt (foldl + 0 (map sqr (map - v1 v2)))))
  (foldl (lambda (x y)
           (let ((xd (distance (conditions x) end-conditions))
                 (yd (distance (conditions y) end-conditions)))
             (if (> xd yd) x y)))
         (car outcome-list)
         (cdr outcome-list)))

;; A quick demonstration of the ending chooser
(define (random-conditions)
  (make-conditions (random) (random) (random) (random)))

(define test-list (list (random-conditions)
                        (random-conditions)
                        (random-conditions)
                        (random-conditions)
                        (random-conditions)
                        (random-conditions)
                        (random-conditions)
                        (random-conditions)))

(display test-list)
(newline)
(display "becomes this:")
(newline)
(display (map (lambda (x) (ending x outcomes)) test-list))
