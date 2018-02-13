;; Lab: Heuristic Search
;; CSC 261 
;;
;; File
;;   driver.scm
;;
;; Summary
;;   A collection of tests for our A* search.
;;
;; Provides
;;   (test-astar)
;;   (run-astar-main-test values)
;;   (run-astar-best-test values)
;;   (run-ids-test values)

;; the last three functions are for running the tests to compare expansions of IDS,
;; astar-main and astar-best functions over different values for last part of the lab. 

(require "search.scm")
(require "problem.scm")
(require "sort.scm")
(require "node.scm")
(require "jump.scm")
(require rackunit)
(require rackunit/text-ui)
(load "heuristic.scm")
(load "astar.scm")



;; Sample test, run test on multiple lists of varying length and make sure that
;; they return the right values.

(define test-astar
  (test-suite
   "Tests of astar"
   (test-case
    "small problems"
    (let ([sol (astar-search (jump-start-state 10) (jump-problem 10) jump-heuristic-main)])
      (check-equal? (equal? #f sol)
                     #f
                     "works on smallest problem")))))

(run-tests test-astar)

;; list of values increasing by a power of 10 from 10 to 1,000,000,000
(define vals (list 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000))

;; function definitions 
(define run-astar-main-test
  (lambda(values)
    (display "hi") ;; print result of running search on car values 
    (if (equal? values '())
        "Done"
        (run-astar-main-test (cdr values)))))

(define run-astar-best-test
  (lambda(values)
    (display "hi") ;; print result of running search on car values 
    (if (equal? values '())
        "Done"
        (run-astar-main-test (cdr values)))))

(define run-ids-test
  (lambda(values)
    (display "hi") ;; print result of running search on car values 
    (if (equal? values '())
        "Done"
        (run-astar-main-test (cdr values)))))

