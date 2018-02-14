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
;;   (test-astar-main)
;;   (test-astar-best)
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


;; TEST MAIN HEURISTIC ON VALUES (1 10 100 400)
(define test-astar-main
  (test-suite
   "Tests of astar"
    (test-case
    "smallest problem"
    (let ([sol (astar-search (jump-start-state 1) (jump-problem 1) jump-heuristic-main)])
      (check-equal?  sol
                     '((1) 1)
                     "works on smallest problem")))(test-case
    "small problem"
    (let ([sol (astar-search (jump-start-state 10) (jump-problem 10) jump-heuristic-main)])
      (check-equal?  sol
                     '((1 3 3 2 1) 13)
                     "works on smallest problem")))
      (test-case
    "medium problem"
    (let ([sol (astar-search (jump-start-state 100) (jump-problem 100) jump-heuristic-main)])
      (check-equal?  sol
                     '((1 3 5 7 9 11 10 9 9 8 7 6 5 4 3 2 1) 4413)
                     "works on medium problem")))
         (test-case
    "long problem"
    (let ([sol (astar-search (jump-start-state 400) (jump-problem 400) jump-heuristic-main)])
      (check-equal?  sol
                     '((1 3 5 7 9 11 13 15 17 19 21 22 21 20 19 18 17 17 16 15 14 13 12 11
                        10 9 9 8 7 6 5 4 3 2 1) 6101157)
                     "works on large problem")))))

;; TEST MAIN HEURISTIC ON VALUES (1 10 100 400)
(define test-astar-best
  (test-suite
   "Tests of astar"
    (test-case
    "smallest problem"
    (let ([sol (astar-search (jump-start-state 1) (jump-problem 1) jump-heuristic-best)])
      (check-equal?  sol
                     '((1) 1)
                     "works on smallest problem")))(test-case
    "small problem"
    (let ([sol (astar-search (jump-start-state 10) (jump-problem 10) jump-heuristic-best)])
      (check-equal?  sol
                     '((1 3 2 2 1 1) 6)
                     "works on smallest problem")))
      (test-case
    "medium problem"
    (let ([sol (astar-search (jump-start-state 100) (jump-problem 100) jump-heuristic-best)])
      (check-equal?  sol
                     '((1 3 5 7 9 11 10 9 8 7 6 6 5 4 3 2 2 1 1) 19)
                     "works on medium problem")))
         (test-case
    "long problem"
    (let ([sol (astar-search (jump-start-state 400) (jump-problem 400) jump-heuristic-best)])
      (check-equal?  sol
                     '((1 3 5 7 9 11 13 15 17 19 21 22 21 20 19 18 17 17 16 15 14 13 12 11 10 9 8 7
                        6 6 5 4 3 2 2 1 1) 37)
                     "works on large problem")))))

;; RUN TESTS
(display "Testing Main A* Heuristic . . .") (newline)
(run-tests test-astar-main) (newline)
(display "Testing Best A* Heuristic . . .") (newline)
(run-tests test-astar-best) (newline)


;; CODE FOR SIDE BY SIDE DATA COLLECTION/COMPARISON
(define side-by-side
  (lambda (n)
    (display "Comparison of IDS and A* with n = ")
    (display n)
    (display "\n\n")
    
    (display "IDS:\n")
    (display (iterative-deepening-search (jump-start-state n) (jump-problem n)))
    (display "\n\n")
    
    (display "A*, heuristic 1:\n")
    (display (astar-search (jump-start-state n) (jump-problem n) jump-heuristic-main))
    (display "\n\n")

    (display "A*, heuristic 2:\n")
    (display (astar-search (jump-start-state n) (jump-problem n) jump-heuristic-best))
    (display "\n\n")
    (display "------------------------------------------\n\n")))

(define astar-side-by-side
  (lambda (n)
    (display "Comparison of A* with n = ")
    (display n)
    (display "\n\n")

    (display "A*, heuristic 1:\n")
    (display (astar-search (jump-start-state n) (jump-problem n) jump-heuristic-main))
    (display "\n\n")

    (display "A*, heuristic 2:\n")
    (display (astar-search (jump-start-state n) (jump-problem n) jump-heuristic-best))
    (display "\n\n")
    (display "------------------------------------------\n\n")))