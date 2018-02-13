;; Lab: Heuristic Search
;; CSC 261 
;;
;; File
;;   heuristic.scm
;;
;; Summary
;;   A collection of heuristic algorithms with one helper
;;
;; Provides
;;   (jump-heuristic-main node)
;;   (jump-heuristic-best node)
;;   (jump-heuristic-alt-1 node)
;;   (jump-heuristic-alt-2 node)
;;   (jump-heuristic-alt-3 node)
;;   (jump-heuristic-alt-4 node)
;;   (decide-momentum total traveled m)

;;; Procedure:
;;;   jump-heuristic-main
;;; Parameters:
;;;   node, a node
;;; Purpose:
;;;   Give a heuristic for the jump problem
;;; Produces:
;;;   val, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   val must be generally higher for worse choices, lower for better choices
(define jump-heuristic-main
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (- total traveled (/ (* m (+ m 1)) 2)))))

;;; Procedure:
;;;   jump-heuristic-best
;;; Parameters:
;;;   node, a node
;;; Purpose:
;;;   Give a heuristic for the jump problem
;;; Produces:
;;;   val, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   val must be generally higher for worse choices, lower for better choices
(define jump-heuristic-best
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (expt (- total traveled (/ (* m (+ m 1)) 2)) 2))))

;;; Procedure:
;;;   jump-heuristic-alt-1
;;; Parameters:
;;;   node, a node
;;; Purpose:
;;;   Give a heuristic for the jump problem
;;; Produces:
;;;   val, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   val must be generally higher for worse choices, lower for better choices
(define jump-heuristic-alt-1
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (- total traveled m))))

;;; Procedure:
;;;   jump-heuristic-alt-2
;;; Parameters:
;;;   node, a node
;;; Purpose:
;;;   Give a heuristic for the jump problem
;;; Produces:
;;;   val, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   val must be generally higher for worse choices, lower for better choices
(define jump-heuristic-alt-2
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (- total traveled (expt m 2)))))

;;; Procedure:
;;;   jump-heuristic-alt-3
;;; Parameters:
;;;   node, a node
;;; Purpose:
;;;   Give a heuristic for the jump problem
;;; Produces:
;;;   val, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   val must be generally higher for worse choices, lower for better choices
(define jump-heuristic-alt-3
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (cond
        [(decide-momentum total traveled (+ m 2)) (+ m 2)]
        [(decide-momentum total traveled (+ m 1)) (+ m 1)]
        [(decide-momentum total traveled m) m]
        [else (- m 1)]))))

;;; Procedure:
;;;   jump-heuristic-alt-4
;;; Parameters:
;;;   node, a node
;;; Purpose:
;;;   Give a heuristic for the jump problem
;;; Produces:
;;;   val, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   val must be generally higher for worse choices, lower for better choices
(define jump-heuristic-alt-4
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (cond
        [(decide-momentum total traveled (+ m 2))
         (expt (- total traveled (/ (* (+ m 3) (+ m 2)) 2)) 2)]
        [(decide-momentum total traveled (+ m 1))
         (expt (- total traveled (/ (* (+ m 2) (+ m 1)) 2)) 2)]
        [(decide-momentum total traveled m)
         (expt (- total traveled (/ (* (+ m 1) m) 2)) 2)]
        [else
         (expt (- total traveled (/ (* m (- m 1)) 2)) 2)]))))

;;; Procedure:
;;;   decide-momentum
;;; Parameters:
;;;   total, an integer
;;;   traveled, an integer
;;;   m, an integer
;;; Purpose:
;;;   Find whether the calculation desired gives a positive (#t) or negative (#f) value.
;;; Produces:
;;;   result, a boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]
(define decide-momentum
  (lambda (total traveled m)
    (< 0 (- total traveljump-heuristiced (/ (* (+ m 1) m) 2)))))