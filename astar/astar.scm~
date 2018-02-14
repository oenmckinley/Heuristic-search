;; Lab: Heuristic Search
;; CSC 261 
;;
;; File
;;   astar.scm
;;
;; Summary
;;   A collection of functions used to implement A* search 
;;
;; Provides
;;   (enqueue-func new-nodes sorted-queue)
;;   (astar-search start-state problem heuristic)

(require "search.scm")
(require "problem.scm")
(require "sort.scm")
(require "node.scm")

;;; Procedure:
;;;   enqueue-func
;;; Parameters:
;;;   new-nodes, a list of nodes
;;;   sorted-queue, a queue of nodes
;;; Purpose:
;;;   To enqueue the new nodes into the queue in an order that is condusive to A* search.
;;; Produces:
;;;   void
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   All nodes from new-nodes should have been transfered to sorted-queue, in the desired order.
(define enqueue-func
     ;; write enqueue method here
     (lambda(new-nodes sorted-queue)
              ;; Sort the new new nodes according to path cost
       (let ((sorted-children (list-keyed-insertion-sort
			       new-nodes
			       node-total-cost
			       <=)))
	 ;; Insert the new nodes efficiently into the already-sorted frontier
	 (let insert ((remaining sorted-children) ;; Items to insert
		      (queue sorted-queue))       ;; List of sorted nodes
	   (cond
	    ((null? remaining) ;; Nothing left to insert?
	     queue) ;; Return the queue (which is sorted)
	    ((null? queue) ;; If the queue is empty, we can simply return the
           remaining)      ;; remaining items, because they're already sorted
          ((< (node-total-cost (car remaining))  ;; Compare path costs
              (node-total-cost (car queue)))  
           (cons (car remaining) ;; List of the first remaining item and the
                 (insert (cdr remaining) queue))) ;;  rest inserted into queue
          (else
           (cons (car queue) ;; List of the queue front and insert remaining
                 (insert remaining (cdr queue)))))))))

;;; Procedure:
;;;   astar-search
;;; Parameters:
;;;   start-state, a state
;;;   problem, a problem
;;;   heuristic, a heuristic for search
;;; Purpose:
;;;   To use the procedure search with enqueue-func and a heuristic to perform A* search.
;;; Produces:
;;;   result, a list with a list of actions taken and the number of nodes expanded
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]
(define astar-search
  (lambda(start-state problem heuristic)
    (search
     start-state
     problem
     enqueue-func
     heuristic)))

