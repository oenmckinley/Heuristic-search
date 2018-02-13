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

(define astar-search
  (lambda(start-state problem heuristic)
    (search
     start-state
     problem
     enqueue-func
     heuristic)))

