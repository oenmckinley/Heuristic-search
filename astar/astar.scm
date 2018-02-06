(require "search.scm")
(require "problem.scm")
(require "sort.scm")
(require "node.scm")
(require "jump.scm")
(require "jump-heuristic.scm")
(load "8puzzle.scm")

(define eight-puzzle (eight-puzzle-problem))

(define zero-fun (lambda (x) 0))

(define eight-puzzle-state (random-eight-puzzle-state 10))

(define heuristic-func
  (lambda(node)
    (eight-puzzle-misplaced node)))

(define astar
  (lambda(start-state problem)
    (search
     start-state
     problem
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
                 (insert remaining (cdr queue))))))))
     ;; write node-total-cost heuristic here
     heuristic-func)))

;;<blink> MAKE SURE TO DOCUMENT THE UCF SEARCH FROM THE PREVIOUS LAB <blink>

(display eight-puzzle-state)
(astar eight-puzzle-state eight-puzzle)