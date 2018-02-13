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
;;;   x, a real number
;;;   y, a real number
;;;   r, a positive real number
;;;   color, a color
;;; Purpose:
;;;   Creates a drawing of a circle of radius r, centered at (x,y).
;;; Produces:
;;;   drawing, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   drawing is an ellipse.  That is (drawing-ellipse? drawing) holds.
;;;   (drawing-left drawing) = (- x r)
;;;   (drawing-top drawing) = (- y r)
;;;   (drawing-width drawing) = (* 2 r)
;;;   (drawing-height drawing) = (* 2 r)
;;;   (drawing-width drawing) = (drawing-height drawing)
;;;   (drawing-color drawing) = color
(define jump-heuristic-main
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (- total traveled (/ (* m (+ m 1)) 2)))))

;;; Procedure:
;;;   circle
;;; Parameters:
;;;   x, a real number
;;;   y, a real number
;;;   r, a positive real number
;;;   color, a color
;;; Purpose:
;;;   Creates a drawing of a circle of radius r, centered at (x,y).
;;; Produces:
;;;   drawing, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   drawing is an ellipse.  That is (drawing-ellipse? drawing) holds.
;;;   (drawing-left drawing) = (- x r)
;;;   (drawing-top drawing) = (- y r)
;;;   (drawing-width drawing) = (* 2 r)
;;;   (drawing-height drawing) = (* 2 r)
;;;   (drawing-width drawing) = (drawing-height drawing)
;;;   (drawing-color drawing) = color
(define jump-heuristic-best
  (lambda(node);;; Procedure:
;;;   circle
;;; Parameters:
;;;   x, a real number
;;;   y, a real number
;;;   r, a positive real number
;;;   color, a color
;;; Purpose:
;;;   Creates a drawing of a circle of radius r, centered at (x,y).
;;; Produces:
;;;   drawing, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   drawing is an ellipse.  That is (drawing-ellipse? drawing) holds.
;;;   (drawing-left drawing) = (- x r)
;;;   (drawing-top drawing) = (- y r)
;;;   (drawing-width drawing) = (* 2 r)
;;;   (drawing-height drawing) = (* 2 r)
;;;   (drawing-width drawing) = (drawing-height drawing)
;;;   (drawing-color drawing) = color
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (expt (- total traveled (/ (* m (+ m 1)) 2)) 2))))

;;; Procedure:
;;;   circle
;;; Parameters:
;;;   x, a real number
;;;   y, a real number
;;;   r, a positive real number
;;;   color, a color
;;; Purpose:
;;;   Creates a drawing of a circle of radius r, centered at (x,y).
;;; Produces:
;;;   drawing, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   drawing is an ellipse.  That is (drawing-ellipse? drawing) holds.
;;;   (drawing-left drawing) = (- x r)
;;;   (drawing-top drawing) = (- y r)
;;;   (drawing-width drawing) = (* 2 r)
;;;   (drawing-height drawing) = (* 2 r)
;;;   (drawing-width drawing) = (drawing-height drawing)
;;;   (drawing-color drawing) = color
(define jump-heuristic-alt-1
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (- total traveled m))))

;;; Procedure:
;;;   circle
;;; Parameters:
;;;   x, a real number
;;;   y, a real number
;;;   r, a positive real number
;;;   color, a color
;;; Purpose:
;;;   Creates a drawing of a circle of radius r, centered at (x,y).
;;; Produces:
;;;   drawing, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   drawing is an ellipse.  That is (drawing-ellipse? drawing) holds.
;;;   (drawing-left drawing) = (- x r)
;;;   (drawing-top drawing) = (- y r)
;;;   (drawing-width drawing) = (* 2 r)
;;;   (drawing-height drawing) = (* 2 r)
;;;   (drawing-width drawing) = (drawing-height drawing)
;;;   (drawing-color drawing) = color
(define jump-heuristic-alt-2
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (- total traveled (expt m 2)))))

;;; Procedure:
;;;   circle
;;; Parameters:
;;;   x, a real number
;;;   y, a real number
;;;   r, a positive real number
;;;   color, a color
;;; Purpose:
;;;   Creates a drawing of a circle of radius r, centered at (x,y).
;;; Produces:
;;;   drawing, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   drawing is an ellipse.  That is (drawing-ellipse? drawing) holds.
;;;   (drawing-left drawing) = (- x r)
;;;   (drawing-top drawing) = (- y r)
;;;   (drawing-width drawing) = (* 2 r)
;;;   (drawing-height drawing) = (* 2 r)
;;;   (drawing-width drawing) = (drawing-height drawing)
;;;   (drawing-color drawing) = color
(define jump-heuristic-alt-3
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (cond
        [(decide-momentum total traveled (+ m 2)) (+ m 2)]
        [(decide-momentum total traveled (+ m 1)) (+ m 1)]
        [(decide-momentum total traveled m) m]
        [else (- m 1)]))))

;;; Procedure:
;;;   circle
;;; Parameters:
;;;   x, a real number
;;;   y, a real number
;;;   r, a positive real number
;;;   color, a color
;;; Purpose:
;;;   Creates a drawing of a circle of radius r, centered at (x,y).
;;; Produces:
;;;   drawing, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   drawing is an ellipse.  That is (drawing-ellipse? drawing) holds.
;;;   (drawing-left drawing) = (- x r)
;;;   (drawing-top drawing) = (- y r)
;;;   (drawing-width drawing) = (* 2 r)
;;;   (drawing-height drawing) = (* 2 r)
;;;   (drawing-width drawing) = (drawing-height drawing)
;;;   (drawing-color drawing) = color
(define jump-heuristic-alt-4
  (lambda(node)
    (let ([total (car node)] [traveled (cadr node)] [m (caddr node)])
      (cond
        [(decide-momentum total traveled (+ m 2)) (expt (- total traveled (/ (* (+ m 3) (+ m 2)) 2)) 2)]
        [(decide-momentum total traveled (+ m 1)) (expt (- total traveled (/ (* (+ m 2) (+ m 1)) 2)) 2)]
        [(decide-momentum total traveled m) (expt (- total traveled (/ (* (+ m 1) m) 2)) 2)]
        [else (expt (- total traveled (/ (* m (- m 1)) 2)) 2)]))))

;;; Procedure:
;;;   circle
;;; Parameters:
;;;   x, a real number
;;;   y, a real number
;;;   r, a positive real number
;;;   color, a color
;;; Purpose:
;;;   Creates a drawing of a circle of radius r, centered at (x,y).
;;; Produces:
;;;   drawing, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   drawing is an ellipse.  That is (drawing-ellipse? drawing) holds.
;;;   (drawing-left drawing) = (- x r)
;;;   (drawing-top drawing) = (- y r)
;;;   (drawing-width drawing) = (* 2 r)
;;;   (drawing-height drawing) = (* 2 r)
;;;   (drawing-width drawing) = (drawing-height drawing)
;;;   (drawing-color drawing) = color
(define decide-momentum
  (lambda (total traveled m)
    (< 0 (- total traveljump-heuristiced (/ (* (+ m 1) m) 2)))))