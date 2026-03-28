; EX1.LSP
; A program to draw a square with inscribed and circumscribed circles.
(defun dtr (ang) 
  (* pi (/ ang 180.0))
)
;
(defun C:ex1 (/ p1 p2 len p3 p4 theta) 
  (setq p1    (getpoint "\nPlease enter the lower left point: ")
        p2    (getpoint p1 "\nDrag the length of side and angle: ") ;Построява се временна прекъсната
        линия от
        точка p1
  )
  (setq len   (distance p1 p2) ; calculate the length 'len'
        theta (angle p1 p2) ; and the angle 'theta'
  )
  (setq p3 (polar p2 (+ theta (dtr 90)) len)
        p4 (polar p3 (+ theta (dtr 180)) len)
  )
  (command "line" p1 p2 p3 p4 "c") ; draw the square
  (setq pcenter (inters p1 p3 p2 p4)) ; find the circle center point
  (command "circle" pcenter (/ len 2)) ; draw the inner circle
  (command "circle" pcenter p1) ; draw the outer circle
  (redraw)
) ;end of the defun ex1