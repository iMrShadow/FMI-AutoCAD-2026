; Homework 3
; Helper functions

; Function to draw a rectangle given center, width and height
(defun draw-rectangle (center width height / half-width half-height p1 p2) 
  (setq half-width (/ width 2.0))
  (setq half-height (/ height 2.0))
  (setq p1 (list (- (car center) half-width) (- (cadr center) half-height)))
  (setq p2 (list (+ (car center) half-width) (+ (cadr center) half-height)))
  (command "_.rectangle" p1 p2)
)

; Function to draw an equilateral triangle given center and side length
(defun draw-triangle (center side / height dist-to-vertex dist-to-base p1 p2 p3) 
  (setq height (* side (sqrt 3.0) 0.5))
  (setq dist-to-vertex (/ (* height 2.0) 3.0))
  (setq dist-to-base (/ height 3.0))
  (setq p1 (list (car center) (+ (cadr center) dist-to-vertex)))
  (setq p2 (list (- (car center) (/ side 2.0)) (- (cadr center) dist-to-base)))
  (setq p3 (list (+ (car center) (/ side 2.0)) (- (cadr center) dist-to-base)))
  (command "_.pline" p1 p2 p3 "_c")
)

; Task 1
; 2.5.4
(defun C:RECTANGLE1 (/ center A B oldsnap) 
  (setq oldsnap (getvar "osmode"))
  (setvar "osmode" 0)

  (setq center (getpoint "\nEnter center: "))
  (setq A (getreal "\nEnter width along X (side A): "))
  (setq B (getreal "\nEnter height along Y (side B): "))

  (draw-rectangle center A B)

  (setvar "osmode" oldsnap)

  (print "Rectangle created.")
  (princ)
)

; 2.5.5
(defun C:TRIANGLE1 (/ center A oldsnap) 
  (setq oldsnap (getvar "osmode"))
  (setvar "osmode" 0)

  (setq center (getpoint "\nEnter center: "))
  (setq A (getreal "\nEnter side length: "))

  (draw-triangle center A)

  (setvar "osmode" oldsnap)

  (print "Equilateral triangle created.")
  (princ)
)

; Task 2
(defun C:DRAWFIGURES (/ start-point center-points side diameter rectangle-side 
                      divider oldsnap
                     ) 
  (setq oldsnap (getvar "osmode"))
  (setvar "osmode" 0)

  (setq start-point (getpoint "\nEnter start point: "))
  (setq center-points (list start-point 
                            (list (+ (car start-point) 20) (cadr start-point))
                            (list (+ (car start-point) 40) (cadr start-point))
                            (list (+ (car start-point) 60) (cadr start-point))
                      )
  )

  (setq side 10)
  (setq diameter 10)
  (setq rectangle-side 6)
  (setq divider 2.0)

  (draw-rectangle (nth 0 center-points) side side)
  (draw-triangle (nth 1 center-points) side)
  (command "_.circle" (nth 2 center-points) "D" diameter)
  (draw-rectangle (nth 3 center-points) rectangle-side side)

  (draw-rectangle (nth 3 center-points) (/ side divider) (/ side divider))
  (draw-triangle (nth 2 center-points) (/ side divider))
  (command "_.circle" (nth 1 center-points) "D" (/ diameter divider))
  (draw-rectangle 
    (nth 0 center-points)
    (/ rectangle-side divider)
    (/ side divider)
  )

  (setvar "osmode" oldsnap)

  (print "Figures created.")
  (princ)
)