; EX1.LSP
; A program to draw a square with inscribed and circumscribed circles.
; (defun dtr (ang) 
;   (* pi (/ ang 180.0))
; )
; ;
; (defun C:ex1 (/ p1 p2 len p3 p4 theta) 
;   (setq p1    (getpoint "\nPlease enter the lower left point: ")
;         p2    (getpoint p1 "\nDrag the length of side and angle: ") ;Построява се временна прекъсната
;         линия от
;         точка p1
;   )
;   (setq len   (distance p1 p2) ; calculate the length 'len'
;         theta (angle p1 p2) ; and the angle 'theta'
;   )
;   (setq p3 (polar p2 (+ theta (dtr 90)) len)
;         p4 (polar p3 (+ theta (dtr 180)) len)
;   )
;   (command "line" p1 p2 p3 p4 "c") ; draw the square
;   (setq pcenter (inters p1 p3 p2 p4)) ; find the circle center point
;   (command "circle" pcenter (/ len 2)) ; draw the inner circle
;   (command "circle" pcenter p1) ; draw the outer circle
;   (redraw)
; ) ;end of the defun ex1

; ; Homework 3
; ; Helper functions

; ; Function to draw a rectangle given center, width and height
; (defun draw-rectangle (center width height / half-width half-height p1 p2) 
;   (setq half-width (/ width 2.0))
;   (setq half-height (/ height 2.0))
;   (setq p1 (list (- (car center) half-width) (- (cadr center) half-height)))
;   (setq p2 (list (+ (car center) half-width) (+ (cadr center) half-height)))
;   (command "_.rectangle" p1 p2)
; )

; ; Function to draw an equilateral triangle given center and side length
; (defun draw-triangle (center side / height dist-to-vertex dist-to-base p1 p2 p3) 
;   (setq height (* side (sqrt 3.0) 0.5))
;   (setq dist-to-vertex (/ (* height 2.0) 3.0))
;   (setq dist-to-base (/ height 3.0))
;   (setq p1 (list (car center) (+ (cadr center) dist-to-vertex)))
;   (setq p2 (list (- (car center) (/ side 2.0)) (- (cadr center) dist-to-base)))
;   (setq p3 (list (+ (car center) (/ side 2.0)) (- (cadr center) dist-to-base)))
;   (command "_.pline" p1 p2 p3 "_c")
; )

; ; Task 1
; ; 2.5.4
; (defun C:RECTANGLE1 (/ center A B oldsnap) 
;   (setq oldsnap (getvar "osmode"))
;   (setvar "osmode" 0)

;   (setq center (getpoint "\nEnter center: "))
;   (setq A (getreal "\nEnter width along X (side A): "))
;   (setq B (getreal "\nEnter height along Y (side B): "))

;   (draw-rectangle center A B)

;   (setvar "osmode" oldsnap)

;   (print "Rectangle created.")
;   (princ)
; )

; ; 2.5.5
; (defun C:TRIANGLE1 (/ center A oldsnap) 
;   (setq oldsnap (getvar "osmode"))
;   (setvar "osmode" 0)

;   (setq center (getpoint "\nEnter center: "))
;   (setq A (getreal "\nEnter side length: "))

;   (draw-triangle center A)

;   (setvar "osmode" oldsnap)

;   (print "Equilateral triangle created.")
;   (princ)
; )

; ; Task 2
; (defun C:DRAWFIGURES (/ start-point center-points side diameter rectangle-side 
;                       divider oldsnap
;                      ) 
;   (setq oldsnap (getvar "osmode"))
;   (setvar "osmode" 0)

;   (setq start-point (getpoint "\nEnter start point: "))
;   (setq center-points (list start-point 
;                             (list (+ (car start-point) 20) (cadr start-point))
;                             (list (+ (car start-point) 40) (cadr start-point))
;                             (list (+ (car start-point) 60) (cadr start-point))
;                       )
;   )

;   (setq side 10.0)
;   (setq diameter 10.0)
;   (setq rectangle-side 6.0)
;   (setq divider 2.0)

;   (draw-rectangle (nth 0 center-points) side side)
;   (draw-triangle (nth 1 center-points) side)
;   (command "_.circle" (nth 2 center-points) "D" diameter)
;   (draw-rectangle (nth 3 center-points) rectangle-side side)

;   (draw-rectangle (nth 3 center-points) (/ side divider) (/ side divider))
;   (draw-triangle (nth 2 center-points) (/ side divider))
;   (command "_.circle" (nth 1 center-points) "D" (/ diameter divider))
;   (draw-rectangle 
;     (nth 0 center-points)
;     (/ rectangle-side divider)
;     (/ side divider)
;   )

;   (setvar "osmode" oldsnap)

;   (print "Figures created.")
;   (princ)
; )

(defun C:SYMBOL1 (/ bp L h p1 p2 p3 p4 rad angl1 tmp tmp1 oldsnap) 
  (setq oldsnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (input_d)
  (cal_d)
  (draw_1)

  (setvar "OSMODE" oldsnap)
)

(defun input_d () 
  (initget 1)
  (setq bp (getpoint "\nEnter base point: "))
  (if (null global:dis1) (setq global:dis1 100.0))
  (initget 6)
  (if (setq tmp (getreal (strcat "\n Input length: <" (rtos global:dis1 2 3) ">: "))) 
    (setq global:dis1 tmp)
  )
  (setq L global:dis1)

  (if (null global:dis2) (setq global:dis2 40.0))

  ; Довърши l/4 <= h <= l/2
  (initget 6)
  (if (setq tmp1 (getreal (strcat "\n Input height <" (rtos global:dis2 2 3) ">: "))) 
    (setq global:dis2 tmp1)
  )
  (setq h global:dis2)

  (setq angl1 (getangle bp "\n Input angle: "))
)

(defun cal_d (/) 
  (setq p2 (polar bp (+ pi angl1) (/ L 2.0)))
  (setq p1 (polar p2 (+ (/ pi 2.0) angl1) h))
  (setq p3 (polar bp (+ 0.0 angl1) (/ L 2.0)))
  (setq p4 (polar p3 (+ (/ pi 2.0) angl1) h))
  (setq rad (/ L 4.0))
)

(defun draw_1 (/) 
  (command "pline" p1 p2 p3 p4 "")
  (command "circle" bp rad)
)

(defun C:SYMBOL2 (/ bp L h p1 p2 p3 p4 rad angl1 tmp tmp1 oldsnap) 
  (setq oldsnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (input_d)
  (cal_d)
  (draw_2)

  (setvar "OSMODE" oldsnap)
)

(defun draw_2 (/ p1a p4a p5) 
  (setq p1a (polar p2 (+ (+ (/ pi 2.0) (/ pi 6.0)) angl1) h))
  (setq p4a (polar p3 (+ (/ pi 3.0) angl1) h))
  (setq p5 (polar bp (+ (/ pi 2.0) angl1) (* h 4.0)))

  (command "pline" p1a p2 p3 p4a p5 "c")
)


(defun C:SPIRAL (/ nt bp cf lp) 
  (initget 1) ; bp must not be null
  (setq bp (getpoint "\nCenter point: "))
  (initget 7) ; nt must not be zero, neg, or null
  (setq nt (getint "\nNumber of rotations: "))
  (initget 3) ; cf must not be zero, or null
  (setq cf (getdist "\nGrowth per rotation: "))
  (initget 6) ; lp must not be zero or neg
  (setq lp (getint "\nPoints per rotation <30>: "))
  (cond ((null lp) (setq lp 30)))
  (cspiral nt bp cf lp)
);end of defun
(defun cspiral (ntimes bpoint cfac lppass / ang dist tp ainc dinc circle bs cs 
                oldsnap
               ) 
  (getsystvar)
  (setq circle (* pi 2)
        ainc   (/ circle lppass)
        dinc   (/ cfac lppass)
        ang    0.0
        dist   0.0
  )
  (turnofsyst)
  (command "pline" bpoint) ; start spiral from base point and...
  (repeat ntimes 
    (repeat lppass 
      (setq tp (polar bpoint 
                      (setq ang (+ ang ainc))
                      (setq dist (+ dist dinc))
               )
      )
      (command tp) ; continue to the next point...
    )
  )
  (command) ; until done.
  (setsystvar)
);end defun cspiral
(defun getsystvar () 
  (setq cs      (getvar "cmdecho") ; save old cmdecho
        oldsnap (getvar "osmode")
  )
) ;end of defun
; turn off systems
(defun turnofsyst () 
  (setvar "cmdecho" 0) ; turn cmdecho off
  (setvar "osmode" 0)
) ;end of defun
(defun setsystvar () 
  (setvar "osmode" oldsnap)
  (setvar "cmdecho" cs)
)