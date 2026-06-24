
(defun get_basepoint (/ bp) 
  (initget 1) ; Required point
  (setq bp (getpoint "\nEnter base point: "))

  bp
)

(defun get_angle (bp / tmp) 
  (if (null global:FIG1_ANG) (setq global:FIG1_ANG 0.0)) ; Default value (0 degrees)

  (initget 0) ; Allow empty input
  (setq tmp (getangle bp 
                      (strcat "\nEnter rotation angle <" 
                              (angtos global:FIG1_ANG 1 2)
                              ">: "
                      )
            )
  )

  (if tmp (setq global:FIG1_ANG tmp))

  global:FIG1_ANG
)

; Function to get the height of the figure with validation
(defun get_a (radius / tmp) 

  (if (or (null global:Sym1_a) (>= global:Sym1_a radius)) 
    (setq global:Sym1_a (* radius 0.5))
  )

  (setq tmp nil)

  ; Loop until a valid height is entered
  (while (or (null tmp) (>= global:Sym1_a radius)) 
    (initget 6) ; Do not allow negative and zero values
    (setq tmp (getreal 
                (strcat "\nEnter a <= " 
                        (rtos (/ radius 2) 2 3)
                        ": "
                )
              )
    )

    ; If the user does not enter a value
    (if (null tmp) (setq tmp global:Sym1_a))

    ; Validation of the entered value
    (if (>= tmp radius) 
      (progn 
        (princ 
          (strcat "\nError!")
        )
        (setq tmp nil) ; For the loop to continue if the value is invalid
      )
    )
  )

  (setq global:Sym1_a tmp)

  global:Sym1_a
)

; Function to get the rotation of the figure
(defun get_radius (/ tmp) 
  (if (null global:Sym1_R) (setq global:Sym1_R 20.0)) ; Default value (0 degrees)

  (initget 6) ; Do not allow negative and zero values
  (setq tmp (getreal "\nEnter radius: "))

  (if tmp (setq global:Sym1_R tmp))

  global:Sym1_R
)

; Task 1.1
(defun C:Sym1 (/ bp radius a oldsnap oldecho angle1) 
  ; Save system variables
  (setq oldsnap (getvar "OSMODE"))
  (setq oldecho (getvar "CMDECHO"))

  ; Enter parameters
  (setq bp (get_basepoint))
  (setq radius (get_radius))
  (setq a (get_a radius))
  (setq angle1 (get_angle bp))

  ; Exclude snaps and echo for commands
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  (draw_sym1 bp radius a angle1)

  ; Restore system variables
  (setvar "OSMODE" oldsnap)
  (setvar "CMDECHO" oldecho)

  (princ)
)

; Function to draw the first figure
(defun draw_sym1 (bp radius a angle1 / p1 p2 p3 p4 p5 p6 p7 arcpoint rad) 
  (setq p1 (polar bp (+ pi angle1) (/ a 2)))
  (setq p2 (polar bp (+ pi angle1) radius))
  (setq p3 (polar bp angle1 radius))
  (setq p4 (polar bp (+ (/ pi 4) angle1) radius))
  (setq p5 (polar bp (+ (* 3 (/ pi 4)) angle1) radius))
  (setq p6 (polar bp angle1 (/ a 2)))
  (setq arcpoint (polar bp (+ (/ pi 2) angle1) radius))

  ; TODO: Get the intersection point
  ; Идеята е, да се вземе пресечната точка
  (setq p7 (inters p6 (polar p6 (+ (* 123 (/ pi 180)) angle1) radius) bp p4 nil))

  (command "pline" p1 p2 "")
  (command "arc" p2 arcpoint p3 "")
  (command "pline" p3 p6 "")
  (command "pline" bp p4 "")
  (command "pline" bp p5 "")
  (command "pline" p6 p7 "")
  (princ)
)


