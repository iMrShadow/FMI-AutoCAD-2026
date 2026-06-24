; Homework 4 - Task 1
; Helper functions

; Function to get the base point of the figure
(defun get_basepoint (/ bp) 
  (initget 1) ; Required point
  (setq bp (getpoint "\nEnter base point (center of the horizontal line): "))

  bp
)

; Function to get the length of the horizontal line
(defun get_length (/ L tmp) 
  (if (null global:FIG1_L) (setq global:FIG1_L 100.0))

  (initget 6) ; Do not allow negative and zero values
  (setq tmp (getreal 
              (strcat "\nEnter length L <" 
                      (rtos global:FIG1_L 2 3)
                      ">: "
              )
            )
  )

  (if tmp (setq global:FIG1_L tmp))

  global:FIG1_L
)

; Function to get the height of the figure with validation
(defun get_height (L / h tmp min_h max_h) 
  ; Check if the global value is within the range between L/4 and L/2
  ; Default value - (1/4L + 1/2L)/2 = 3/8L = 0.375L
  (if (or (null global:FIG1_H) (< global:FIG1_H min_h) (> global:FIG1_H max_h)) 
    (setq global:FIG1_H (* L 0.375))
  )

  (setq min_h (/ L 4.0))
  (setq max_h (/ L 2.0))

  (setq tmp nil)

  ; Loop until a valid height is entered
  (while (or (null tmp) (< tmp min_h) (> tmp max_h)) 
    (initget 6) ; Do not allow negative and zero values
    (setq tmp (getreal 
                (strcat "\nEnter height h (" 
                        (rtos min_h 2 3)
                        " <= h <= "
                        (rtos max_h 2 3)
                        ") <"
                        (rtos global:FIG1_H 2 3)
                        ">: "
                )
              )
    )

    ; If the user does not enter a value
    (if (null tmp) (setq tmp global:FIG1_H))

    ; Validation of the entered value
    (if (or (< tmp min_h) (> tmp max_h)) 
      (progn 
        (princ 
          (strcat "\nError! h must be between " 
                  (rtos min_h 2 3)
                  " and "
                  (rtos max_h 2 3)
                  ". Please try again."
          )
        )
        (setq tmp nil) ; For the loop to continue if the value is invalid
      )
    )
  )

  (setq global:FIG1_H tmp)

  global:FIG1_H
)

; Function to get the rotation of the figure
(defun get_angle (bp / angle1 tmp) 
  (if (null global:FIG1_ANG) (setq global:FIG1_ANG 0.0)) ; Default value (0 degrees)

  (initget 0) ; Allow empty input
  (setq tmp (getangle bp 
                      (strcat "\nEnter rotation angle <" 
                              (angtos global:FIG1_ANG 0 2)
                              ">: "
                      )
            )
  )

  (if tmp (setq global:FIG1_ANG tmp))

  global:FIG1_ANG
)

; Task 1.1
(defun C:FIGURE1 (/ bp L h angle1 oldsnap oldecho) 
  ; Save system variables
  (setq oldsnap (getvar "OSMODE"))
  (setq oldecho (getvar "CMDECHO"))

  ; Enter parameters
  (setq bp (get_basepoint))
  (setq L (get_length))
  (setq h (get_height L))
  (setq angle1 (get_angle bp))

  ; Exclude snaps and echo for commands
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  (draw_figure1 bp L h angle1)

  ; Restore system variables
  (setvar "OSMODE" oldsnap)
  (setvar "CMDECHO" oldecho)

  (princ)
)

; Function to draw the first figure
(defun draw_figure1 (bp L h angle1 / p1 p2 p3 p4 rad) 
  ; Calculating the points
  (setq p2 (polar bp (+ pi angle1) (/ L 2.0)))
  (setq p1 (polar p2 (+ (/ pi 2.0) angle1) h))
  (setq p3 (polar bp angle1 (/ L 2.0)))
  (setq p4 (polar p3 (+ (/ pi 2.0) angle1) h))
  (setq rad (/ L 4.0))

  (command "pline" p1 p2 p3 p4 "")

  (command "circle" bp rad)

  (princ)
)

; Task 1.2
(defun C:FIGURE2 (/ bp L h angle1 oldsnap oldecho) 
  ; Save system variables
  (setq oldsnap (getvar "OSMODE"))
  (setq oldecho (getvar "CMDECHO"))

  ; Enter parameters
  (setq bp (get_basepoint))
  (setq L (get_length))
  (setq h (get_height L))
  (setq angle1 (get_angle bp))

  ; Exclude snaps and echo for commands
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  (draw_figure2 bp L h angle1)

  ; Restore system variables
  (setvar "OSMODE" oldsnap)
  (setvar "CMDECHO" oldecho)

  (princ)
)

; Function to draw the second figure
(defun draw_figure2 (bp L h angle1 / p1 p2 p3 p4 p5) 
  (setq p2 (polar bp (+ pi angle1) (/ L 2.0)))
  (setq p3 (polar bp angle1 (/ L 2.0)))

  (setq p1 (polar p2 (+ (+ (/ pi 2.0) (/ pi 6.0)) angle1) h))
  (setq p4 (polar p3 (+ (/ pi 3.0) angle1) h))
  (setq p5 (polar bp (+ (/ pi 2.0) angle1) (* h 4.0)))

  (command "pline" p1 p2 p3 p4 p5 "c")

  (princ)
)
