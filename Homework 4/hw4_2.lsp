; Homework 4 - Task 2
; Helper functions

; Function to get the base point of the figure
(defun get_basepoint (/ bp) 
  (initget 1) ; Required point
  (setq bp (getpoint "\nEnter base point (center of the hypocycloid): "))

  bp
)

; Function to get the radius of the large circle
(defun get_big_radius (/ R tmp) 
  (if (null global:HYP_RL) (setq global:HYP_RL 100.0))

  (initget 6) ; Does not allow negative and zero values
  (setq tmp (getreal 
              (strcat "\nEnter radius of the large circle R <" 
                      (rtos global:HYP_RL 2 3)
                      ">: "
              )
            )
  )

  (if tmp (setq global:HYP_RL tmp))

  global:HYP_RL
)

; Function to get the radius of the small circle
(defun get_small_radius (RL / RS tmp) 
  (if (or (null global:HYP_RS) (>= global:HYP_RS RL)) 
    (setq global:HYP_RS (/ RL 4.0)) ; Default value - 1/4 of the large radius
  )

  (setq tmp nil)

  (while (or (null tmp) (>= tmp RL)) 
    (initget 6) ; Do not allow negative and zero values
    (setq tmp (getreal 
                (strcat "\nEnter radius of the small circle r (< R=" 
                        (rtos RL 2 3)
                        ") <"
                        (rtos global:HYP_RS 2 3)
                        ">: "
                )
              )
    )

    ; If the user does not enter a value
    (if (null tmp) (setq tmp global:HYP_RS))

    (if (>= tmp RL) 
      (progn 
        (princ 
          (strcat "\nError! r must be less than R=" 
                  (rtos RL 2 3)
                  "."
          )
        )
        (setq tmp nil) ; For the loop to continue if the value is invalid
      )
    )
  )

  (setq global:HYP_RS tmp)

  global:HYP_RS
)

; Function to get the number of turns the circle has to make
(defun get_number_of_turns (/ turns tmp) 
  (if (null global:HYP_TURNS) (setq global:HYP_TURNS 1))

  (initget 6) ; Do not allow negative and zero values
  (setq tmp (getint 
              (strcat "\nEnter number of rotations <" 
                      (itoa global:HYP_TURNS)
                      ">: "
              )
            )
  )

  (if tmp (setq global:HYP_TURNS tmp))

  global:HYP_TURNS
)

; Function to get the number of points for calculation per turn
(defun get_number_of_steps (/ steps tmp) 
  (if (null global:HYP_STEPS) (setq global:HYP_STEPS 50))

  (initget 6) ; Do not allow negative and zero values
  (setq tmp (getint 
              (strcat "\nEnter number of points for calculation <" 
                      (itoa global:HYP_STEPS)
                      ">: "
              )
            )
  )

  (if tmp (setq global:HYP_STEPS tmp))

  global:HYP_STEPS
)

; Task 2
(defun C:HYPOCYCLOID (/ RL RS steps turns bp oldsnap oldecho) 
  ; Save system variables
  (setq oldsnap (getvar "OSMODE"))
  (setq oldecho (getvar "CMDECHO"))

  ; Enter parameters
  (setq bp (get_basepoint))
  (setq RL (get_big_radius))
  (setq RS (get_small_radius RL))
  (setq turns (get_number_of_turns))
  (setq steps (get_number_of_steps))

  ; Exclude snaps and echo for commands
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  ; Draw the hypocycloid
  (draw_hypocycloid bp RL RS turns steps)

  ; Restore system variables
  (setvar "OSMODE" oldsnap)
  (setvar "CMDECHO" oldecho)

  (princ "\nHypocycloid created.")
  (princ)
)

; Function to draw the hypocycloid
(defun draw_hypocycloid (bp RL RS turns steps_per_turn / ang delta x y tp total_steps 
                         R_minus_r ratio
                        ) 
  ; Calculate the angle step. One turn is 2PI. Steps are the amount of points to calculate for a single turn.
  (setq delta (/ (* 2.0 pi) steps_per_turn))
  (setq total_steps (+ 1 (* steps_per_turn turns))) ; Add 1 for closure

  ; Define constants, because it takes a lot of time to compute them in the loop
  (setq R_minus_r (- RL RS)) ; R - r
  (setq ratio (/ R_minus_r RS)) ; (R - r) / r

  (setq ang 0.0)

  (command "_.pline")

  (repeat total_steps 
    ; Calculate the coordinates
    (setq x (* R_minus_r (cos ang)))
    (setq y (* R_minus_r (sin ang)))

    (setq x (+ x (* RS (cos (* ratio ang)))))
    (setq y (- y (* RS (sin (* ratio ang)))))

    ; Translate the point to the base point
    (setq tp (list (+ (car bp) x) (+ (cadr bp) y)))
    (command tp)

    ; Increment the angle
    (setq ang (+ ang delta))
  )

  (command "") ; End the polyline

  (princ)
)