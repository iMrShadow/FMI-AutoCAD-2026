; Homework 5

; Helper function to get a valid entity of specific type
(defun GetValidEntity (promptMsg typeName / ent) 
  (setq ent (entsel promptMsg))

  ; Loop until a valid entity is selected
  (while 
    (or (null ent)  ; No entity selected
        (/= typeName (cdr (assoc 0 (entget (car ent))))) ; Wrong type of entity
    )
    (if (null ent) 
      (princ "\nYou didn't select anything. Try again.")
      (princ (strcat "\nSelected object is not a " typeName "! Try again."))
    )

    (setq ent (entsel promptMsg))
  )

  ent
)

; Task 1
; Task 1.1
(defun C:MOD1A (/ el) 
  (setq ent (GetValidEntity 
              "\nSelect a circle to move its center to (2,2): "
              "CIRCLE"
            )
  )
  (setq el (entget (car ent)))
  (setq el (subst (list 10 2.0 2.0 0.0) (assoc 10 el) el))
  (entmod el)

  (princ)
)

; Task 1.2
(defun C:MOD1B (/ el) 
  (setq ent (GetValidEntity 
              "\nSelect an arc to extend to 180 degrees: "
              "ARC"
            )
  )
  (setq el (entget (car ent)))
  (setq el (subst (cons 51 pi) (assoc 51 el) el))
  (entmod el)

  (princ)
)

; Task 1.3
(defun C:MOD1C (/ el) 
  (setq ent (GetValidEntity 
              "\nSelect a line to move its endpoint to (7,4): "
              "LINE"
            )
  )
  (setq el (entget (car ent)))
  (setq el (subst (list 11 7.0 4.0 0.0) (assoc 11 el) el))
  (entmod el)

  (princ)
)

; Task 1.4
(defun C:MOD1D (/ el) 
  (setq ent (GetValidEntity 
              "\nSelect a circle to move center to (1,6) and radius to 1.25: "
              "CIRCLE"
            )
  )
  (setq el (entget (car ent)))
  (setq el (subst (list 10 1.0 6.0 0.0) (assoc 10 el) el))
  (setq el (subst (cons 40 1.25) (assoc 40 el) el))
  (entmod el)

  (princ)
)

; Task 1.5
(defun C:MOD1E (/ el) 
  (setq ent (GetValidEntity 
              "\nSelect a text object to change height to 0.75 and content to 'Robot': "
              "TEXT"
            )
  )
  (setq el (entget (car ent)))
  (setq el (subst (cons 1 "Robot") (assoc 1 el) el))
  (setq el (subst (cons 40 0.75) (assoc 40 el) el))
  (entmod el)

  (princ)
)

; Task 2
(defun C:MOD2 (/ el elist ent center radius cen_x cen_y oldsnap oldecho) 
  (setq ent (GetValidEntity 
              "\nChoose a circle: "
              "CIRCLE"
            )
  )

  (setq elist (entget (car ent)))
  (setq center (cdr (assoc 10 elist)))
  (setq radius (cdr (assoc 40 elist)))
  (setq cen_x (car center))
  (setq cen_y (cadr center))

  (setq oldsnap (getvar "OSMODE"))
  (setq oldecho (getvar "CMDECHO"))

  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  (command "_.rectang" 
           (list (- cen_x radius) (- cen_y radius))
           (list (+ cen_x radius) (+ cen_y radius))
  )

  (command "_.polygon" "6" center "I" radius)

  (setvar "OSMODE" oldsnap)
  (setvar "CMDECHO" oldecho)

  (princ)
)
