; Homework 5
; Task 1

; TODO: Validation with while loop and prompt for correct input
; Task 1.1
(defun C:MODC1 (/ el) 
  (setq el (entget (car (entsel))))
  (setq el (subst (list 10 2.0 2.0 0.0) (assoc 10 el) el))
  (entmod el)
);end defun

; Task 1.2
(defun C:MODA1 (/ el) 
  (setq el (entget (car (entsel))))
  (setq el (subst (cons 51 pi) (assoc 51 el) el))
  (entmod el)
);end defun

; Task 1.3
(defun C:MODD1 (/ el) 
  (setq el (entget (car (entsel))))
  (setq el (subst (list 10 6.0 2.0 0.0) (assoc 10 el) el))
  (setq el (subst (list 11 7.0 4.0 0.0) (assoc 11 el) el))
  (entmod el)
);end defun

; Task 1.4
(defun C:MODE1 (/ el) 
  (setq el (entget (car (entsel))))
  (setq el (subst (list 10 1.0 6.0 0.0) (assoc 10 el) el))
  (setq el (subst (cons 40 1.25) (assoc 40 el) el))
  (entmod el)
);end defun

; Task 1.5
(defun C:MODF1 (/ el) 
  (setq el (entget (car (entsel))))
  (setq el (subst (cons 1 "Robot") (assoc 1 el) el))
  (setq el (subst (cons 40 0.75) (assoc 40 el) el))
  (entmod el)
);end defun

; Task 2
(defun C:MODIFYCIRCLE (/ el elist ent) 
  ; (if
  ;(and
  (setq ent (entsel "\nИзберете окръжност: "))
  ;  (setq elist (entget ent))
  ;   (= "CIRCLE" (cdr (assoc 0 elist)))
  ; )

  ; (progn
  (setq elist ent)
  ;; Проверка дали код 62 (цвят) съществува

  (setq center (assoc 10 elist))
  (setq radius (assoc 40 elist))
  ; (setq el (entget (car (entsel))))
  ; (setq el (subst (cons 1 "Robot") (assoc 1 el) el))
  ; (setq el (subst (cons 40 0.75) (assoc 40 el) el))
  ; (entmod el)


  (entmod elist)
  (entupd ent) ;; Препоръчително за пълно визуално обновяване
  ; )
);end defun