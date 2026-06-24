;Comments
;Or is it
; NOOOO
(defun med3 (x y z) (/ (+ x y z) 3.0))


(defun c:testline (/ a b)  ;define the function
  (setq a (getpoint "\nEnter First Point:")) ;get the first point
  (setq b (getpoint a "\nEnter Second Point:")) ;get the second point
  (command "Line" a b "") ;draw the line
  (princ) ;clean running
) 

(defun c:DrawRect1 () 
  (setq p1 (getpoint "\nПосочете първи ъгъл: "))
  (setq p2 (getcorner p1 "\nПосочете втори ъгъл: "))
  (command "_.RECTANG" p1 p2) ; Извиква вградената команда
  (princ)
)

(defun c:DrawRectDim (/ p1 len wid p2 p3 p4) 
  (setq p1  (getpoint "\nНачална точка (долен ляв ъгъл): ")
        len (getdist "\nДължина по X: ")
        wid (getdist "\nШирина по Y: ")
  )
  (setq p2 (list (+ (car p1) len) (cadr p1))
        p3 (list (+ (car p1) len) (+ (cadr p1) wid))
        p4 (list (car p1) (+ (cadr p1) wid))
  )
  (command "_.PLINE" p1 p2 p3 p4 "_C") ; Изчертава полилиния и я затваря
  (princ)
)

(defun c:DrawTriangle () 
  (setq center (getpoint "\nЦентър на триъгълника: ")
        rad    (getdist center "\nРазстояние до върха: ")
  )
  ; Използваме вградената команда за полигон с 3 страни
  (command "_.POLYGON" 3 center "_I" rad)
  (princ)
)

(defun c:CustomPoly (/ sides center method rad) 
  (setq sides  (getint "\nБрой страни: ")
        center (getpoint "\nЦентър на многоъгълника: ")
  )
  (initget "Inscribed Circumscribed")
  (setq method (getkword "\nМетод [Вписан(I)/Описан(C)] <I>: "))
  (if (null method) (setq method "I")) ; По подразбиране е "Inscribed"
  (setq rad (getdist center "\nРадиус: "))
  (command "_.POLYGON" sides center method rad)
  (princ)
)

(defun c:EasyArc () 
  (setq p1 (getpoint "\nНачална точка: ")
        p2 (getpoint p1 "\nМеждинна точка: ")
        p3 (getpoint p2 "\nКрайна точка: ")
  )
  (command "_.ARC" p1 p2 p3)
  (princ)
)

(defun C:listex () 
  (setq listex1 (list "A" "B" "C" "D"))
  (print listex1)
);end defun

(defun C:listex (/ L2 listex1) 
  (setq listex1 (list "A" "B" "C" "D"))
  (setq L2 (cons 1 listex1))
  (setq L3 (append L2 (list 9)))
  (setq elemnt1 (car L3))
  (setq elemnt2 (cadr L3))
  (setq elemnt5 (car (cdr (cdr (cdr (cdr L3))))))
  (setq elemnt_last (last L3))
  (reverse L3)
  (setq L3 (cdr L3))
);end defun

(defun c:rectangle1 (/ p1 p2) 
  (setq center (getpoint "\nПосочете център: "))
  (setq A (getreal "\nПосочете първата страна: "))
  (setq B (getreal "\nПосочете втората страна: "))
  (command "_.RECTANG" (list (+ (car center) (/ A 2)) (- (cadr center) (/ B 2))) (list (- (car center) (/ A 2)) (+ (cadr center) (/ B 2)))) ; Извиква вградената команда
  (princ)
)

