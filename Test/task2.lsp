(defun draw_egg (bp radius / p1 p2 p3 p4 radius3 bp2) 

  (setq p1 (polar bp pi radius))
  (setq p2 (polar bp 0 radius))
  (command "arc" "c" bp p1 p2 "")

  (setq p3 (polar p1 (/ pi 4) (* 2 radius)))
  (setq p4 (polar p2 (* 3 (/ pi 4)) (* 2 radius)))

  (command "arc" "c" p1 p2 p3 "")
  (command "arc" "c" p2 p4 p1 "")

  (setq radius3 (- (* 2 radius) (* (sqrt 2) radius)))

  (setq bp2 (polar bp (/ pi 2) radius))

  (command "arc" "c" bp2 p3 p4 "")

  (princ)
)


(defun C:Sym3 (/ ss sl sl1 i rad llist oldcmd oldosm oldsnap) 
  (initget 3)
  (setq ss (ssget '((0 . "CIRCLE") (40 . 20.0))))
  (setq sl (sslength ss)
        i  0
  )
  (setq oldsnap (getvar "OSMODE"))
  (setq oldecho (getvar "CMDECHO"))

  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  (repeat sl 
    (setq sl1 (ssname ss i)
          i   (+ i 1)
    )

    (setq llist (entget sl1)
          rad   (cdr (assoc 40 llist))
          pc    (cdr (assoc 10 llist))
    )

    (ssdel sl1 ss)

    (draw_egg pc rad)
    ; Element needs to be deleted, but not enough time
  )

  (setvar "OSMODE" oldsnap)
  (setvar "CMDECHO" oldecho)
);end defun