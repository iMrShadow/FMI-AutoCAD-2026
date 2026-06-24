; ================================
; LEGO BRICK GENERATOR for AutoCAD
; ================================

; Helpers

(defun batch-union (solid-list / result) 
  (if (null solid-list) 
    nil
    (progn 
      (setq result (car solid-list))
      (foreach s (cdr solid-list) 
        (vla-Boolean result acUnion s)
      )
      result
    )
  )
)

; Vla helper operations
(defun addbox (p1 p2 / cen l w h) 
  (setq cen (mapcar '/ (mapcar '+ p1 p2) '(2.0 2.0 2.0))
        l   (abs (- (car p2) (car p1)))
        w   (abs (- (cadr p2) (cadr p1)))
        h   (abs (- (caddr p2) (caddr p1)))
  )
  (vla-AddBox *ms* (vlax-3d-point cen) l w h)
)

(defun addcyl (base dia height / cen) 
  (setq cen (list (car base) (cadr base) (+ (caddr base) (/ height 2.0))))
  (vla-AddCylinder *ms* (vlax-3d-point cen) (/ dia 2.0) height)
)

(defun sub-from (main tool) 
  (vla-Boolean main acSubtraction tool)
)

; 3. Knobs underneath (for 1×n or n×1 bricks)
(defun add-knobs (brick width depth brick-w hw hd tube-height / start-x start-y i j 
                  knob-d knob knob-list
                 ) 
  (if (or (and (= width 1) (> depth 1)) (and (> width 1) (= depth 1))) 
    (progn 
      (setq knob-d 3.2) ; 1 stud diameter = 12 LDU
      (setq knob-list '())
      (setq start-x (- (- hw) (/ brick-w 2)))
      (setq start-y (- (- hd) (/ brick-w 2)))
      (if (> width 1) (setq start-x (+ start-x 4)))
      (if (> depth 1) (setq start-y (+ start-y 4)))

      (if (= width 1) 
        (progn 
          (setq j 1)
          (while (<= j (1- depth)) 
            (setq cy (+ start-y (* j brick-w)))
            (setq knob-list (cons (addcyl (list 0 cy 0) knob-d tube-height) 
                                  knob-list
                            )
            )
            (setq j (1+ j))
          )
        )
        (progn 
          (setq i 1)
          (while (<= i (1- width)) 
            (setq cx (+ start-x (* i brick-w)))
            (setq knob-list (cons (addcyl (list cx 0 0) knob-d tube-height) 
                                  knob-list
                            )
            )
            (setq i (1+ i))
          )
        )
      )

      (if knob-list 
        (vla-Boolean brick acUnion (batch-union knob-list))
      )
    )
  )
)

(defun add-studs-and-dimples (brick width depth brick-w hw hd brick-h top-shell 
                              stud-d stud-h stud-dimple-d stud-dimple-h / stud-list 
                              dimple-list cx cy i j dimple-solid start-x start-y
                             ) 
  (setq stud-list   '()
        dimple-list '()
        start-x     (- (- hw) (/ brick-w 2))
        start-y     (- (- hd) (/ brick-w 2))
  )
  ; Create all studs
  (setq i 1)
  (while (<= i width) 
    (setq j 1)
    (while (<= j depth) 
      (setq cx (+ start-x (* i brick-w)))
      (setq cy (+ start-y (* j brick-w)))
      (setq stud-list (cons (addcyl (list cx cy brick-h) stud-d stud-h) stud-list))
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  ; Create all dimples
  (setq i 1)
  (while (<= i width) 
    (setq j 1)
    (while (<= j depth) 
      (setq cx (+ start-x (* i brick-w)))
      (setq cy (+ start-y (* j brick-w)))
      (setq dimple-list (cons 
                          (addcyl 
                            (list cx cy (- brick-h top-shell))
                            stud-dimple-d
                            stud-dimple-h
                          )
                          dimple-list
                        )
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )

  (if stud-list 
    (vla-Boolean brick acUnion (batch-union stud-list))
  )
  (if dimple-list 
    (progn 
      (setq dimple-solid (batch-union dimple-list))
      (vla-Boolean brick acSubtraction dimple-solid)
    )
  )
)

; 5. Interior walls/struts (for bricks larger than 1×1)
(defun add-interior-walls (brick width depth brick-w hw hd shell top-shell brick-h / 
                           all-ents i j wall strut-bottom strut-top start-x start-y
                          ) 
  (if (and (> width 1) (> depth 1)) 
    (progn 
      (setq start-x (- (- hw) (/ brick-w 2)))
      (setq start-y (- (- hd) (/ brick-w 2)))
      (if (> width 1) (setq start-x (+ start-x 4)))
      (if (> depth 1) (setq start-y (+ start-y 4)))
      (setq strut-bottom top-shell)
      (setq strut-top (- brick-h top-shell))
      (if (or (>= width 4) (>= depth 4)) 
        (progn 
          (setq all-ents '())
          ;; X walls
          (if (>= width 4) 
            (progn 
              (setq i 2)
              (while (<= i (1- width)) 
                (setq cx (+ start-x (* i brick-w)))
                (if (and (> i 0) (< i width)) 
                  (progn 
                    (setq wall (addbox 
                                 (list (- cx 0.4) (+ (- hd) shell) strut-bottom)
                                 (list (+ cx 0.4) (- hd shell) strut-top)
                               )
                    )
                    (setq all-ents (cons wall all-ents))
                  )
                )
                (setq i (+ i 2))
              )
            )
          )
          ;; Y walls
          (if (>= depth 4) 
            (progn 
              (setq j 2)
              (while (<= j (1- depth)) 
                (setq cy (+ start-y (* j brick-w)))
                (if (and (> j 0) (< j depth)) 
                  (progn 
                    (setq wall (addbox 
                                 (list (+ (- hw) shell) (- cy 0.4) strut-bottom)
                                 (list (- hw shell) (+ cy 0.4) strut-top)
                               )
                    )
                    (setq all-ents (cons wall all-ents))
                  )
                )
                (setq j (+ j 2))
              )
            )
          )
          (if all-ents 
            (vla-Boolean brick acUnion (batch-union all-ents))
          )
        )
      )
    )
  )
)

; 6. Interior ribs (for 1×n or n×1 bricks)
(defun add-interior-ribs (is-plate brick width depth brick-w hw hd shell top-shell 
                          brick-h / strut-height strut-bottom strut-top all-ents 
                          ridge-top ridge-bottom ridge i j start-x start-y rib
                         ) 
  (if (or (= width 1) (= depth 1)) 
    (progn 
      (setq start-x (- (- hw) (/ brick-w 2)))
      (setq start-y (- (- hd) (/ brick-w 2)))
      (if (> width 1) (setq start-x (+ start-x 4)))
      (if (> depth 1) (setq start-y (+ start-y 4)))

      (setq strut-height 6.20)

      (if (= is-plate T) (setq strut-height 2.06))
      (setq ridge-top (- brick-h top-shell))
      (setq ridge-bottom (- ridge-top strut-height))

      (setq all-ents '())
      (if (= width 1) 
        (progn 
          (setq j 1)
          (while (< j depth) 
            (setq cy (+ start-y (* j brick-w)))
            (setq rib (addbox 
                        (list (+ (- hw) shell) (- cy 0.4) ridge-bottom)
                        (list (- hw shell) (+ cy 0.4) ridge-top)
                      )
            )
            (setq all-ents (cons rib all-ents))
            (setq j (1+ j))
          )
        )
        (progn 
          (setq i 1)
          (while (< i width) 
            (setq cx (+ start-x (* i brick-w)))
            (setq rib (addbox 
                        (list (- cx 0.4) (+ (- hd) top-shell) ridge-bottom)
                        (list (+ cx 0.4) (- hd top-shell) ridge-top)
                      )
            )
            (setq all-ents (cons rib all-ents))
            (setq i (1+ i))
          )
        )
      )
      (if all-ents 
        (vla-Boolean brick acUnion (batch-union all-ents))
      )
    )
  )
)

; 7. Bottom tubes
(defun add-tubes (brick width depth brick-w hw hd tube-height / tube-out-dia 
                  tube-int-dia outer-list inner-list i j cx cy inner-solid
                 ) 
  (setq tube-out-dia 6.41
        tube-int-dia 4.80
        outer-list   '()
        inner-list   '()
  )
  (if (and (> width 1) (> depth 1)) 
    (progn 
      (setq i 1)
      (while (<= i (1- width)) 
        (setq j 1)
        (while (<= j (1- depth)) 
          (setq cx (+ (- hw) (* i brick-w)))
          (setq cy (+ (- hd) (* j brick-w)))
          (setq outer-list (cons (addcyl (list cx cy 0) tube-out-dia tube-height) 
                                 outer-list
                           )
          )
          (setq inner-list (cons (addcyl (list cx cy 0) tube-int-dia tube-height) 
                                 inner-list
                           )
          )
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )
      ; Union all outer tubes first, then union with brick
      (if outer-list 
        (vla-Boolean brick acUnion (batch-union outer-list))
      )
      ; Union all inner bores into one solid, then subtract once (removes walls)
      (if inner-list 
        (progn 
          (setq inner-solid (batch-union inner-list))
          (vla-Boolean brick acSubtraction inner-solid)
        )
      )
    )
  )
)

; 8. Inner wall ridges
(defun add-inner-ridges (brick width depth brick-w hw hd tube-height shell / ridge-w 
                         ridge-h half-ridge start-x start-y i j cx cy ridge all-ents
                        ) 
  (if (and (> width 1) (> depth 1)) 
    (progn 
      (setq ridge-w 0.60)
      (setq ridge-h 0.30)
      (setq half-ridge (/ ridge-w 2.0))

      (setq start-x (- (- hd) (/ brick-w 2))) ; for X-centers on left/right walls
      (setq start-y (- (- hw) (/ brick-w 2))) ; for Y-centers on front/back walls
      (setq all-ents '())
      ; X direction (left & right walls) – ridges along Y
      (setq i 1)
      (while (<= i depth) 
        (setq cx (+ start-x (* i brick-w))) ; now cx is inside [-hd, hd]
        ; left wall ridge
        (setq ridge (addbox 
                      (list (+ (- hw) shell) (- cx half-ridge) 0)
                      (list (+ (- hw) shell ridge-h) (+ cx half-ridge) tube-height)
                    )
        )
        (setq all-ents (cons ridge all-ents))
        ; right wall ridge
        (setq ridge (addbox 
                      (list (- hw shell ridge-h) (- cx half-ridge) 0)
                      (list (- hw shell) (+ cx half-ridge) tube-height)
                    )
        )
        (setq all-ents (cons ridge all-ents))
        (setq i (1+ i))
      )
      ; Y direction (front & back walls) – ridges along X
      (setq j 1)
      (while (<= j width) 
        (setq cy (+ start-y (* j brick-w))) ; now cy is inside [-hw, hw]
        ; front wall ridge
        (setq ridge (addbox 
                      (list (- cy half-ridge) (+ (- hd) shell) 0)
                      (list (+ cy half-ridge) (+ (- hd) shell ridge-h) tube-height)
                    )
        )
        (setq all-ents (cons ridge all-ents))
        ; back wall ridge
        (setq ridge (addbox 
                      (list (- cy half-ridge) (- hd shell ridge-h) 0)
                      (list (+ cy half-ridge) (- hd shell) tube-height)
                    )
        )
        (setq all-ents (cons ridge all-ents))
        (setq j (1+ j))
      )
      (if all-ents 
        (vla-Boolean brick acUnion (batch-union all-ents))
      )
    )
  )
)

(defun make-lego-brick (width depth is-plate color-num opt-studs opt-tubes opt-ribs 
                        opt-ridges / brick-w stud-d stud-h stud-dimple-d stud-dimple-h 
                        tube-height shell total-w total-d hw hd inner-w inner-d 
                        top-shell layer-name brick cavity oldsnap oldecho oldsnapmode 
                        oldregenmode oldsolidhist
                       ) 

  (vl-load-com)
  (setq *acad* (vlax-get-acad-object))
  (setq *doc* (vla-get-ActiveDocument *acad*))
  (setq *ms* (vla-get-ModelSpace *doc*))

  (setq oldsnap (getvar "OSMODE"))
  (setq oldecho (getvar "CMDECHO"))
  (setq oldsnapmode (getvar "SNAPMODE"))
  (setq oldregenmode (getvar "REGENMODE"))
  (setq oldsolidhist (getvar "SOLIDHIST"))

  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "SNAPMODE" 0)
  (setvar "REGENMODE" 0)
  (setvar "SOLIDHIST" 0)
  (vla-StartUndoMark *doc*)

  ; Constants
  (setq brick-w 8) ; 1 brick width/depth = 20 LDU
  (setq brick-h 9.6) ; 1 brick height = 24 LDU
  (if is-plate (setq brick-h (/ brick-h 3.0)))
  (setq stud-d 4.8) ; 1 stud diameter = 12 LDU
  (setq stud-h 1.8) ; 1 stud height = 4 LDU
  ; (setq plate-h 3.2) ; 1 plate height = 8 LDU
  (setq shell 1.2) ; wall / floor thickness (LDU)

  ; Spec
  (setq stud-dimple-d 2.60) ; stud dimple diameter (mm)
  (setq stud-dimple-h 1.70) ; stud dimple height (mm)

  (setq tube-height 8.50) ; strut height from floor (mm)
  (if is-plate (setq tube-height 2.10))

  ; (setq strut-w 6.99) ; strut width (mm)
  ; (setq strut-thick 0.80) ; strut thickness (mm)
  ; (setq strut-h 6.20) ; strut height from floor (mm)

  ; Derived dimensions
  (setq total-w (* width brick-w))
  (setq total-d (* depth brick-w))
  (setq hw (/ total-w 2.0)) ; half-width
  (setq hd (/ total-d 2.0)) ; half-depth
  (setq inner-w (- total-w (* 2 shell)))
  (setq inner-d (- total-d (* 2 shell)))
  (setq top-shell 1.1)

  ; Create layer for this brick
  (setq layer-name (strcat "LEGO_" (itoa color-num)))
  (command "_.LAYER" 
           "_MAKE"
           layer-name
           "_COLOR"
           (itoa color-num)
           layer-name
           ""
  )

  ; 1. Main brick body
  (setq tolerance 0.2)
  (setq offset (/ tolerance 2))

  (setq brick (addbox 
                (list (+ (- hw) offset)  ; = -(hw - offset)
                      (+ (- hd) offset)
                      0
                )
                (list (- hw offset) 
                      (- hd offset)
                      brick-h
                )
              )
  )

  ; 2. Hollow bottom cavity (always)
  (setq cavity (addbox 
                 (list (- (/ inner-w 2.0)) (- (/ inner-d 2.0)) 0)
                 (list (/ inner-w 2.0) (/ inner-d 2.0) (- brick-h top-shell))
               )
  )
  (sub-from brick cavity)

  ; 3. Knobs underneath (for 1×n or n×1 bricks)
  (if opt-tubes 
    (add-knobs brick width depth brick-w hw hd tube-height)
  )

  ; 4. Studs and dimples (optional)
  (if opt-studs 
    (add-studs-and-dimples brick width depth brick-w hw hd brick-h top-shell stud-d 
                           stud-h stud-dimple-d stud-dimple-h
    )
  )

  ; 5. Interior walls/struts, nxn bricks only
  (if (and opt-ribs) 
    (add-interior-walls brick width depth brick-w hw hd shell top-shell brick-h)
  )

  ; 6. Interior walls/struts for 1×n/n×1
  (if opt-ribs 
    (add-interior-ribs is-plate brick width depth brick-w hw hd shell top-shell 
                       brick-h
    )
  )

  ; 7. Bottom tubes
  (if opt-tubes 
    (add-tubes brick width depth brick-w hw hd tube-height)
  )

  ; 8. Inner wall ridges
  (if opt-ridges 
    (add-inner-ridges brick width depth brick-w hw hd tube-height shell)
  )

  ; Adjust layer and solid
  (vla-put-Layer brick layer-name)
  (vla-put-Color brick color-num)

  (setvar "OSMODE" oldsnap)
  (setvar "CMDECHO" oldecho)
  (setvar "SNAPMODE" oldsnapmode)
  (setvar "REGENMODE" oldregenmode)
  (setvar "SOLIDHIST" oldsolidhist)

  (command "_.REGEN")
  (command "_.ZOOM" "_EXTENTS")

  (vla-EndUndoMark *doc*)
  (princ)
)

; Command entries. Used for debugging only (testing)
; (defun c:LEGO-BRICK (/ w d h col) 
;   (setq w (getint "\nWidth  (studs, >= 1): "))
;   (setq d (getint "\nDepth  (studs, >= 1): "))
;   (setq is-plate (if (= (getint "\nIs it a plate? (0=No, 1=Yes): ") 1) T nil))
;   (setq col (getint "\nColor (1=Red 2=Yellow 3=Green 4=Cyan 5=Blue 6=Magenta): "))
;   (if (or (< w 1) (< d 1) (< col 1) (> col 255)) 
;     (alert "Invalid input.")
;     (make-lego-brick w d is-plate col T T T T)
;   )
; )

; (defun c:BRICK (/ w d col) 
;   (setq w (getint "\nWidth  (studs): "))
;   (setq d (getint "\nDepth  (studs): "))
;   (setq col (getint "\nColor (1-6): "))
;   (make-lego-brick w d nil col T T T T)
; )

; (defun c:PLATE (/ w d col) 
;   (setq w (getint "\nWidth  (studs): "))
;   (setq d (getint "\nDepth  (studs): "))
;   (setq col (getint "\nColor (1-6): "))
;   (make-lego-brick w d T col T T T T)
; )
