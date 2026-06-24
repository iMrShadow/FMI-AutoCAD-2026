; Lego GUI - A dialog interface for generating LEGO bricks in AutoCAD

; Global variables
(setq *lbg-sld* "C:/VLISP/SLD/brick_example.sld")
(if (not *lbg-last-w*) (setq *lbg-last-w* 2))
(if (not *lbg-last-d*) (setq *lbg-last-d* 4))
(if (not *lbg-last-plate*) (setq *lbg-last-plate* nil))
(if (not *lbg-last-color*) (setq *lbg-last-color* 1))
(if (not *lbg-last-studs*) (setq *lbg-last-studs* T))
(if (not *lbg-last-tubes*) (setq *lbg-last-tubes* T))
(if (not *lbg-last-ribs*) (setq *lbg-last-ribs* T))
(if (not *lbg-last-ridges*) (setq *lbg-last-ridges* T))

(setq *lbg-color-labels* '("White" "Orange" "Magenta" "Light Blue" "Yellow" "Lime" 
                           "Pink" "Gray" "Light Gray" "Cyan" "Purple" "Blue" "Brown" 
                           "Green" "Red" "Black"
                          )
)
(setq *lbg-color-aci* '(7 30 6 131 2 3 220 8 9 4 200 5 44 94 1 250))

; Library functions
(defun lbg:display-sld (img_key sld_path zoom x_off y_off / tw th dw dh x y) 
  (if (and sld_path (findfile sld_path)) 
    (progn 
      (start_image img_key)
      (setq tw (dimx_tile img_key)
            th (dimy_tile img_key)
            dw (* tw zoom)
            dh (* th zoom)
            x  (+ (/ (- tw dw) 2.0) x_off)
            y  (+ (/ (- th dh) 2.0) y_off)
      )
      (slide_image (fix x) (fix y) (fix dw) (fix dh) sld_path)
      (end_image)
    )
    (progn 
      (fill_image 0 0 (dimx_tile img_key) (dimy_tile img_key) -2)
      (end_image)
    )
  )
)

(defun lbg:dcl-path () 
  (cond 
    ((findfile "LEGO_GUI.dcl"))
    ((findfile "C:/VLISP/DCL/LEGO_GUI.dcl"))
    (T (alert "LEGO_GUI.dcl not found!") "")
  )
)

(defun lbg:load-core () 
  (if (not (boundp 'make-lego-brick)) 
    (progn 
      (setq lsp-path (cond 
                       ((findfile "LEGO.lsp"))
                       ((findfile "C:/VLISP/LSP/LEGO.lsp"))
                       (T nil)
                     )
      )
      (if lsp-path 
        (load lsp-path)
        (alert "LEGO.lsp not found! (parametric generator)")
      )
    )
  )
)

; Validation
(defun lbg:validate-all (w-str d-str is-plate / w d max-w max-d errs) 
  (setq errs '())
  (setq w (lbg:parse-int w-str))
  (setq d (lbg:parse-int d-str))
  (if (not w) (setq errs (cons "Width must be a positive number." errs)))
  (if (not d) (setq errs (cons "Depth must be a positive number." errs)))
  (if (and w d) 
    (progn 
      (if is-plate 
        (setq max-w 48
              max-d 48
        )
        (setq max-w 16
              max-d 16
        )
      )
      (if (< w 1) (setq errs (cons "Width must be at least 1." errs)))
      (if (> w max-w) 
        (setq errs (cons 
                     (strcat "Width max is " 
                             (itoa max-w)
                             (if is-plate " for plates." " for bricks.")
                     )
                     errs
                   )
        )
      )
      (if (< d 1) (setq errs (cons "Depth must be at least 1." errs)))
      (if (> d max-d) 
        (setq errs (cons 
                     (strcat "Depth max is " 
                             (itoa max-d)
                             (if is-plate " for plates." " for bricks.")
                     )
                     errs
                   )
        )
      )
    )
  )
  (if errs 
    (progn 
      (alert (apply 'strcat (mapcar '(lambda (e) (strcat e "\n")) (reverse errs))))
      nil
    )
    T
  )
)

(defun lbg:parse-int (str / n) 
  (if 
    (and str 
         (> (strlen str) 0)
         (not (vl-string-search "." str))
         (not (vl-string-search "-" str))
         (not (vl-string-search " " str))
    )
    (progn (setq n (atoi str)) (if (> n 0) n nil))
    nil
  )
)

; GUI
(defun lbg:on-width-change (val / parsed) 
  (setq parsed (lbg:parse-int val))
  (if parsed (setq w parsed))
)

(defun lbg:on-depth-change (val / parsed) 
  (setq parsed (lbg:parse-int val))
  (if parsed (setq d parsed))
)

(defun lbg:on-brick () (setq is-plate nil))
(defun lbg:on-plate () (setq is-plate T))

(defun lbg:on-color (idx) 
  (setq col-num (nth (atoi idx) *lbg-color-aci*))
)

(defun lbg:on-studs (val) (setq opt-studs (= val "1")))
(defun lbg:on-tubes (val) (setq opt-tubes (= val "1")))
(defun lbg:on-ribs (val) (setq opt-ribs (= val "1")))
(defun lbg:on-ridges (val) (setq opt-ridges (= val "1")))

(defun lbg:on-accept (/ w-str d-str w-val d-val) 
  (setq w-str (get_tile "width")
        d-str (get_tile "depth")
        w-val (lbg:parse-int w-str)
        d-val (lbg:parse-int d-str)
  )

  (if (lbg:validate-all w-str d-str is-plate) 
    (progn 
      (setq *lbg-last-w*      w-val
            *lbg-last-d*      d-val
            *lbg-last-plate*  is-plate
            *lbg-last-color*  col-num
            *lbg-last-studs*  opt-studs
            *lbg-last-tubes*  opt-tubes
            *lbg-last-ribs*   opt-ribs
            *lbg-last-ridges* opt-ridges
      )
      (setq w w-val
            d d-val
      )
      (done_dialog 1)
    )
    (set_tile "status" "Please correct the errors above.")
  )
)

(defun lbg:on-cancel () (done_dialog 0))

; Main command
(defun c:LEGO (/ dcl-id dlg-done) 
  (lbg:load-core)

  (setq dcl-id (load_dialog (lbg:dcl-path)))
  (if (< dcl-id 0) (progn (alert "Cannot load LEGO_GUI.dcl") (exit)))
  (if (not (new_dialog "lego_brick_dialog" dcl-id)) 
    (progn (alert "Cannot open lego_brick_dialog") (unload_dialog dcl-id) (exit))
  )

  ; initial global variables
  (setq w          *lbg-last-w*
        d          *lbg-last-d*
        is-plate   *lbg-last-plate*
        col-num    *lbg-last-color*
        opt-studs  *lbg-last-studs*
        opt-tubes  *lbg-last-tubes*
        opt-ribs   *lbg-last-ribs*
        opt-ridges *lbg-last-ridges*
  )

  (set_tile "width" (itoa w))
  (set_tile "depth" (itoa d))
  (if is-plate 
    (progn (set_tile "rb_plate" "1") (set_tile "rb_brick" "0"))
    (progn (set_tile "rb_brick" "1") (set_tile "rb_plate" "0"))
  )

  ; Populate colour popup_list
  (start_list "color")
  (foreach label *lbg-color-labels* (add_list label))
  (end_list)

  ; Find the index of the saved ACI colour
  (setq color-idx (vl-position col-num *lbg-color-aci*))
  (if (not color-idx) (setq color-idx 0))
  (set_tile "color" (itoa color-idx))

  (set_tile "opt_studs" (if opt-studs "1" "0"))
  (set_tile "opt_tubes" (if opt-tubes "1" "0"))
  (set_tile "opt_ribs" (if opt-ribs "1" "0"))
  (set_tile "opt_ridges" (if opt-ridges "1" "0"))

  ; display preview
  (lbg:display-sld "preview" *lbg-sld* 2.5 0 -150)

  ; action tiles
  (action_tile "width" "(lbg:on-width-change $value)")
  (action_tile "depth" "(lbg:on-depth-change $value)")
  (action_tile "rb_brick" "(lbg:on-brick)")
  (action_tile "rb_plate" "(lbg:on-plate)")
  (action_tile "color" "(lbg:on-color $value)")
  (action_tile "opt_studs" "(lbg:on-studs $value)")
  (action_tile "opt_tubes" "(lbg:on-tubes $value)")
  (action_tile "opt_ribs" "(lbg:on-ribs $value)")
  (action_tile "opt_ridges" "(lbg:on-ridges $value)")
  (action_tile "accept" "(lbg:on-accept)")
  (action_tile "cancel" "(lbg:on-cancel)")

  (setq dlg-done (start_dialog))
  (unload_dialog dcl-id)

  (if (= dlg-done 1) 
    (progn 
      (princ 
        (strcat "\nGenerating " 
                (itoa w)
                "×"
                (itoa d)
                (if is-plate " plate" " brick")
                " color="
                (itoa col-num)
                " …"
        )
      )
      (make-lego-brick w d is-plate col-num opt-studs opt-tubes opt-ribs opt-ridges)
    )
    (princ "\nCancelled.")
  )
  (princ)
)

(princ "\nLEGO GUI loaded. Type LEGO to start.\n")
(princ)