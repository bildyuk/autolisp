
(defun get_block_by ( / parameters)
  "Documentation for get_block_by."
  (setq a (ssget "_W" (getpoint) (getpoint) (list (cons 2 "`*U*")  (cons 0 "INSERT"))))
(ss_2_list a)
)

;(setq l (get_block_by))
;(filter_list_f l get_block_record_name (cons 2 "Отв*"))
; выбрали все ответвители лотка


(defun optimize (a)
  "Documentation for optimize."
  (setq 
    blk (entget a)
    blk_coord (assoc 10 blk)
    blk_coord_fixed (mapcar 'fix blk_coord))

  (entmod (subst blk_coord_fixed blk_coord blk)))

(defun verticalize ()
  "Documentation for verticalize."
   (setq 
    base_blk (entget(car(nentsel)))
    blk_coord (assoc 10 base_blk)
    blk_coord_x ( cadr blk_coord)
    blk_coord_y ( caddr blk_coord)
    )


   )

(defun list_block (/ a b)
  "Documentation for list_block."
  (setq 
    c (tblnext "block" t)
    b '())
  (while (/= c nil)
    (setq 
       a (cdr(assoc 330(entget(cdr(assoc -2 c ))))))
       c (print (tblnext "block" )))
       (if (= (cdr(assoc 70 c)) 0) (setq b (append b (list a)))))

  



  (defun del_hatch_ (block_name / a b)
    "Documentation for del_hatch."
    (setq 
      a (print (tblsearch "block" block_name))
      b (print (cdr(assoc -2 a))))
    (print (entmake (list (cons 0 "block") (cons 2 block_name) (list 10 0 0 0)(cons 70 0))))
    (while (/= b nil)
      (setq c (entget b))
      (if (/= (cdr(assoc 0 c)) "HATCH") (entmake c))
      (print (setq b (entnext b)))
      )
    (entmake (list (cons 0 "endblk")))
    )