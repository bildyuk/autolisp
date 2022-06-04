
(defun get_block_by ( / parameters)
  "Documentation for get_block_by."
  (setq a (ssget "_W" (getpoint) (getpoint) (list (cons 2 "`*U*")  (cons 0 "INSERT"))))
(ss_2_list a)
)

;(setq l (get_block_by))
;(filter_list_f l get_block_record_name (cons 2 "Отв*"))
; выбрали все ответвители лотка


