(defun get_pwline_by ( / parameters)
  "Documentation for get_pwline."
  (setq all_pwline (ssget "_W" (getpoint) (getpoint) (list (cons 0 "lwpolyline") (list -3 (list "PE_URL")))))
(ss_2_list all_pwline)
)

(defun start_check (parameters)
  "Documentation for start_check."
  ; проверить слои

  ; получить блоки в соответствии со списком
  ; получить плинии с гиперссылками
  ; сверить количетво блоков линиям
    ; проверить маркировку блока слою 
    ; проверить маркировку плиний слою

  ; разложить по спискам плинии в соответствии со слоями
  ; разложить по списка блоки в соответствии со слоями



  ;проверить линии на 
  ())

(defun check_start (/ ss_ins)
  "Documentation for check_start."
  (setq 
    dev_list  (ss_2_list (ssget "x" (list (cons 0 "insert"))))
    link_list (ss_2_list (ssget "x" (list (cons 0 "lwpolyline") (list -3 (list "PE_URL"))))))
    
  (setq 
    fun_num_link (lambda(x) (cadr(cadr(assoc -3 (entget x '("PE_URL"))))))
    fun_num_dev  (lambda(x) (assoc 1(search_ent x (list (cons 2 "P_TAG_SKS")))))
    )

  (setq sck_link (filter_list link_list (cons 8 "_CL.TC##*")))
  (setq sck_dev(filter_list dev_list (cons 8 "_SCS.*socket*")))

  (setq wf_link (filter_list link_list (cons 8 "_CL.TC## WF*")))
  (setq wf_dev  (filter_list  dev_list (cons 8 "_SCS.TC*W*")))

  (setq de_link (filter_list link_list (cons 8 "_CL.TC## DECT*")))
  (setq de_dev  (filter_list  dev_list (cons 8 "_SCS.*Dect")))


  (mapcar '(lambda(x) (print  (fun_num_link x))) sck_link)
  (mapcar '(lambda(x) (print  (fun_num_dev x ))) sck_dev)

  (mapcar '(lambda(x) (print  (fun_num_link x))) wf_link)
  (mapcar '(lambda(x) (print  (fun_num_dev x ))) wf_dev)

  (mapcar '(lambda(x) (print  (fun_num_link x))) de_link)
  (mapcar '(lambda(x) (print  (fun_num_dev x ))) de_dev)


(print  "socket link")
(print (length sck_link))
(print " socket dev:")
(print (length sck_dev))

(print  "wifi link")
(print (length wf_link))
(print " wifi dev:")
(print (length wf_dev))
(print  "dect link")
(print (length de_link))
(print  "dect dev")
(print (length de_dev))

end.)
 
  
; (cadr(cadr(assoc -3 (entget (car (entsel)) '("PE_URL")))))




  