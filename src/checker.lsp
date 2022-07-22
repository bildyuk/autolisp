(defun get_pwline_by ( / parameters)
  "Documentation for get_pwline."
  (setq all_pwline (ssget "_W" (getpoint) (getpoint) (list (cons 0 "lwpolyline") (list -3 (list "PE_URL")))))
  (ss_2_list all_pwline)
  )

(defun start_check (parameters)
  "Documentation for start_check."
  ; проверить слои

  ; получить блоки в соответствии со списком
  (setq 
    all_blk_l  (ss_2_list(ssget "x"  (list (cons 0 "INSERT"))))
    all_link_l (ss_2_list (ssget "x" (list (cons 0 "lwpolyline") (list -3 (list "PE_URL")))))
    dev_tc_l     (filter_list_f all_blk_l get_block_record_name (cons 2 "Device_TC"))
    dev_socket_l (filter_list_f all_blk_l get_block_record_name (cons 2 "Device_socket"))
    dev_wifi_l   (filter_list_f all_blk_l get_block_record_name (cons 2 "Device_WiFi"))
    dev_dect_l   (filter_list_f all_blk_l get_block_record_name (cons 2 "Device_DECT")))
  ; получить плинии с гиперссылками
  ; сверить количетво блоков линиям
    ; проверить маркировку блока слою 
    ; проверить маркировку плиний слою

  ; разложить по спискам плинии в соответствии со слоями
  ; разложить по списка блоки в соответствии со слоями
  ;

;(entmake '((0 . "INSERT") (66 . 0) (cons 2 "blockname") (50 . 0) (210 0.0 0.0 1.0)))
 

  ;проверить линии на
  (print 
    (setq 
      tc_list (mapcar '(lambda(x) (cdr(assoc 1 (search_ent x (list (cons 2 "P_TAG")))))) dev_tc_l)
      tc_num_l (mapcar '(lambda(x) (substr x 3))tc_list)
      )
    )
  (mapcar 'che tc_num_l)
  )


(defun ch (a / fu1)
  "Documentation for ch."

  (setq layers (srch_layer a)

    fu1 (lambda(x) 
      (list x (sslength 
        (ssget "X" (list (cons 0 "lwpolyline") (cons 8 x) (list -3 (list "PE_URL" (cons 1000 "*")))))))))

  (mapcar 'fu1 layers))

;сверили линии
(defun che (n / fu summ)
 (setq 
    str_n n ;(itoa n)

    layer_name_cl_socket (strcat "_CL.TC" str_n " #*")
    layer_name_cl_dect   (strcat "_CL.TC" str_n " dect*")
    layer_name_cl_wf     (strcat "_CL.TC" str_n " wf*")
    layers_cl_list (list 
      layer_name_cl_socket 
      layer_name_cl_dect
      layer_name_cl_wf) 
    fu (lambda(x y) ;search lwpline in x layers? with y hyperlink 
      (sslength 
          (ssget "X" 
            (list 
              (cons 0 "lwpolyline") 
              (cons 8 x) 
              (list -3 
                (list "PE_URL" 
                  (cons 1000 y)))))))


    cl_count_list (mapcar '(lambda(x) (fu x "##/##.##")) layers_cl_list))

  (print (srch_layer layer_name_cl_socket)) (princ "\n")
  (print (srch_layer layer_name_cl_dect))   (princ "\n")
  (print (srch_layer layer_name_cl_wf))     (princ "\n")


  (princ 
    (strcat "всего линий в слоях " "_CL.TC" str_n "* : " (itoa (fu (strcat "_CL.TC" str_n "*") "##/##.##")) "\n"))
  (princ 
    (strcat "всего линий c маркировкой "  str_n "/##.## : " (itoa (fu "*" (strcat str_n "/##.##") )) "\n"))

  (mapcar '(lambda(x y) 
    (princ  
      (strcat "всего линий в слоях " x " : " (itoa y) "\n"))) layers_cl_list cl_count_list)
  (princ (strcat " суммарно sck&dect&wf : " (itoa (apply '+ cl_count_list)) "\n"))

  its_nothing.)

(defun chee (n)

  "Documentation for chee."
  ())


(defun optimizilla (parameters)
  "Documentation for optimizilla."
  (setq 
    tc (list
      (list "TC11" '(10 35608  24672  0))
      (list "TC12" '(10 83559  24709  0))
      (list "TC13" '(10 59460  71222  0))
      (list "TC14" '(10 59460  119170 0)))
    ; (list "TC21" '(10 155598 36663  0))
    ; (list "TC22" '(10 227763 36669  0))
    ; (list "TC23" '(10 203720 119153 0))
    ; (list "TC24" '(10 155522 119132 0))
    ; (list "TC25" '(10 108584 84457  0)))
    ; all_blk_l  (ss_2_list(ssget "x"  (list (cons 0 "INSERT"))))
    all_blk_l  (ss_2_list(ssget "W" (getpoint) (getpoint) (list (cons 0 "INSERT"))))
    dev_tk_l   (filter_list_f all_blk_l get_block_record_name (cons 2 "Device_CCTV_1"))
    fu_tk_tc_distance (lambda(x y) (list (car y) (get_orto_distance (assoc 10 (entget x)) (cadr y)))))
; (print (length dev_tk_l))
 (print (length dev_tk_l))
;(print (get_orto_distance (assoc 10 (entget (car dev_tk_l))) (cadr (car tc))))
;(fu_tk_tc_distance (car dev_tk_l) (car tc))
(mapcar '(lambda(x)(mapcar '(lambda(y)(fu_tk_tc_distance x y )) tc)) dev_tk_l)

)

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


  (setq 
    sck_link_l (mapcar '(lambda(x) (cdr  (fun_num_link x))) sck_link)
    sck_dev_l  (mapcar '(lambda(x) (cdr  (fun_num_dev x ))) sck_dev)

    wf_link_l (mapcar '(lambda(x) (cdr  (fun_num_link x))) wf_link)
    wf_dev_l (mapcar '(lambda(x) (cdr  (fun_num_dev x ))) wf_dev)

    de_link_l (mapcar '(lambda(x) (cdr  (fun_num_link x))) de_link)
    de_dev_l (mapcar '(lambda(x) (cdr  (fun_num_dev x ))) de_dev)
    )
  (print  "socket link")
  (print (length sck_link_l))
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



(defun rechecker ( / parameters)
  "Documentation for rechecker."
  (setq 
    all_blk_l  (ss_2_list(ssget "x"  (list (cons 0 "INSERT"))))
    all_link_l (ss_2_list (ssget "x" (list (cons 0 "lwpolyline") (list -3 (list "PE_URL")))))
    dev_tc_l     (filter_list_f all_blk_l get_block_record_name (cons 2 "Device_TC"))
    dev_socket_l (filter_list_f all_blk_l get_block_record_name (cons 2 "Device_socket"))
    dev_wifi_l   (filter_list_f all_blk_l get_block_record_name (cons 2 "Device_WiFi"))
    dev_dect_l   (filter_list_f all_blk_l get_block_record_name (cons 2 "Device_DECT")))

  (print (length all_blk_l))
  (print (length all_link_l))


  (print (length dev_dect_l))
  (print (length dev_wifi_l))
  (print (length dev_socket_l))

  (mapcar 
    '(lambda (x) 
      (progn 
        (setq
          a (cdr (assoc 1 (search_ent x (list (cons 2 "P_TAG_SKS"))))))
        ( get_link_spec (srch_lwpline_by_mark a))
        ))
      dev_socket_l))


;хочешь поменять атрибуты в блоке ну вот тебе
; (setq fua (lambda(x) (entmod (subst (cons 1  (strcat "QF" x)) (setq b (assoc 1 (setq a (entget (car(nentsel)))))) a))))
; (mapcar fua '(1 2))



;(srch_lwpline_by_mark "11/11.11,12/11.11") -> selection set
;(srch_lwpline_by_mark "##/0[12].11") -> selection set ; ##/01 or 02 .11 
(defun srch_lwpline_by_mark( a )
  "Documentation for srch_lwpline_by_mark. search lwpolyline with hyperlink mark"
  (print a)
  (ssget "X" (list (cons 0 "lwpolyline") 
    (list -3 (list "PE_URL" 
      (cons 1000 (pm_mark a)))))))


;(get_link_spec ss) -> list (((mark . "hyperlink")(layer . "layer") (length . "lwplinelength")) ...)
(defun get_link_spec (a / b g )
  (setq 
    b (ss_2_list a))

  (mapcar 
    '(lambda(x)
      (progn 
        (setq g (entget x '("PE_URL")))
        (list 
          (cons 'mark (cdr(cadr(cadr(assoc -3 g )))))
          (cons 'layer (cdr (assoc 8 g)))
          (cons 'length (/ (fix (get_lwpline_length x)) 1000))
          ))) b))


(defun pm_mark (a)
  "Documentation for pm_mark."
  (cond
    ((wcmatch a "##/##.##")      (print a))
    ((wcmatch a "##/##.##`,##")  (print (strcat (substr a 1 8) "," (substr a 1 6) (substr a 10))))
    ((wcmatch a "##/##.##...##") 
      (print 
        (apply 'strcat
          (mapcar 
            '(lambda (x) (strcat (substr a 1 6)  (add_0 x) ","))
            (gen_list (atoi(substr a 7 8 )) (atoi(substr a 12 13)))))))
    (t (print a))
        ))


(defun diff_lists (a b / a1)
  "Documentation for diff_lists."
  (setq 
    head_a (car a)
    head_b (car b)
    tail_a (cdr a)
    tail_b (cdr b))

  (cond 
    (
      (= head_a head_b)
      (diff_lists tail_a tail_b)
      )

    )
  )
; (cadr(cadr(assoc -3 (entget (car (entsel)) '("PE_URL")))))
