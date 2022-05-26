
(defun map_entity_desc( f blk_entity)
  "проход по всем вложеным вхождениям.
  и выполнить функцию f"
  
  (setq 
    ; blk_entity (car elem)
    blk_desc (entget blk_entity)
    blk_type (cdr (assoc 0 blk_desc))
    blk_name (cdr (assoc 2 blk_desc))
    )
  (print (entnext blk_entity))
  (print blk_type)
  (cond 
    (
      (= blk_type "INSERT")
      (progn
        (print blk_name)
        (map_entity_desc  f (entnext blk_entity))))

    (
      (= blk_type "ATTRIB")
      (progn
        (f blk_desc)
        (map_entity_desc f (entnext blk_entity))))
    (
      (= blk_type "SEQEND")
      )
    ( t (print "dont't know todo"))))


;(ssget "x" (list(list -3 (list "PE_URL"))))
;  (ssget "x" (list (cons 0 "lwpolyline") (list -3 (list "PE_URL"))))
; (wcmatch "_CL.TC11 0.000" "_CL.TC*")

 ; (sslength (ssget "x" (list (cons 0 "lwpolyline") (cons 8 "_CL.TC13 0.000")(list -3 (list "PE_URL")))))


; (srch_layer "*TC*") -> список слоев удовлетворяющй паттерну
(defun srch_layer (layer_name_pattern)
  "Documentation for srch_layer."
  (setq 
    layer_name (cdr(assoc 2 (tblnext "layer"))))
  ;(print layer_name)
  (cond 
    (
      (= layer_name nil)
      (tblnext "layer" t) 
      end.)
    (
      (/= (wcmatch layer_name layer_name_pattern) T)
      (progn    
        (srch_layer layer_name_pattern)) )
    (
      (= (wcmatch layer_name layer_name_pattern) T)
      (progn
        (print layer_name)
        (append 
          (list layer_name)
          (srch_layer layer_name_pattern))) )))

; суммируем длину линий из набора ss
(defun set_lwpline_ss ( ss)
  "Documentation for set_lwpline_ss."
  (setq 
    tail (cdr ss))
  ; (cadar (ssnamex ss))  ; первое entity name из ss
  (cond
        (
          (/= tail nil)  
          (+ (get_lwpline_length (cadar ss)) (set_lwpline_ss (cdr ss))))
        (
          (= tail nil)
          0)))


; (get_lwpline_length (car(entsel)))
(defun get_lwpline_length (entity_name / AcDbPolyline vrtx_n)
  "Documentation for lwpline. вычисляет длину полилинии"
  (setq
    AcDbPolyline (member (cons 100 "AcDbPolyline")(entget entity_name))
    vrtx_n (cdr(assoc 90 AcDbPolyline)))
  (get_vrtx_dist (get_vrtx_list AcDbPolyline)))


(defun get_vrtx_list (a / head tail x y )
   "Documentation for vrtx. получить список вершин из AcDbPolyline"
   (setq 
    head (car(member (assoc 10 a) a))
    tail (cdr(member (assoc 10 a) a))
    x (cadr head)
    y (caddr head)
    )
   ; (print head)
   (cond
        (
          (/= tail nil)  
          (append (list (list x y)) (vrtx tail)))))


; (get_vrtx_dist '((0 0)(1 0) (1 1)(0 1)(0 0)))
(defun get_vrtx_dist (vrtx_list)
  "Documentation for get_vrtx_dist. посчитать длину списка вершин ((1 1) (2 2)(3 3))"
  (setq 
    tail (cdr vrtx_list))
  ; (print tail)
  (cond
        (
          (/= tail nil)  
          (+ (distance (car vrtx_list) (cadr vrtx_list)) (get_vrtx_dist tail)))
        (
          (= tail nil)
          0)))


 ; (tblobjname "block" "Device_CCTV_1")
 ; (hadent "5ce")
 ; (ssget "x" (List(cons 2  "`*U*"))))

 ; annonimous block access to her block record



(defun get_block_record_name (entity_name / a b c d)
  "Documentation for get_block_record_name. определяет название анонимного блока"
  (setq 
    a (entget (cdr (assoc 360 (member '(102 . "{ACAD_XDICTIONARY") (entget entity_name)))))
    b (entget (cdr (assoc 360 (member '(3 . "AcDbBlockRepresentation") a))))
    c (entget (cdr (assoc 360 (member '(3 . "AcDbRepData") b))))
    d (entget (cdr (assoc 340 c))))
   (assoc 2 d))

; (search_ent(car(entsel)) (list (cons 2 "ADDR")(cons 1 "TC01/01.02")))
(defun search_ent (entity_name srch_tuple_lst / entity_desc entity_type srch_a srch_b)
  "Documentation for search_ent.
  entity_name like as: <Entity name: 7ff2d43e2590> returned on (car(entsel))"
 ; ent_type ent_name ent_value
 ; ent_type ex: (0 . "ATTRIB")
 ; ent_name ex:  (2 . "ADDR") 
 ; ent_value ex: (1 . "01.02") 
 (setq 
    entity_desc (entget entity_name)
    )
 (foreach n srch_tuple_lst 
    (progn
      (setq 
        entity_type (cdr(assoc 0 entity_desc))
        srch_a (car n)
        srch_b (cdr n))
      (cond 
        (
          (= entity_type "SEQEND")                            ; дошли до конца цепочци атрибутов блока и ;отключив ее пройдется по всем облокам в модели
          nil)                                                ; вернули 
        (
          (= nil (cdr(assoc srch_a entity_desc)))             ; если искомый параметр не нашли в текущем вхождении 
          (search_ent (entnext entity_name) srch_tuple_lst))  ; пошли дальше по цепочке
        (    
            (= srch_b (cdr(assoc srch_a entity_desc)))        ; если нашли то, что искали 
             entity_desc                                      ; вернули 
            )
        (
          (/= entity_type "SEQEND")                           ; выше ничего не подошло и не дошли до конца, и 
          (search_ent (entnext entity_name) srch_tuple_lst)   ; пошли дальше по цепочке
        ))
  )))


; upd_entity_desc( (entget) 2 "12")
(defun upd_entity_desc( entity_desc attr value )
  "в примере меняет в дескрипции вхождения (2 . какое-то значение) на (2. 12) и обновляет само вхождение"
  (print value)
  (setq
    blk_name (cdr (assoc 2 entity_desc))
    )
  (setq 
    entity_desc 
    (subst 
      (cons attr value) 
      (assoc attr entity_desc) 
      entity_desc)
    )
  (entmod entity_desc))


; (cycle "aa." 1 24) -> "aa.01" ..."aa.24"
(defun cycle( prefix start end)
  "цикл смены атрибута в блоках"
 (cond
  ((<= start end) 
    (progn 
      ; (print (strcat prefix (add_0 start)))
      (upd_entity_desc 
        ;(entget(entnext(car(entsel))))
        (search_ent(car(entsel)) (list (cons 2 "NOTE")))
        1 
        (marker prefix start) )
      (cycle prefix (+ start 1) end)
      ))
  (t "ok")))


; (add_0 9) -> "09"
; (add_0 10) -> "10"
(defun add_0 ( i )
  (cond 
    ((< i 10)
      (strcat "0" (itoa i)))
    (t (itoa i))))


(defun iterator (fun arg start end)
  "Documentation for iterator."
  (cond
  ((<= start end) 
    (progn 
     ; (print(fun arg start))
     (print start)
      (iterator fun arg (+ start 1) end)
      ))
  ; (t "pp")
  ))


; (mark "TC" 1 10)
(defun mark (marker prefix start end)
  "Documentation for mark."
  (iterator marker ))


;     (marker "aa." 1) -> "aa.01"
(defun marker (prefix id) 
  "создает маркировочную строку из префикса(string) и id(int) -> (string)"
  (strcat prefix (add_0 id)))


(defun marker_2 (prefix id suffix) 
  "создает маркировочную строку из префикса(string) id(int) суффикса(string) -> (string)"
  (strcat prefix (add_0 id) suffix))




; (gen_list '() 1 5)'
(defun gen_list (list_ start end)
   "Documentation for gen_list."
   (cond
    (
      (<= start end)
      (progn
        (gen_list (cons start list_) (+ 1 start) end)))
    (t (reverse list_))))
  
   

(defun map_test ()
  "Documentation for map_test."
  (setq p (mapcar '(lambda (x)  (strcat "s" (add_0 x) ))  (list 1 2 7)))
  (mapcar '(lambda (x)  (strcat "s" (add_0 x) )) p)
  )


(defun cyclon ( f list_)
  "Documentation for cyclon."
  (setq li_ '())

  (foreach n list_ 
    (setq 
      li_ (cons (f n) li_))
    )
  (reverse li_)) 




; (mapcar '(lambda (x)  (marker "r" x) )  (list 1 2 7))

; (vl-string-subst  "TC02" "TC01" "TC01/01.12" ) -> "TC02/01.12"

; модификация атрибута
; (setq a (entget(entnext(car(entsel))))
; (setq a (subst (cons 1 "12") (assoc 1 a) a))
; (entmod a)
