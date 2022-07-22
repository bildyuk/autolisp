
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

 ; (tblobjname "block" "Device_CCTV_1")
 ; (hadent "5ce")
 ; (ssget "x" (List(cons 2  "`*U*"))))

 ; annonimous block access to her block record



(defun get_block_record_name (entity_name / a b c d)
  "Documentation for get_block_record_name. определяет название анонимного блока"
  (setq
     din_blk (wcmatch (cdr (assoc 2 (entget entity_name))) "`*U*"))
  (print (entget entity_name))
    (cond
      (
        din_blk ;if true 
        (progn 
          (setq 
            a (entget (cdr (assoc 360 (member '(102 . "{ACAD_XDICTIONARY") (entget entity_name)))))
            b (entget (cdr (assoc 360 (member '(3 . "AcDbBlockRepresentation") a))))
            c (entget (cdr (assoc 360 (member '(3 . "AcDbRepData") b))))
            d (entget (cdr (assoc 340 c))))
          (assoc 2 d)))
        (t (assoc 2 (entget entity_name)))))


;(mapcar '(lambda (x) (entmod (subst (cons 1 (strcat "QF" x)) (assoc 1 (setq a (entget(car(nentsel))))) a))) '("01" "02"))

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


(defun rot_orto ( / a b c)
  "Documentation for rot_orto."
  (setq 
    a (entget(car(nentsel)))
    b (assoc 50 a)
    c (cons 50 (+ (cdr b) (/ pi 2)))
    e (cdar (entmod (subst c b a))))
  (entupd e))


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

