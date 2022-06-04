
; attr.lsp

(defun map_entity_desc( f blk_entity)
  "проход по всем вложеным вхождениям, и выполнить функцию f"
  
(defun get_block_record_name (entity_name / a b c d)
  "Documentation for get_block_record_name. определяет название анонимного блока"

; (search_ent(car(entsel)) (list (cons 2 "ADDR")(cons 1 "TC01/01.02")))
(defun search_ent (entity_name srch_tuple_lst / entity_desc entity_type srch_a srch_b)
  "Documentation for search_ent.
  entity_name like as: <Entity name: 7ff2d43e2590> returned on (car(entsel))"
 

; upd_entity_desc( (entget) 2 "12")
(defun upd_entity_desc( entity_desc attr value )
  "в примере меняет в дескрипции вхождения (2 . какое-то значение) на (2. 12) и обновляет само вхождение"
  

; (cycle "aa." 1 24) -> "aa.01" ..."aa.24"
(defun cycle( prefix start end)
  "цикл смены атрибута в блоках"

; (add_0 9) -> "09"
; (add_0 10) -> "10"
(defun add_0 ( i )
  (cond 
    ((< i 10)
      (strcat "0" (itoa i)))
    (t (itoa i))))


(defun iterator (fun arg start end)
  "Documentation for iterator."
 

; (mark "TC" 1 10)
(defun mark (marker prefix start end)
  "Documentation for mark."
  (iterator marker ))


;     (marker "aa." 1) -> "aa.01"
(defun marker (prefix id) 
  "создает маркировочную строку из префикса(string) и id(int) -> (string)"
  

(defun marker_2 (prefix id suffix) 
  "создает маркировочную строку из префикса(string) id(int) суффикса(string) -> (string)"
  

; (gen_list '() 1 5)'
(defun gen_list (list_ start end)
   "Documentation for gen_list."
 

(defun map_test ()
  "Documentation for map_test."
 
(defun cyclon ( f list_)
  "Documentation for cyclon."
  