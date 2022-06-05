
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


; (add_0 9) -> "09" ; (add_0 10) -> "10"
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
