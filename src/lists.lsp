
; ss -> list 
(defun ss_2_list (ss / head tail)
  "Documentation for ss_2_list.
  превращает selection set в list "
  (setq 
    head (ssname ss 0)
    tail (ssdel head ss))
  (cond
    (
      (> (sslength tail) 0) 
      (progn
        (print head)
        (append (list head ) (ss_2_list tail)) ))
    (
      (= (sslength tail) 0)
      (progn
        (print head)
        (append (list head ) '()))) ))
  
   ;(filter_list link_list (cons 8 "_CL.TC##*"))
(defun filter_list (list_ent pattern / head tail)
  "Documentation for filter_list. фильтрует список(entity name) по паттерну "
  (setq 
    a (car pattern)
    b (cdr pattern)

    head (car list_ent)
    tail (cdr list_ent)
    c (cdr (assoc a (entget head))))
    ; (print (length list_ent))
  (cond
    (
      (/= (length tail) 0) 
      (cond
        (
          (wcmatch c b)
          (append (list head) (filter_list tail pattern)))
        (t (append (list) (filter_list tail pattern))))
        )))

; (length (filter_list_f l get_block_record_name (cons 2 "Лот*")))
(defun filter_list_f (list_ent  fu pattern / head tail)
  "Documentation for filter_list. фильтрует список(entity name) функцией"
  (setq 
    a (car pattern)
    b (cdr pattern)

    head (car list_ent)
    tail (cdr list_ent)
    ; c (cdr (assoc a (entget head)))
    c (car(fu head))
    d (cdr (fu head)))
     (print d)
  (cond
    (
      (/= (length tail) 0) 
      (cond
        (
          (wcmatch d b)
          (append (list head) (filter_list_f tail fu pattern)))
        (t (append (list) (filter_list_f tail fu pattern))))
        )))
  
; (cadr(cadr(assoc -3 (entget (car (entsel)) '("PE_URL")))))




  