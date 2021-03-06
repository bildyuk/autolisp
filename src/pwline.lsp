(defun get_pwline_w_url ( / parameters)
  "Documentation for get_pwline."
  (setq all_pwline (ssget "_W" (getpoint) (getpoint) (list (cons 0 "lwpolyline") (list -3 (list "PE_URL")))))
(ss_2_list all_pwline)
)


; (cadr(cadr(assoc -3 (entget (car (entsel)) '("PE_URL")))))

;(-3 ("PE_URL" (1000 . "17/11.11") (1002 . "{") (1000 . "17/11.11") (1002 . "{") (1071 . 0) (1002 . "}") (1002 . "}"))))
;(sslength (ssget "X" '(( 0 . "lwpolyline") ( -3 ("PE_URL" (1000 . "14/##.##"))))))

; суммируем длину линий из набора ss
(defun get_lwpline_length_ss ( ss)
  "Documentation for get_lwpline_length_ss."
  (get_lwpline_length_l (ss_2_list ss)))

; суммируем длину линий из  list
(defun get_lwpline_length_l ( _list / tail)
  "Documentation for get_lwpline_length_l."
(apply '+ (mapcar 'get_lwpline_length _list)))


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
          (append (list (list x y)) (get_vrtx_list tail)))))


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


;get_orto_distance (10 1 1 0) (10 2 2 0)
(defun get_orto_distance (a b)
  "Documentation for get_orto_distance."
  (setq 
    x1 (cadr a) 
    y1 (caddr a)
    x2 (cadr b) 
    y2 (caddr b)
    diff_x (abs (- x1 x2))
    diff_y (abs (- y1 y2))
    distance (+ diff_x diff_y)
    weight (abs (- diff_x diff_y)))
    (list weight distance))


(defun xline (parameters)
  "Documentation for xline."
  (setq 
    a (get_vrtx_list (entget(car (nentsel))))
    b (get_vrtx_list (entget(car (nentsel))))
    a_x1 (caar a)  a_y1 (cadar a) 
    a_x2 (caadr a) a_y2 (cadadr a)
    b_x1 (caar b)  b_y1 (cadar b) 
    b_x2 (caadr b) b_y2 (cadadr b)
    delta 10)

(print (list a_x1 a_x2 a_y1 a_y2))
(print (list b_x1 b_x2 b_y1 b_y2))
(print (abs (- a_x1 a_x2)))
(print (abs (- b_y1 b_y2)))
  (print (abs (- b_x1 b_x2)))
    (print (abs (- a_y1 a_y2)))
(print (and (= a_x1 a_x2) (= b_y1 b_y2)))
(print (and (= b_x1 b_x2) (= a_y1 a_y2)))
    
    (cond 
      (
        (and (<= (abs (- a_x1 a_x2)) delta) (<= (abs (- b_y1 b_y2)) delta)) (list a_x1 b_y1))
      ( 
        (and (<= (abs (- b_x1 b_x2)) delta) (<= (abs (- a_y1 a_y2)) delta)) (list b_x1 a_y1))
      )

  )
  