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
  (setq 
    tail (cdr _list))
  ; (cadar (ssnamex ss))  ; первое entity name из ss
  (cond
        (
          (/= tail nil)  
          (+ (get_lwpline_length (car _list)) (get_lwpline_length_l (cdr _list))))
        (
          (= tail nil)
          (get_lwpline_length (car _list)))))




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




  