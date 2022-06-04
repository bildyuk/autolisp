(defun get_pwline_by ( / parameters)
  "Documentation for get_pwline."
  (setq all_pwline (ssget "_W" (getpoint) (getpoint) (list (cons 0 "lwpolyline") (list -3 (list "PE_URL")))))
(ss_2_list all_pwline)
)


; (cadr(cadr(assoc -3 (entget (car (entsel)) '("PE_URL")))))




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




  