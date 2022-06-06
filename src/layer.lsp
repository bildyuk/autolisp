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
      (/= (wcmatch (strcase layer_name) (strcase layer_name_pattern)) T)
      (progn    
        (srch_layer layer_name_pattern)) )
    (
      (= (wcmatch (strcase layer_name) (strcase layer_name_pattern)) T)
      (progn
       ; (print layer_name)
        (append 
          (list layer_name)
          (srch_layer layer_name_pattern))) )))