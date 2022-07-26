

;вставить текст из MTEXT в ATTRIB блока
(setq fu_mu (lambda () (progn (entmod (subst (assoc 1 (entget(car(ENTSEL)))) (assoc 1 (setq a (entget(car(NENTSEL))))) a)) (fu_mu))))
; run -> (fu_mu)


(defun srch_blk ()
	"Documentation for srch_blk."	
	(setq 
		;blk_l (list (cdr(assoc -2 (tblnext "block" t))))
		blk_name (cdr (assoc 2 (tblnext "block" t)))
		a '())
	(while (/=  blk_name nil )

		(setq 
			a (append a (list blk_name))
			blk_name (cdr (assoc 2 (tblnext "block"))))
		)a)

; (defun c:coloroz ()
; 	"Documentation for name."
; 	(print (setq a (mapcar '(lambda(x) (cdr(assoc 2 (entget x)))) (ss_2_list (SSget "_I" (list (cons 0 "INSERT")))))))
	
; 	(setq b (mapcar '(lambda(x) (cdr(assoc -2 (tblsearch "block" x)))) a ))
; 	(mapcar 'srch_ent b))


(defun c:color_by_layer_in_blks ()
	"Documentation for color_by_layer_in_blks."
	(mapcar 'color_by_layer_in_blk (srch_blk)))


(defun color_by_layer_in_blk ( blk_name)
	"Documentation for color_by_layer_in_blk."
		(setq 
		a (print (tblsearch "block" blk_name))
		ent_name (print (cdr(assoc -2 a))))
	;(setq a '())
	(while (/=  ent_name nil )
		(setq 
			entity_desc (entget ent_name))
		(cond 
			((/= (setq color (assoc 62 entity_desc)) nil)
			(print (entmod (vl-remove (cons 420  (cdr(assoc 420 entity_desc))) (append entity_desc '((62 . 256)))))))
			(t (print ent_name)))
		;(setq a (append a (list ent_name)))
		(setq ent_name (entnext ent_name))
		))



(defun c:color_by_layer ()
	"Documentation for c_by_layer."
	(setq 
		ss (print (ssget "x"))
		ss_head (print (ssname ss 0)))

	(while (/= ss_head nil)
		(setq 
			entity_desc (entget ss_head))

		(cond 
			((/= (setq color (assoc 62 entity_desc)) nil)
				(print (entmod (vl-remove (cons 420  (cdr(assoc 420 entity_desc))) (append entity_desc '((62 . 256)))))))
			(t (print ent_name)))
		(setq 
			ss (print (ssdel ss_head ss))
			ss_head (print (ssname ss 0)))))


(defun c:del_hatch_solid_3Dsolid_in_blks ()
	"Documentation for del_hatch_in_blks."
	(mapcar 'del_hatch_solid_3Dsolid_in_blk (srch_blk)))


(defun del_hatch_solid_3Dsolid_in_blk (block_name / a b c p_70)
	"Documentation for del_hatch_solid_3Dsolid_in_blk."
	(setq 
		a (print (tblsearch "block" block_name))
		b (print (cdr(assoc -2 a)))
		p_70 (print (cdr(assoc 70 a))))

	(cond 
		((print (or (= p_70 0) (= p_70 2)))
			(progn
				(print (entmake (list (cons 0 "block") (cons 2 block_name) (list 10 0 0 0)(cons 70 p_70))))
				(while (/= b nil)
					(setq c (print(entget b)))
					(if  (and 
						 (/= (cdr(assoc 0 c)) "HATCH")
						 (/= (cdr(assoc 0 c)) "3DSOLID")
						 (/= (cdr(assoc 0 c)) "SOLID")) (entmake c))
					(print (setq b (entnext b)))
					)
				(entmake (list (cons 0 "endblk")))))))


;"HATCH" "3DSOLID" "SOLID"
(defun c:del_hatch_solid_3Dsolid ()
	"Documentation for del_hatch."
	(del_primitives "HATCH")
	(del_primitives "solid")
	(del_primitives "3dsolid"))

(defun del_primitives (typ_prim)
	"Documentation for del_primitives."
	(setq 
		ss (print (ssget "x" (list (cons 0 typ_prim))))
		ss_head (print (cond (ss (ssname ss 0)))))

	(while (/= ss_head nil)

		(setq 
			ss (print (ssdel ss_head ss))
			hatch (print (entdel ss_head))
			ss_head (print (ssname ss 0))
			)
		))

