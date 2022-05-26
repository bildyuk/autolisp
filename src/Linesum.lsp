(defun C:LINESUM ()
  (setq sset (ssget '((0 . "LINE"))))
  (if sset
    (progn
      (setq tot 0.0)
      (setq num (sslength sset) itm 0)
      (while (< itm num)
        (setq hnd (ssname sset itm))
        (setq ent (entget hnd))
        (setq pt1 (cdr (assoc 10 ent)))
        (setq pt2 (cdr (assoc 11 ent)))
        (setq dis (distance pt1 pt2))
        (setq tot (+ tot dis))
        (setq itm (1+ itm))
      )
      (princ (strcat "\nTotal Distance = " (rtos tot 2 2 )))
    )
  )
  (princ)
)


(defun C:pwlinessum ( / sset sslen lentot i )
  (setq 
    sset (ssget '((0 . "LWPOLYLINE")))
    sslen (sslength sset)
    i -1
    lentot 0
    )
  (repeat sslen
    (setq
      i (+ i 1)
      ssel (entget (ssname sset i))
      lentot (+ lentot (lwcoord ssel))
      )
    )
  (princ (strcat "\nTotal length = " (rtos lentot 2 2)))
  (princ)
)



(defun lwcoord (e / len n e1 lenx vrtxl vrtx)
  ; (setq e (entget (car (entsel))))
  ;get the entity list
  (setq len (length e))
  ;get the length of the list
  (setq n 0)
  (setq lenx 0)
  ;set counter to zero
  (repeat len
  ;repeat for the length of the entity list
    (setq e1 (car (nth n e)))
    ;get each item in the entity list
    ;and strip the entity code number
    (if (= e1 10)
    ;check for code 10 (vertex)
      (progn
      ;if it's group 10 do the following
    (terpri)
      ;new line
      
      (setq vrtx (cdr (nth n e)))   
      (if (= vrtxl nil) (setq vrtxl vrtx))
      (setq lenx (+ lenx (distance vrtxl vrtx)))
       
       ; (princ lenx)
       (setq vrtxl vrtx)
      ;print the co-ordinates
      );progn
    );i
    (setq n (1+ n))
    ;increment the counter
  );repeat
    lenx
);defun


; (defun c:create_blk ( / a )
;   (setq a 
;     (list 
;       (cons 0 "INSERT")
;       (cons 5 "41F") 
;        ;(102 . "{ACAD_XDICTIONARY")
;        ;(360 . <Entity name: 7fb51562b2d0>) 
;        ;(102 . "}") 
;        ;(330 . <Entity name: 7fb51a9b19f0>)
;       ; (cons 100 "AcDbEntity")
;       ; (67 . 0) (410 . "Model") (8 . "0")
;       ; (100 . "AcDbBlockReference") (66 . 1)
;       ; (2 . "IR_sensor") (10 50.8861 36.0963 0.0) (41 . 1.0) (42 . 1.0) (43 . 1.0)
;       ; (50 . 0.0) (70 . 0) (71 . 0) (44 . 0.0) (45 . 0.0) (210 0.0 0.0 1.0))
;   )

;   )
; )


(defun c:circ()

  (entmake
  (list
   (cons 0 "BLOCK")
   (cons 2 "circ_blk")
   (cons 10 '(0.0 0.0 0.0))
   (cons 70 2)
  )
 )
  (entmake
  (list
   (cons 0 "CIRCLE")
   (cons 8 "0")
   (cons 10 '(0.0 0.0 0.0))
   (cons 40 10)
   ; (cons 62 objColor)
  )
 )
  (entmake
  (list
   (cons 0  "ENDBLK")
   (cons 8  "0")
  )
  )
  )



(defun c:Create_Test_Point_Blocks ()
 (setq blockName "TestPoint"
       objLayer "Test_Points"
       objColor 62
       tpCircleRad 300
       tpNumberTxtHgt 150
       padNumTxtHgt 125
       padNumTxtLoc (* tpNumberTxtHgt -1.0)
 )
 (entmake
  (list
   (cons 0 "BLOCK")
   (cons 2 blockName)
   (cons 10 '(0.0 0.0 0.0))
   (cons 70 2)
  )
 )
 (entmake
  (list
   (cons 0 "CIRCLE")
   (cons 8 objLayer)
   (cons 10 '(0.0 0.0 0.0))
   (cons 40 tpCircleRad)
   (cons 62 objColor)
  )
 )
 (entmake
  (list
   (cons 0 "ATTDEF")
   (cons 1 "")
   (cons 2 "TEST_POINT_NUMBER")
   (cons 3 "Enter Test Point Number")
   (cons 8 objLayer)
   (cons 10 '(0.0 0.0 0.0))
   (cons 70 0) ;Attribute flags, 1 = Attribute is invisible
   (cons 72 1) ;Horizontal text justification, 1 = Center
   (cons 74 2) ;Vertical text justification, 2 = Middle
   (cons 40 tpNumberTxtHgt)
  )
 )
 (entmake
  (list
   (cons 0 "ATTDEF")
   (cons 1 "")
   (cons 2 "PAD_NUMBER")
   (cons 3 "Enter Pad Number")
   (cons 8 objLayer)
   (cons 10 (list 0.0 padNumTxtLoc 0.0)) ;Text start point
   (cons 40 padNumTxtHgt)
   (cons 70 1) ;Attribute flags, 1 = Attribute is invisible
   (cons 72 1) ;Horizontal text justification, 1 = Center
   (cons 74 2) ;Vertical text justification, 2 = Middle
  )
 )
 (entmake
  (list
   (cons 0  "ENDBLK")
   (cons 8  "0")
  )
 )
)




(defun c:get_mtext ( / elem)
(setq 
  elem (car (entsel))
  ed (entget elem)
  )
(cdr (assoc 1 ed))
;"{\\fArial Black|b0|i0|c204|p0;jnjnhjhnjnj}"
; (setq ed (subst (cons 8 "1") (assoc 8 ed) ed ))
; (entmod ed)
  )

(defun c:parse_mtext ( / elem)
(setq 
  elem (car (entsel))
  ed (entget elem)
  a (cdr (assoc 1 ed)) ; return "{\\fArial Black|b0|i0|c204|p0;jnjnhjhnjnj}"
  )
(if (= 0 (vl-string-search "{" a))
  (progn
    (pars (vl-string-left-trim "{" a))
  )
)
; (setq ed (subst (cons 8 "1") (assoc 8 ed) ed ))
; (entmod ed)
  )


; parse string
;  (vl-string->list a)
(defun pars ( a /) 
  (setq b (vl-string->list a))
(if (vl-string-search "\\f" a) 
  (progn
    (setq 
      a1 (vl-string-left-trim "\\f" (substr a  1 (vl-string-search ";" a)))
      a2 (vl-string-left-trim a1 (vl-string-left-trim "\\f" a))
        )
))

(if (vl-string-search ";" a2) 
  (progn
    (setq 
      b1 (vl-string-left-trim ";" (substr a2  1 (vl-string-search "\\f" a2)))
      a3 (vl-string-left-trim b1 a2)
        )
))
(list (cons a1 b1) a3)
)


(defun TitleCase ( s / a b c)
    ; (vl-list->string
        (mapcar
            (function
                (lambda ( a b c) 
                  (if (= a 92)
                    (if (= b 102)
                      nil ) 
                    c
                    )

                )
            )
            (vl-string->list s)     ; a
            (cons 32 (cdr (vl-string->list   s))) ; b
            (cons 32 (cons 32 (cdr(cdr (vl-string->list  s))))) ; c
        )
    ; )
)
; \\ 92
; {  123
; }  125
; f  102
; ;  59


(defun pstr ( a / )
(list "{")
  (while (/= nil (vl-string-search "\\f" a))
  (setq 
      out (list "{")
      an (substr a 1 (+ 2 (vl-string-search "\\f" a)))
      a  (vl-string-left-trim an a)
        )
  (cons an out)
  )
  (cons a out)
  )


(defun pp ( l )
   (setq
       l (reverse (vl-string->list l)))
(ppp l)
   )
(defun ppp ( l / )

  (setq tail (cdr l))

  (if (/= nil tail) 
    (ppp tail))
  (print (car l))
  (princ)
  )

(defun ppparse ( l / i s)
  (setq
    i 0
    s nil    
    )
  (repeat (length l)
    
  (setq 
    s (append (list (compare (nth i l))) s)
    ; (list (nth i l))
    i (+ 1 i))
  ; (print (compare (nth i l)))
  )
   s
  )


(defun compare ( el / a i s)
; \\ 92 ; {  123 ; }  125 ; f  102 ; ;  59
  (setq
    i 0
    a '(59 92 102 123 125)
    s nil
    )
  (if (/= nil (member el a)) 
    el 0)
  )

(defun ppparse` ( l1 l2 / i s)
  (setq
    i 0
    s nil    
    )
  (repeat (length l2)

    (if (/= nil (member 92 l2)) 
      el 0)

    (setq
      i  (i (+ 1 i)))
  )
  )

; (defun LM:TitleCase ( s )
;     (vl-list->string
;         (mapcar
;             (function
;                 (lambda ( a b c ) (if (= 32 a) b c))
;             )
;             (cons 32 (vl-string->list s))
;             (vl-string->list (strcase s))
;             (vl-string->list (strcase s t))
;         )
;     )
; )


; (vl-string-position 92 "azbzlmnqc" nil t)



(defun c:get_attr( / elem)
; get attributes
(setq elem (car (entsel)))
(while (/= nil (entnext elem))
  
  (entget elem)
  (setq elem (entnext elem))
  (princ (entget elem))
  (princ "\n")
  )
(princ)
)


(defun c:set_layer ( / elem)
(setq 
  elem (car (entsel))
  ed (entget elem)
  )
(setq ed (subst (cons 8 "1") (assoc 8 ed) ed ))
(entmod ed)
  )

(defun set_addr( elem addr /  edel attr_name blck_end)
; set addr
; (setq addr "12")
; (setq elem (car (entsel)))

(while (/= (cdr (assoc 0 (entget elem))) "SEQEND")
  ; (0 . SEQEND)

  (setq attr_name (assoc 2 (entget elem)))

   (setq edel (entget elem))
  (if (= (cdr attr_name) "ADDR") 
    (progn
    (setq edel (subst (cons 1 addr) (assoc 1 edel ) edel))
    ; (princ ed)
    (entmod edel)
    ))
  (setq elem (entnext elem))
   ; (princ (entget elem))
   ; (princ "\n")
  )
(princ)
)


(defun c:ssets ( / i sset sslen)
(setq 
  sset  (ssget)
  sslen (sslength sset)
  i     0
  )

(while (> sslen i)

  (setq ssel (ssname sset i))
  ; (princ i)
  (set_addr ssel (rtos i 2 0 ))
  (setq i (+ i 1))
  )
  )


(defun c:g_corner ( )

  (setq  
    pt1      (getpoint)
    pt3      (getcorner pt1)
    pt1_x    (car pt1)
    pt1_y    (cadr pt1)
    pt3_x    (car pt3)
    pt3_y    (cadr pt3)
    pt2      (list pt1_x pt3_y)
    pt4      (list pt3_x pt1_y)
    center_x (/ (+ pt1_x pt3_x) 2)
    center_y (/ (+ pt1_y pt3_y) 2)
    center   (list center_x center_y)
    )
  (print (*
    (distance pt1 pt2)
    (distance pt1 pt4)   
    ))

(princ)
  )

(defun c:coord_blk ( )

  (setq 
    dist 8.0
    h 10.0 ;высота квадрата
    w 10.0 ;ширина квадрата
    ac 2 ; кол-во малых отрезоков (первый и последний)
    bc (fix (/ h dist)) ; кол-во больших отрезков
    )

  (setq 
    abc (+ (* bc 2) 2)
    a   (atoi(rtos (/ h abc) 2 0))
    b   (/ (- h (* a 2)) bc)
    ; количество отрезков 
    )

; (setq
;    x (atoi(rtos (/ h abc) 2 0))
;    y (atoi(rtos (/ h abc) 2 0)))
  (list a b)
  ; ( atoi(rtos 12.6 2 0))
  )


(defun c:reload ()
  (load "Linesum.lsp")
  )

(defun c:list_generate ( template \ range)


  )




(defun split_str ( s d )
  ( (lambda (f) (if (/= "" d) (f s d "" nil)))
    (lambda ( s d c b / k )
      (if (/= s "")
        (if (= d (setq k (substr s 1 1))) 
          (append 
            (cond 
              ( (/= c "") (list c d) )
              ( (list d) )
            ) 
            (f (substr s 2) d "" t)
          )
          (f (substr s 2) d (strcat c k) b)
        )
        (if b (list c) c)
      )
    )
  )
); defun split



(defun c:put_attr (prefix x / s)
  (print x)
  ; (put_attr prefix (+ 1 x))
  )

(defun c:coods_vrtx_pline()
  (setq obj (entget (car(entsel))))
  (member(assoc 10 obj) obj)
  )


; модификация атрибута
; (setq a (entget(entnext(car(entsel))))
; (setq a (subst (cons 1 "12") (assoc 1 a) a))
; (entmod a)

