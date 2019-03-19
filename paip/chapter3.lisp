


;;;;;;;;;;;;;;;; Chapter 3


;;;ex3.3
(defun -print (l)
  (if (null l)
      '()
      (if (atom (car l))
	  (cons (format nil "~a" (car l))
		  (-print (cdr l)))
       (append (-print (car l))
	       (-print (cdr l))))))

(defun PRN (l)
  (let ((o (-print l)))
    (format nil "(~A)" o)))

(defun prn! (l)
  (labels ((-prn (l)
	     (cond ((null l) "")
		   ((and (atom (car l)) (listp (cdr l)))
		    (if (null (cdr l))
			(format nil "~a" (car l))
			(concatenate 'string (format nil "~a" (car l)) " " (-prn (cdr l)))))
		   ((and (atom (car l)) (atom (cdr l)))
		    (concatenate 'string (format nil "~a" (car l)) " . " (format nil "~a" (cdr l))))
		   (t (concatenate 'string (-prn (car l)) (-prn (cdr l)))))))
    (concatenate 'string "(" (-prn l) ")"))) ;;; fuuuugly AND wrong


;; prn [] = ()
;; prn [atom a] = (a)
;; prn [atom a:atom b] = (a . b)
;; prn [atom x:xs] = a " " (prn xs)
;; prn [list x:xs] = ( prn x ) " " (prn xs)
(defun prn!! (l)
  (cond ((null l) "NIL")
	((atom l) (format nil "~a" l))
	((atom (cdr l)) (concatenate 'string "(" (prn!! (car l)) " . " (format nil "~a" (cdr l)) ")"))
	(t (concatenate 'string "(" (prn!! (car l)) " " (prn!! (cdr l)) ")"))))
;; wrong but at least regular output

(defun atos (a) (format nil "~a" a))
(defun cat (&rest xs) (apply #'concatenate 'string xs ))

;; (defun prn3 (l)
;;   (cond ((null l) "NIL")
;; 	((atom (car l))
;; 	 (cond ((null (cdr l)) (cat (atos (car l))))
;; 	       ((atom (cdr l)) (cat (atos (car l)) " . " (atos (cdr l))))
;; 	       (t (cat (atos (car l)) " " (prn3 (cdr l))))))
;; 	(t (cat (-prn #'prn3 (car l)) " " (prn3 (cdr l))))))

(defun prn3 (l)
  (cond ((null l) "NIL")
	((atom (car l))
	 (let ((hs (atos (car l))))
	  (cond ((null (cdr l)) hs)
		((atom (cdr l)) (cat hs " . " (atos (cdr l))))
		(t (cat hs " " (prn3 (cdr l)))))))
	(t (cat (-prn #'prn3 (car l)) " " (prn3 (cdr l))))))

(defun -prn (p l)
  (concatenate 'string "(" (funcall p l) ")")) ;;; ohh it werks

(defun show (l)
  (labels ((-show (l) (cond ((null l) "NIL")
			    ((atom (car l))
			     (let ((hs (atos (car l))))
			       (cond ((null (cdr l)) hs)
				     ((atom (cdr l)) (cat hs " . " (atos (cdr l))))
				     (t (cat hs " " (-show (cdr l)))))))
			    (t (cat (show (car l)) " " (-show (cdr l)))))))
    (cat "(" (-show l) ")"))) ;; final version.


;; (defun show (l)
;;   (labels ((-show (l)
;; 	     (cond ((null l) "NIL")
;; 		   ((atom (car l))
;; 		    (cond ((null (cdr l)) (atos (car l)))
;; 			  ((atom (cdr l)) (cat (atos (car l)) " . " (atos (cdr l))))
;; 			  (t (cat (atos (car l)) " " (-show (cdr l))))))
;; 		   (t (cat (show (car l)) " " (-show (cdr l)))))))
;;     (cat "(" (-show l) ")"))) ;; final version.

(defun show (l)
  (labels ((-show (l) (cond ((null l) "NIL")
			    ((atom (car l))
			     (let ((hs (atos (car l))))
			       (cond ((null (cdr l)) hs)
				     ((atom (cdr l)) (cat hs " . " (atos (cdr l))))
				     (t (cat hs " " (-show (cdr l)))))))
			    ((null (cdr l)) (show (car l)))
			    (t (cat (show (car l)) " " (-show (cdr l)))))))
    (cat "(" (-show l) ")")))

(show '(defun show (l)
  (labels ((-show (l) (cond ((null l) "NIL")
			    ((atom (car l))
			     (let ((hs (atos (car l))))
			       (cond ((null (cdr l)) hs)
				     ((atom (cdr l)) (cat hs " . " (atos (cdr l))))
				     (t (cat hs " " (-show (cdr l)))))))
			    ((null (cdr l)) (show (car l)))
			    (t (cat (show (car l)) " " (-show (cdr l)))))))
    (cat "(" (-show l) ")"))))
