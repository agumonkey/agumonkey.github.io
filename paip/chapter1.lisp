;;; paip

(defpackage :paip
  (:use common-lisp))

(in-package :paip)

(defvar *indent* 1 "indentation level")

(defun count-atoms (l)
  (if (null l)
      0
      (let ((h (car l))
	    (r (cdr l)))
	(if (atom h)
	    (1+ (count-atoms r))
	    (+ (count-atoms h) (count-atoms r))))))

(defun assert-equals (f a r)
  (let ((status (if (equal (funcall (symbol-function f) a) r) "[SUCCESS]" "[FAILURE]")))
    (format t "~a (~a ~a) => ~a.~%" status (symbol-name f) a r)))

(count-atoms '(a (b) ((c) d e) (((f) g) h))) ;; => 8
(assert-equals 'count-atoms '(a (b) ((c) d e) (((f) g) h)) 8)

(defun count-atoms% (l)
  (cond ((null l) 0)
	((atom (car l)) (1+ (count-atoms (cdr l))))
	(t (+ (count-atoms (car l)) (count-atoms (cdr l))))))

(assert-equals 'count-atoms% '(a (b) ((c) d e) (((f) g) h)) 8)

(defun flatten (l)
  (cond ((null l) nil)
	((atom (car l)) (cons (car l) (flatten (cdr l))))
	(t (append (flatten (car l)) (flatten (cdr l))))))

;;; dfs

(defun dfs (tree acc m z)
  (cond ((null tree) z)
	((atom (car tree)) (funcall acc (car tree) (dfs (cdr tree) acc m z)))
	(t (funcall m (dfs (car tree) acc m z) (dfs (cdr tree) acc m z)))))

;;; (dfs '(a b c d e) #'cons nil) obsolete
(dfs '(a b (c) d e) #'cons #'append nil)

(defun dfa (tree r a)
    (cond ((null tree) a)
	  ((atom (car tree) (funcall r (car tree) (dfa (cdr tree) r a))))
	  (t (dfa (car tree) r (cdr tree)))))

(dfa '(a) #'cons nil)
(dfa '(a b (c) d e) #'cons nil)

(defun untree (-t f g)
  (cond ((null -t) nil)
	((atom (car -t)) (funcall f (car -t) (untree (cdr -t) f g)))
	(t (funcall g (untree (car -t) f g) (untree (cdr -t) f g)))))

(let ((flatten (lambda (tree) (untree tree #'cons #'append)))
      (oak '(a (b) ((c) d e) (((f) g) h))))
  (funcall flatten oak))

