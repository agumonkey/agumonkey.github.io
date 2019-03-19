


;;;;;;;;;;;;;;;; Chapter 2


(defpackage :paip-2
  (:use common-lisp))

(in-package :paip-2)

;;; code begins here.

"
s => np + vp
np => a + n
vp => v + np
a => a, the, some
n => man, cat, woman, boat
v => ate, took, saw, liked
"

(defun one-of (l)
  (list (nth (random (length l)) l)))

;;; (one-of '(1 2 3 4 5 6))

(defun n () (one-of '(man cat woman boat)))
(defun v () (one-of '(ate took saw liked)))
(defun a () (one-of '(a the some)))

(defun np () (append (a) (n)))
(defun vp () (append (v) (np)))
(defun s () (append (np) (vp)))

(s)

(defun range (n) (loop for i from 0 to n collect n))

(defmacro fn (&rest body)
  `(lambda (&rest _) (declare (ignore _)) ,@body))

(defun test ()
  (let ((sentences (mapcar (fn (s)) (range 12))))
    (dolist (s sentences)
      (format t "~@(~{~A~^ ~}~).~%" s))))

;; ((THE CAT ATE A MAN)
;;  (A BOAT SAW A MAN)
;;  (THE CAT LIKED SOME BOAT)
;;  (A BOAT LIKED A WOMAN)
;;  (SOME BOAT SAW A WOMAN)
;;  (THE CAT LIKED THE CAT)
;;  (SOME BOAT TOOK THE BOAT)
;;  (THE CAT LIKED A WOMAN)
;;  (A MAN LIKED A MAN)
;;  (THE CAT LIKED THE CAT)
;;  (A BOAT SAW A MAN)
;;  (THE CAT ATE SOME MAN)
;;  (A BOAT SAW THE WOMAN))

;;; end of :paip-2

(defvar *grammar*
  '(sentence => noun-phrase verb-phrase
    noun-phrase => article noun
    verb-phrase => verb noun-phrase
    noun => man cat woman boat
    article => ate took saw liked
    verb => a the some))

;;; kleene grammar

(defvar *grammar-k*
  '(sentence => noun-phrase verb-phrase
    noun-phrase => article adj* noun pp*
    adj* => 0 or adj adj* or hyp adj
    pp* => 0 or pp pp*
    prep-phrase => prep noun-phrase
    prep => and to in by with
    adj => green big small cute hard fast round
    verb-phrase => verb noun-phrase
    noun => man cat woman boat
    article => ate took saw liked
    verb => a the some))

(defun adj* ()
  (if (= (random 2) 0)
      nil
      (if (= (random 2) 0)
	  (append (hyp) (adj))
	  (append (adj) (adj*)))))

(defun pp* ()
  (if (= (random 2) 0)
      nil
      (append (pp) (pp*))))

(defun n () (one-of '(man cat woman dog boat tree house)))
(defun v () (one-of '(ate took saw liked painted called)))
(defun a () (one-of '(a the some one no)))

(defun hyp () (one-of '(super hyper mega very anti)))
(defun adj () (one-of '(green big small cute hard fast round)))
(defun pp () (append (prep) (np)))
(defun prep () (one-of '(and to in by with)))
(defun np () (append (a) (adj*) (n) (pp*)))

(defun show-sentence (s) (format t "~@(~{~A~^ ~}~).~%" s))

(defun test-k ()
  (let ((sentences (mapcar (fn (s)) (range 12))))
    (dolist (s sentences)
      (format t "~@(~{~A~^ ~}~).~%" s))))


;;;; self parsing interlude

(defvar *linear* '(c l a s s SPACE O b j e c t OPENBRACE N o t h i n g ENDBRACE))

(defun parse-l (a b s)
  (cond ((or (null a) (null b)) (list :len-error (reverse s)))
	((eq (car a) (car b)) (parse-l (cdr a) (cdr b) (cons (car a) s)))
	(t (list :lang-error (reverse s) :not-in b))))

(parse-l '(c l a s s SPACE a b j e c t OPENBRACE N o t h i n g ENDBRACE)
	 *linear* nil)

(defun parser (g s z)
  (let ((r (g z)))
    (cond ((altp r) (if-let (v (try-each r)) v :error))
	  ((seqp r) (append (thread r #'parse s z)))
	  (t :wat))))

(defvar g '((root (seq np vp))
	    (np (seq a n))
	    (vp (seq v np))
	    (n (or john mary))
	    (a (or the a))
	    (v (or saw like called))))

;;;;;;;;;;;;;

(defparameter *g0*
  '((s -> (np vp))
    (np -> (A N))
    (vp -> (V np))
    (A -> a the some)
    (N -> man cat tree)
    (V -> saw liked called)))

(defun g-lhs (r) (car r))  ;; keys
(defun g-rhs (r) (cddr r)) ;; vals
(defun g-get (o g) (assoc o g))

(defun nt? (o) (and (listp (car o)) (= (length o) 1)))
(defun nt* (seq) (car seq))

(defun gen (g -r)
  (let* ((r (g-get -r g))
	 (o (g-rhs r)))
    (cond ((nt? o) (apply #'append (mapcar (lambda (m) (gen g m)) (nt* o))))
	  (t (one-of o))))) ;;; FUGLY but functional

(defparameter g1
  '((s -> (np vp))
    ;; (np -> (A adj* N))
    (np -> (A N))
    (vp -> (V np))
    (adj* -> (ADJ ADJ))
    (ADJ -> big fast small gentle angry)
    (A -> a the some)
    (N -> man cat tree dog woman bird boat fish car)
    (V -> saw took ate liked called)))

;; PAIP-2> (gen g1 's)
;; (SOME BIG BIG TREE LIKED SOME FAST FAST TREE)

;; PAIP-2> (show-sentence (gen g1 's))
;; Some fish took a woman.

;;; TODO: kleene star capable gen-k function

(defun all (p l)
  (cond ((null l) t)
	(t (and (funcall p (car l)) (all p (cdr l))))))

(defun alt? (o) (and (listp o) (> (length o) 1) (all #'listp o)))

(defun gen-k (g -r)
  (let* ((r (g-get -r g))
	 (o (g-rhs r)))
    (cond ((nt? o)
	   (let ((no (nt* o)))
	     (if (alt? no)
		 (apply #'append (mapcar (lambda (m) (gen-k g m)) (car (one-of no))))
		 (apply #'append (mapcar (lambda (m) (gen-k g m)) no)))))
	  (t (one-of o)))))

(defparameter gk0
  '((s -> (np vp))
    (np -> ((A N) (A ADJ N) (A adj* N)))
    (vp -> ((V) (V np) (V np pp np)))
    (pp -> and or then)
    (adj* -> (ADJ ADJ))
    (ADJ -> big fast small gentle angry)
    (A -> a the some)
    (N -> man cat tree dog woman bird boat fish car)
    (V -> saw took ate liked called)))

(gen-k gk0 's)

;;; DONE: kleene star capable gen-k function

(defun compose (f g)
  (lambda (v) (funcall g (funcall f v))))

(defun rew (s g) (g-rhs (g-get s g)))

(defun mappend (f l) (apply #'append (mapcar f l)))

(defun gen-n (g s)
  (cond ((listp s) (mappend (lambda (m) (gen-n g m)) s))
	((rew s g) (gen-n g (one-of (rew s g))))
	(t (list s))))

(show-sentence (gen-n gk0 's)) ;;; wrung

;;; rewrite of gen-k using mappend and rew

(defun gen-k2 (g -r)
  (let ((r (rew -r g)))
    (cond ((nt? r)
	   (let ((no (nt* r)))
	     (if (alt? no)
		 (mappend (lambda (m) (gen-k2 g m)) (car (one-of no)))
		 (mappend (lambda (m) (gen-k2 g m)) no))))
	  (t (one-of r)))))

(gen-k2 gk0 's)

;;; no nested cond let if ?

(defmacro fn (&rest body) `(lambda (it) ,@body))

(defun gen-k3 (g -r)
  (let ((r (rew -r g)))
    (cond ((and (nt? r) (alt? (nt* r))) (mappend (fn (gen-k3 g it)) (car (one-of (nt* r)))))
	  ((and (nt? r))                (mappend (fn (gen-k3 g it)) (nt* r)))
	  (t                            (one-of r)))))

(gen-k3 gk0 's)


(defparameter gk0_fr
  '((s -> (np vp))
    (np -> ((A N) (A ADJ N) (A adj* N)))
    (vp -> ((V) (V np) (V np pp np)))
    (pp -> et ou puis)
    (adj* -> (ADJ ADJ))
    (ADJ -> grand rapide petit gentil mechant)
    (A -> un le certain)
    (N -> homme chat arbre chien femme oiseau bateau poisson voiture)
    (V -> vois prends mange aime appelle)))
