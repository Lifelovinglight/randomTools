;;;; Copyright Bo Victor Natanael Fors <krakow89@gmail.com>
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANtability or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; --- Utility functions ---

;;; Fold function fn from init over ln.
(define fold
  (lambda (init fn ln)
    (letrec ((fold'
	      (lambda (fn ln r)
		(if (eq? (list) ln)
		    r
		    (fold' fn (cdr ln) (fn r (car ln)))))))
      (fold' fn ln init))))

;;; Create a division function.
(define divider
  (lambda (fn)
    (lambda (e n)
      (fn (/ e n)))))

;;; Divide a number, rounding up.
(define div-up (divider ceiling))

;;; Divide a number, rounding down.
(define div-down (divider floor))

;;; Check a value against a type label.
(define typecheck
  (lambda (val type)
    (let ((type-function
	   (case type
	     ((bool) boolean?)
	     ((integer) integer?)
	     ((float) float?)
	     ((symbol) symbol?)
	     ((list) list?)
	     (else (error "unknown type")))))
      (if (type-function val)
	  #t
	  (error "type error")))))

;;; --- Dice roll related functions ---

;;; Roll n six-sided dice.
(define d6
  (lambda (n)
    (typecheck n 'integer)
    (map (lambda args
	   (+ 1 (random 6)))
	 (iota n))))

;;; Show all rolls?
(define *show-rolls* #f)

;;; Was the last roll a glitch?
(define *glitch* #f)

;;; Is this dice roll a success?
(define success?
  (lambda (n)
    (typecheck n 'integer)
    (>= n 5)))

;;; Is this dice roll a glitch?
(define glitch?
  (lambda (n)
    (typecheck n 'integer)
    (= n 1)))

;;; Roll a given SR5 dice pool with a limit
;;; returning the number of successes, and
;;; setting the glitch flag.
(define roll
  (lambda (pool limit)
    (typecheck pool 'integer)
    (typecheck limit 'integer)
    (let ((dice (d6 pool)))
      (if *show-rolls*
	  (begin (display dice)
		 (newline)))
      (let ((result (length (filter success? dice))))
	(set! *glitch*
	  (if (> (length (filter glitch? dice))
		 (/ pool 2))
	      #t
	      #f))
	(if (>= result limit)
	    limit
	    result)))))

;;; SR5 success test.
(define success-test
  (lambda (pool limit threshold)
    (typecheck pool 'integer)
    (typecheck limit 'integer)
    (typecheck threshold 'integer)
    (let ((result (roll pool limit)))
	    (if (>= result threshold)
		#t
		#f))))

;;; SR5 opposed test
(define opposed-test
  (lambda (pool-attacker limit-attacker pool-defender limit-defender)
    (typecheck pool-attacker 'integer)
    (typecheck limit-attacker 'integer)
    (typecheck pool-defender 'integer)
    (typecheck limit-defender 'integer)
    (let ((result-attacker (roll pool-attacker limit-attacker))
	  (result-defender (roll pool-defender limit-defender)))
      (max 0 (- result-attacker result-defender)))))

;;; Predicate version of an SR5 opposed test
;;; tie goes to the defender as per the corebook rules.
(define opposed-test?
  (lambda (pool-attacker limit-attacker pool-defender limit-defender)
    (typecheck pool-attacker 'integer)
    (typecheck limit-attacker 'integer)
    (typecheck pool-defender 'integer)
    (typecheck limit-defender 'integer)
    (let ((attacker-successes (opposed-test
			       pool-attacker limit-attacker
			       pool-defender limit-defender)))
      (if (zero? attacker-successes) #f #t))))

;;; Calculate dice pool for an attribute and skill
;;; defaulting if the skill isn't known.
(define default
  (lambda (attribute skill)
    (typecheck attribute 'integer)
    (typecheck skill 'integer)
    (if (= 0 skill)
	(- attribute 1)
	(+ skill attribute))))

;;; --- Randomness-related functions. ---

(define range
  (lambda (from to)
    (+ (random (- to (- from 1))) from)))

(define plus-minus
  (lambda (n v)
    (+ (range (- v) v) n)))

(define randomly
  (lambda (n)
    (if (< n 2)
	#t
	(if (= 1 (random n))
	    #t
	    #f))))

(define add-one-in
  (lambda (n i l)
    (if (randomly n)
	(cons i l)
	l)))

;;; --- Container related functions. ---

;;; Create a stored expression
;;; when referenced in a container this will be called
;;; on the container itself.
(define stored-expression
  (lambda (fn)
    (cons 'srex fn)))

;;; Type predicate for a stored expression.
(define stored-expression?
  (lambda (t)
    (and (pair? t)
	 (eq? 'srex (car t)))))

;;; Cap a value v against the floor f and ceiling c.
(define cap
  (lambda (v f c)
    ((lambda (v)
       (if c (min v c) v))
     (if f (max v f) v))))

;;; A container to store values.
;;; It supports typed values as well as floors and ceilings
;;; for numerical types.
(define container
  (lambda ()
    (let ((hash-table (make-hash-table))     ; The table storing values.
	  (type-table (make-hash-table))     ; The table storing types.
	  (floor-table (make-hash-table))    ; The table storing numerical floor values.
	  (ceiling-table (make-hash-table))) ; The table storing numerical ceiling values.
      (hash-set! hash-table 'container #t)
      (letrec ((this (lambda args
		       (case (length args)
			 ;; Reference a stored value.
			 ((1) (letrec ((label (car args))
				       (result (hash-ref hash-table label #f)))
				(if (stored-expression? result)
				    ((cdr result) this)
				    result)))
			 ;; Set a value.
			 ((2) (letrec ((label (car args))
				       (val (cadr args))
				       (type (hash-ref type-table label))
				       (floor (hash-ref floor-table label))
				       (ceiling (hash-ref ceiling-table label)))
				  (if type (typecheck val type))
				  (hash-set! hash-table label
					     (if (number? val)
						 (cap val floor ceiling)
						 val))))
			 ((3) (begin
				(let ((operation (car args)))
				  (case operation
				    ;; Set the type of a value.
				    ((container-operation-set-type)
				     (let ((label (cadr args))
					   (type (caddr args)))
				       (hash-set! type-table label type)))
				    (else (error "unknown container operation"))))))
			 ((4) (begin
				(let ((operation (car args)))
				  (case operation
				    ;; Set the caps of a value.
				    ((container-operation-set-caps)
				     (let ((label (cadr args))
					   (floor (caddr args))
					   (ceiling (cadddr args)))
				       (hash-set! floor-table label floor)
				       (hash-set! ceiling-table label ceiling)))
				    (else (error "unknown container-operation"))))))
			 (else (error "wrong number of arguments to container"))))))
	this))))

;;; --- Container operations functions. ---

;;; Set the type t of a value v in the container c.
(define container-set-type
  (lambda (c v t)
    (c 'container-operation-set-type v t)))

;;; Set the floor f and ceiling cl of a type v in the container c.
(define container-set-caps
  (lambda (c v f cl)
    (c 'container-operation-set-caps v f cl)))

;;; --- Template related functions ---

;;; A macro that creates a template function.
;;; A template function takes zero to one arguments.
;;; if provided with a container as an argument
;;; it will apply itself to that container.
;;; Else, it will just create a new instance of the template.
(define-syntax template
  (syntax-rules ()
    ((_ exp ...)
     (letrec-syntax
	 ((apply-template
	   (syntax-rules ()
	     ((_ this-macro ()) this-macro)
	     ((_ this-macro (template . rest))
	      (begin
		(template this-macro)
		(apply-template this-macro rest))))))
       (letrec-syntax
	   ((templ-expr
	     (syntax-rules (expression add sub div div-up div-down mul inheriting add-gear caps type)
	       ((_ this-macro level ()) (begin (display "\n") this-macro))
	       ((_ this-macro level ((inheriting . templates) . rest))
		(begin
		  (apply-template this-macro templates)
		  (templ-expr this-macro level rest)))
	       ((_ this-macro level ((name (expression expr)) . rest))
		(begin
		  (this-macro 'name (stored-expression expr))
		  (templ-expr this-macro level rest)))
	       ((_ this-macro level ((name (add expr)) . rest))
		(begin
		  (this-macro 'name (+ (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))	     
	       ((_ this-macro level ((name (sub expr)) . rest))
		(begin
		  (this-macro 'name (- (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ((_ this-macro level ((name (div expr)) . rest))
		(begin
		  (this-macro 'name (/ (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ((_ this-macro level ((name (div-up expr)) . rest))
		(begin
		  (this-macro 'name (div-up (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ((_ this-macro level ((name (div-down expr)) . rest))
		(begin
		  (this-macro 'name (div-down (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ((_ this-macro level ((name (mul expr)) . rest))
		(begin
		  (this-macro 'name (* (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ((_ this-macro level ((name (add-gear expr)) . rest))
		(begin
		  (this-macro 'name (append (this-macro 'name) (gear-list expr)))
		  (templ-expr this-macro level rest)))
	       ((_ this-macro level ((name (type typeval)) . rest))
		(begin
		  (this-macro 'container-operation-set-type 'name 'typeval)
		  (display ".")
		  (templ-expr this-macro level rest)))
	       ((_ this-macro level ((name (caps floor ceiling)) . rest))
		(begin
		  (this-macro 'container-operation-set-caps 'name floor ceiling)
		  (display ".")
		  (templ-expr this-macro level rest)))
	       
	       ((_ this-macro level ((name value) . rest))
		(begin
		  (this-macro 'name
			      (if (symbol? 'value) 'value value))
		  (display ".")
		  (templ-expr this-macro level rest))))))
	 (lambda argv
	   (let ((this-macro (if (eq? (list) argv) (container) (car argv))))
	     (display "initializing template\n")
	     (templ-expr this-macro level (exp ...)))))))))
  
(define gear-list
  (lambda (ln)
    (fold (list)
	  (lambda (l v)
	    (add-one-in (car v) (cdr v) l))
	  ln)))

(define one-in-template
 (lambda (n t i)
   (if (randomly n)
       (t i)
       i)))

;;; --- Templates ---

(define metahuman
  (template
   (metahuman #t)
   (bod (type integer))
   (bod (plus-minus 3 1))
   (bod (caps 0 #f))
   (str (plus-minus 3 1))
   (str (caps 0 #f))
   (qui (plus-minus 3 1))
   (qui (caps 0 #f))
   (rea (plus-minus 3 1))
   (rea (caps 0 #f))
   (int (plus-minus 3 1))
   (int (caps 0 #f))
   (log (plus-minus 3 1))
   (log (caps 0 #f))
   (wil (plus-minus 3 1))
   (wil (caps 0 #f))
   (cha (plus-minus 3 1))
   (cha (caps 0 #f))
   (physical-wound-track 0)
   (stun-wound-track 0)
   (mental-limit
    (expression
     (lambda (this)
       (div-up
	(+ (* 3 (this 'log))
	   (this 'wil)
	   (this 'int)) 3))))
   (inventory (list))
   (cyberware (list))
   (genes (new-genes))))

(define cyberware
  (template (inheriting device)
   (cyberware #t)
   (essence-cost 0.0)
   (cyberware-rating standard)
   (location torso)
   (visible #f)))

(define datajack
  (template (inheriting cyberware)
   (datajack #t)
   (essence-cost 0.1)
   (location head)
   (visible #t)))

(define alphaware
  (template
   (cyberware-grade alphaware)
   (essence-cost (div 2))
   (availability (add 2))
   (price (mul 2))))

(define cyberlimb
  (template (inheriting cyberware)
   (cyberlimb #t)
   (cyberware-location right-arm)
   (str 3)
   (bod 3)
   (qui 3)
   (essence-cost 1.0)
   (visible #f)))

(define device
  (template (inheriting gear)
   (device #t)
   (rating 1)))

(define gear
  (template
   (availability 10)
   (legality legal)
   (price 10)))

(define cyberdeck
  (template (inheriting device)
   (cyberdeck #t)
   (rating 3)
   (sleaze 6)
   (attack 5)
   (firewall 4)
   (data-processing 3)))

(define in-list
  (lambda (n ln)
    (if (eq? (list) ln)
	#f
	(if (eq? n (car ln))
	    #t
	    (in-list n (cdr ln))))))

(define configure-cyberdeck
  (lambda (device one two three four)
    (let ((ratings '(data-processing firewall sleaze attack)))
      (if (and (device 'cyberdeck)
	       (in-list one ratings)
	       (in-list two ratings)
	       (in-list three ratings)
	       (in-list four ratings))
	  (let ((rating (device 'device-rating)))
	    (device one (+ 3 rating))
	    (device two (+ 2 rating))
	    (device three (+ 1 rating))
	    (device four rating)
	    #t)
	  #f))))

(define *genetic-code* 0)

(define new-genes
  (lambda ()
    (let ((genes *genetic-code*))
      (set! *genetic-code* (+ *genetic-code* 1))
      genes)))

(define system-identification-number
  (template
   (sin-type corporate-born)
   (name "Yamada Taro")
   (genetic-data #nil)
   (issuer renraku)
   (ethnicity asian)
   (meta human)
   (licenses (list))))

(define license
  (template
   (license-type civilian-firearms)
   (issuer renraku)))	    

(define wageslave
  (template (inheriting metahuman)
   (log (add (range 0 1)))
   (cha (add (range 0 1)))
   (cyberware (add-gear
	       (list
		(cons 2 (one-in-template
			 2 alphaware (datajack))))))))

(define mage
  (template
   (magic 3)
   (magic (caps 0 #f))
   (tradition hermetic)
   (conjuring (range 0 3))
   (spellcasting (range 0 3))
   (aura-reading (range 0 3))
   (arcana (range 0 3))
   (banishing (range 0 3))
   (initiation (range 0 1))
   (log (add (range 1 2)))
   (wil (add (range 1 2)))
   (bound-spirits (list))
   (known-spells (list))))
