;;;; Copyright Bo Victor Natanael Fors <krakow89@gmail.com>
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
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
(define-macro (divider fn)
  `(lambda (e n)
     (,fn (/ e n))))

;;; Divide a number, rounding up.
(define div-up (divider ceiling))

;;; Divide a number, rounding down.
(define div-down (divider floor))

;;; Create a unique number incrementing from init
(define unique-counter
 (typed-lambda ((init integer))
   (lambda ()
     (begin
       (set! init (+ 1 init))
       (- init 1)))))
		 

;;; Convenience macro used to define association lists
;;; resolves the second argument in the pair but not the first.
(define-syntax symbol-list
  (syntax-rules ()
     ((_ ln)
      (letrec-syntax
	  ((symbol-listp
	    (syntax-rules ()
	      ((_ ()) '())
	      ((_ ((k v) . rest))
	       (cons (cons 'k v)
		     (symbol-listp rest))))))
	(symbol-listp ln)))))

;;; A hash table of type symbols and predicates.
(define *types*
  (fold (make-hash-table)
	(lambda (t kv)
	  (begin
	    (hash-set! t (car kv) (cdr kv))
	    t))
        (symbol-list
	 ((bool boolean?)
	  (integer integer?)
	  (number number?)
	  (rational rational?)
	  (complex complex?)
	  (real real?)
	  (symbol symbol?)
	  (list list?)))))

;;; Check a value against a type label.
(define typecheck
  (lambda (val type)
    (let ((type-function
	   (hash-ref *types* type
		     (lambda args
		       (error (string-append
			       "unknown type: "
			       (format #nil "~a" type)))))))
      (if (type-function val)
	  #t
	  (error (string-append "type error: "
				(format #nil "~a" val)
				" is not of type "
				(format #nil "~a" type)))))))

;;; A macro that defines lambda expressions that optionally
;;; perform type checking before running the function body.
(define-macro
  (typed-lambda args . rest)
  (let ((formal-args (map (lambda (n)
			    (if (pair? n)
				(car n)
				n))
			  args))
	(typed-args (filter pair? args)))
    `(lambda ,formal-args
       (map typecheck (list ,@(map car typed-args)) ',(map cadr typed-args))
       ,@rest)))

;;; Does ln contain the value n?
(define in-list
  (lambda (n ln)
    (if (eq? (list) ln)
	#f
	(if (eq? n (car ln))
	    #t
	    (in-list n (cdr ln))))))

;;; Are the elements in the list ln unique?
(define uniq?
  (lambda (ln)
    (letrec
	((uniq?'
	  (lambda (r ln)
	    (if (eq? (list) ln)
		#t
		(let ((comb (append ln r)))
		  (if (in-list (car comb) (cdr comb))
		      #f
		      (uniq?' (cons (car ln) r) (cdr ln))))))))
      (uniq?' (list) ln))))

;;; --- Dice roll related functions ---

;;; Roll n six-sided dice.
(define d6
  (typed-lambda ((n integer))
    (map (lambda args
	   (+ 1 (random 6)))
	 (iota n))))

;;; Show all rolls?
(define *show-rolls* #f)

;;; Was the last roll a glitch?
(define *glitch* #f)

;;; Is this dice roll a success?
(define success?
  (typed-lambda ((n integer))
    (>= n 5)))

;;; Is this dice roll a glitch?
(define glitch?
  (typed-lambda ((n integer))
    (= n 1)))

;;; Roll a given SR5 dice pool with a limit
;;; returning the number of successes, and
;;; setting the glitch flag.
(define roll
  (typed-lambda ((pool integer) (limit integer))
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
  (typed-lambda ((pool integer) (limit integer) (threshold integer))
    (let ((result (roll pool limit)))
	    (if (>= result threshold)
		#t
		#f))))

;;; SR5 opposed test
(define opposed-test
  (typed-lambda ((pool-attacker integer)
	         (limit-attacker integer)
	         (pool-defender integer)
	         (limit-defender integer))
    (let ((result-attacker (roll pool-attacker limit-attacker))
	  (result-defender (roll pool-defender limit-defender)))
      (max 0 (- result-attacker result-defender)))))

;;; Predicate version of an SR5 opposed test
;;; tie goes to the defender as per the corebook rules.
(define opposed-test?
  (typed-lambda ((pool-attacker integer)
	         (limit-attacker integer)
	         (pool-defender integer)
	         (limit-defender integer))
    (let ((attacker-successes (opposed-test
			       pool-attacker limit-attacker
			       pool-defender limit-defender)))
      (if (zero? attacker-successes) #f #t))))

;;; Calculate dice pool for an attribute and skill
;;; defaulting if the skill isn't known.
(define default
  (typed-lambda ((attribute integer) (skill integer))
    (if (= 0 skill)
	(- attribute 1)
	(+ skill attribute))))

;;; --- Randomness-related functions. ---

;;; Return an integer in the range from to to.
(define range
  (lambda (from to)
    (+ (random (- to (- from 1))) from)))

;;; Return a value around n varied by v.
(define plus-minus
  (lambda (n v)
    (+ (range (- v) v) n)))

;;; Return t with a probability of 1 in n.
(define randomly
  (lambda (n)
    (if (< n 2)
	#t
	(if (= 1 (random n))
	    #t
	    #f))))

;;; Add i to l with a probability of 1 in n.
(define add-one-in
  (typed-lambda ((n integer) i (l list))
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
	  (floor-table (make-hash-table))    ; The table of floor values.
	  (ceiling-table (make-hash-table))) ; The table of ceiling values.
      (hash-set! hash-table 'container #t)
      (letrec
	  ((this
	    (lambda args
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
	     (syntax-rules (expression add sub div div-up div-down
				       mul inheriting add-gear caps type
				       transform)
	       ((_ this-macro level ()) (begin (display "\n") this-macro))
	       ;; Handle inheritance.
	       ((_ this-macro level ((inheriting . templates) . rest))
		(begin
		  (apply-template this-macro templates)
		  (templ-expr this-macro level rest)))
	       ;; Handle stored expressions.
	       ((_ this-macro level ((name (expression expr)) . rest))
		(begin
		  (this-macro 'name (stored-expression expr))
		  (templ-expr this-macro level rest)))
	       ;; Add a value to the inherited value.
	       ((_ this-macro level ((name (add expr)) . rest))
		(begin
		  (this-macro 'name (+ (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ;; Subtract a value from the inherited value.
	       ((_ this-macro level ((name (sub expr)) . rest))
		(begin
		  (this-macro 'name (- (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ;; Divide the inherited value by a value.
	       ((_ this-macro level ((name (div expr)) . rest))
		(begin
		  (this-macro 'name (/ (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ;; Divide while rounding up the inherited value by a value.
	       ((_ this-macro level ((name (div-up expr)) . rest))
		(begin
		  (this-macro 'name (div-up (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ;; Divide while rounding down the inherited value by a value.
	       ((_ this-macro level ((name (div-down expr)) . rest))
		(begin
		  (this-macro 'name (div-down (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ;; Multiply the inherited value by a value.
	       ((_ this-macro level ((name (mul expr)) . rest))
		(begin
		  (this-macro 'name (* (this-macro 'name) expr))
		  (templ-expr this-macro level rest)))
	       ;; Add a value to a list by a factor of one in n
	       ;; the argument is an association list shaped like (n . value).
	       ((_ this-macro level ((name (add-gear expr)) . rest))
		(begin
		  (this-macro 'name (append (this-macro 'name)
					    (gear-list expr)))
		  (templ-expr this-macro level rest)))
	       ;; Set the type of a value.
	       ((_ this-macro level ((name (type typeval)) . rest))
		(begin
		  (this-macro 'container-operation-set-type 'name 'typeval)
		  (display ".")
		  (templ-expr this-macro level rest)))
	       ;; Set the caps of a value.
	       ((_ this-macro level ((name (caps floor ceiling)) . rest))
		(begin
		  (this-macro 'container-operation-set-caps 'name floor ceiling)
		  (display ".")
		  (templ-expr this-macro level rest)))
	       ;; Directly transform the container using a function.
	       ((_ this-macro level ((transform fn) . rest))
		(begin
		  (fn this-macro)
		  (templ-expr this-macro level rest)))
	       ;; Set a value.
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

;;; Function to handle the add-gear
;;; part of the template macro.
(define gear-list
  (lambda (ln)
    (fold (list)
	  (lambda (l v)
	    (add-one-in (car v) (cdr v) l))
	  ln)))

;;; Apply a template t to i
;;; with a probability of one in n.
(define one-in-template
 (lambda (n t i)
   (if (randomly n)
       (t i)
       i)))

;;; Modify the attributes of a metahuman template.
(define metahuman-attributes
  (typed-lambda ((bod integer) (str integer) (rea integer) (agi integer)
	         (cha integer) (log integer) (int integer) (wil integer))
    (let ((attribute-caps (list bod str rea agi
				cha log int wil)))
      (lambda (this)
	(let ((attribute-symbols
	       '(base-bod base-str base-rea base-agi
			  base-cha base-log base-int base-wil)))
	  (map this attribute-symbols
	       (map (lambda (cap)
		      (plus-minus (div-down cap 2) 1))
		    attribute-caps))
	  (map (lambda (attribute cap)
		 (this 'container-operation-set-caps attribute 0 cap))
	       attribute-symbols
	       attribute-caps)
	  (map (lambda (attribute)
		 (this 'container-operation-set-type
		       attribute 'integer))
	       attribute-symbols))))))

(define new-genes (unique-counter 0))

;;; --- Templates ---

;;; The basic metahuman template
;;; this is also the "human" template.
(define metahuman-template
  (template
   (metahuman #t)
   (metatype human)
   (transform (metahuman-attributes 6 6 6 6 6 6 6 6))
   (physical-wound-track 0)
   (physical-wound-max
    (expression
     (lambda (this)
       (+ 8 (div-down (this 'base-body) 2)))))
   (stun-wound-track 0)
   (stun-wound-max
    (expression
     (lambda (this)
       (+ 8 (div-down (this 'base-will) 2)))))
   (mental-limit
    (expression
     (lambda (this)
       (div-up
	(+ (* 3 (this 'base-log))
	   (this 'base-wil)
	   (this 'base-int)) 3))))
   (inventory (list))
   (cyberware (list))
   (genes (new-genes))))

(define dwarf-racial-template
  (template
   (metatype dwarf)
   (transform (metahuman-attributes 8 8 5 6 6 6 6 7))))

(define ork-racial-template
  (template
   (metatype ork)
   (transform (metahuman-attributes 9 8 6 6 5 5 6 6))))

(define elf-racial-template
  (template
   (metatype elf)
   (transform (metahuman-attributes 6 6 6 7 8 6 6 6))))

(define troll-racial-template
  (template
   (metatype troll)
   (transform (metahuman-attributes 10 10 6 5 4 5 5 6))))

(define cyberware-template
  (template (inheriting device-template)
   (cyberware #t)
   (base-essence-cost 0.0)
   (essence-cost
    (expression
     (lambda (this)
       (* (this 'base-essence-cost)
	  (case (this 'cyberware-rating)
	    ((used) 1.2)
	    ((standard) 1)
	    ((alphaware) 0.8)
	    ((betaware) 0.6)
	    ((deltaware) 0.5))))))
   (cyberware-rating standard)
   (location torso)
   (visible #f)))

(define datajack-template
  (template (inheriting cyberware-template)
   (datajack #t)
   (base-essence-cost 0.1)
   (location head)
   (visible #t)))

(define cyberlimb-template
  (template (inheriting cyberware-template)
   (cyberlimb #t)
   (cyberware-location right-arm)
   (base-str 3)
   (base-bod 3)
   (base-qui 3)
   (essence-cost 1.0)
   (visible #t)))

(define device-template
  (template (inheriting gear-template)
   (device #t)
   (device-rating 1)))

(define gear-template
  (template
   (availability 10)
   (legality legal)
   (value 10)))                         ; in nuyen

(define cyberdeck-template
  (template (inheriting device-template)
   (cyberdeck #t)
   (device-rating 3)
   (matrix-sleaze 6)
   (matrix-attack 5)
   (matrix-firewall 4)
   (matrix-data-processing 3)))

(define configure-cyberdeck
  (lambda (device one two three four)
    (let ((ratings '(data-processing firewall sleaze attack)))
      (if (and (device 'cyberdeck)
	       (in-list one ratings)
	       (in-list two ratings)
	       (in-list three ratings)
	       (in-list four ratings)
	       (uniq? (list one two three four)))
	  (let ((rating (device 'device-rating)))
	    (device one (+ 3 rating))
	    (device two (+ 2 rating))
	    (device three (+ 1 rating))
	    (device four rating)
	    #t)
	  #f))))

(define system-identification-number-template
  (template
   (sin-type corporate-born)
   (name "Yamada Taro")
   (genetic-data #nil)
   (issuer renraku)
   (ethnicity asian)
   (metatype human)
   (awakened-emerged #f)
   (licenses (list))))

(define license-template
  (template
   (license-type civilian-firearms)
   (issuer renraku)))    

(define wageslave-template
  (template (inheriting metahuman-template)
   (base-log (add (range 0 1)))
   (base-cha (add (range 0 1)))
   (cyberware (add-gear
	       (symbol-list ((2 (datajack-template))))))))

(define mage-template
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
   (base-log (add (range 1 2)))
   (base-wil (add (range 1 2)))
   (bound-spirits (list))
   (known-spells (list))))
