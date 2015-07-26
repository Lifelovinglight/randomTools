(use-modules (srfi srfi-31))

(define d6
  (lambda (n)
    (map (lambda args
	   (+ 1 (random 6)))
	 (iota n))))

(define *show-rolls* #f)

(define *glitch* #f)

(define success?
  (lambda (n)
    (>= n 5)))

(define glitch?
  (lambda (n)
    (= n 1)))

(define roll
  (lambda (pool limit)
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

(define success-test
  (lambda (pool limit threshold)
    (let ((result (roll pool limit)))
	    (if (>= result threshold)
		#t
		#f))))

(define opposed-test
  (lambda (pool-attacker limit-attacker pool-defender limit-defender)
    (let ((result-attacker (roll pool-attacker limit-attacker))
	  (result-defender (roll pool-defender limit-defender)))
      (if (> result-attacker result-defender)
	  #t #f))))

(define default
  (lambda (attribute skill)
    (if (= 0 skill)
	(- attribute 1)
	(+ skill attribute))))

(define srex
  (lambda (fn)
    (cons 'srex fn)))

(define srex?
  (lambda (t)
    (and (pair? t)
	 (eq? 'srex (car t)))))

(define container
  (lambda ()
    (let ((hash-table (make-hash-table)))
      (hash-set! hash-table 'container #t)
      (letrec ((this (lambda args
		       (case (length args)
			 ((1) (let ((result (hash-ref hash-table (car args) #f)))
				(if (srex? result)
				    ((cdr result) this)
				    result)))
			 ((2) (hash-set! hash-table (car args) (cadr args)))
			 (else (error))))))
	this))))

(define divider
  (lambda (fn)
    (lambda (e n)
      (fn (/ e n)))))

(define div-up (divider ceiling))

(define div-down (divider floor))

(define *this* #nil)

(define-syntax template
  (syntax-rules ()
    ((_ exp ...)
     (letrec-syntax
	 ((templ-expr
	   (syntax-rules (expression add)
	     ((_ this-macro level ()) (begin (display "\n") this-macro))
	     ((_ this-macro level ((name (expression expr)) . rest))
	      (begin
		(this-macro 'name (cons 'srex expr))
		(templ-expr this-macro level rest)))
	     ((_ this-macro level ((name (add expr)) . rest))
	      (begin
		(this-macro 'name (+ (this-macro 'name) expr))
		(templ-expr this-macro level rest)))
	     ((_ this-macro level ((name (add-gear expr)) . rest))
	      (begin
		(this-macro 'name (append (this-macro 'name) (gear-list expr)))
		(templ-expr this-macro level rest)))
	     ((_ this-macro level ((name value) . rest))
	      (begin
		(this-macro 'name value)
		(display ".")
		(templ-expr this-macro level rest))))))
       (lambda argv
	 (let ((this-macro (if (eq? (list) argv) (container) (car argv))))
	   (display "initializing template\n")
	   (templ-expr this-macro level (exp ...))))))))

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

(define fold
  (lambda (init fn ln)
    (letrec ((fold'
	      (lambda (fn ln r)
		(if (eq? (list) ln)
		    r
		    (fold' fn (cdr ln) (fn r (car ln)))))))
      (fold' fn ln init))))
  
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

(define metahuman
  (template
   (metahuman #t)
   (bod (plus-minus 3 1))
   (str (plus-minus 3 1))
   (qui (plus-minus 3 1))
   (rea (plus-minus 3 1))
   (int (plus-minus 3 1))
   (log (plus-minus 3 1))
   (wil (plus-minus 3 1))
   (cha (plus-minus 3 1))
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
  (template
   (cyberware #t)
   (essence-cost 0.0)
   (cyberware-grade 1)
   (cyberware-rating 'standard)))

(define datajack
  (template
   (datajack #t)
   (essence-cost 0.1)))

(define alphaware
  (template
   (cyberware-grade 'alphaware)))

(define device
  (template
   (device-rating 1)))

(define cyberdeck
  (template
   (cyberdeck #t)
   (device-rating 3)
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
   (sin-type 'corporate-born)
   (name "Yamada Taro")
   (genetic-data #nil)
   (issuer 'renraku)
   (ethnicity 'asian)
   (hair 'black)
   (meta 'human)
   (licenses (list))))

(define license
  (template
   (license-type "civilian firearms")
   (issuer "renraku")))

(define wageslave
  (template
   (log (add (range 0 1)))
   (cha (add (range 0 1)))
   (cyberware (add-gear
	       (list (cons 2 (one-in-template
			      2 alphaware (datajack (cyberware)))))))))
