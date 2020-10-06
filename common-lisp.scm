
(define-module (common-lisp)
  #:use-module (srfi srfi-1)

  #:export
  (mapc maplist mapl mapcan mapcon
   atom? yes-or-no?)
  
  #:export-syntax
  (begin1 begin2 dolist dotimes
   setf! push! pop! incf! decf! defstruct))

; prog1
(define-syntax begin1
  (syntax-rules ()
    ((begin1 expr1 expr ...)
     (call-with-values (λ () expr1)
       (lambda result
	 (begin expr ... (apply values result)))))))

; prog2 
(define-syntax begin2
  (syntax-rules ()
    ((begin2 expr1 expr2 expr ...)
     (call-with-values (λ () expr1 expr2)
       (lambda result
	 (begin expr ... (apply values result)))))))

(define-syntax dolist
  (syntax-rules ()
    ((dolist (var list ret ...) body0 body ...)
     (begin (for-each (λ (var) body0 body ...) list)
	    ret ...))))

(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (var expr ret ...) body ...)
     (let ((res expr))
       (do ((var 0 (1+ var)))
	   ((= var res) ret ...) body ...)))))

(define-syntax setf!
  (syntax-rules ()
    ((setf! var val) (set! var val))
    ((setf! var val rest ...)
     (begin (set! var val)
	    (setf! rest ...)))))

(define-macro (push! x place)
  `(set! ,place (cons ,x ,place)))

(define-macro (pop! place)
  `(begin1 (car ,place) (set! ,place (cdr ,place))))

(define-syntax incf!
  (syntax-rules ()
    ((incf! place) (incf! place 1))
    ((incf! place n)
     (set! place (+ place n)))))

(define-syntax decf!
  (syntax-rules ()
    ((decf! place) (decf! place 1))
    ((decf! place n) (incf! place (- n)))))

(define mapc for-each)

(define (maplist f . lists)
  (if (any null? lists) '()
      (cons (apply f lists)
	    (apply maplist f (map cdr lists)))))

(define (mapl f . lists)
  (unless (any null? lists)
    (apply f lists)
    (apply mapl f (map cdr lists))))

(define (mapcan f . lists)
  (apply append! (apply map f lists)))

(define (mapcon f . lists)
  (apply append! (apply maplist f lists)))

(define (atom? x)
  (not (or (pair? x) (list? x))))

(define (yes-or-no? question . args)
  (apply format #t question args)
  (display " (yes/no): ") 
  (case (read)
    ((yes y) #t)
    ((no n) #f)
    (else
     (display "Please enter y, yes, n or no!\n")
     (apply yes-or-no? question args))))



;;; defstruct

(define (T? x) (eq? x #T))

(define (orid p f)
  (lambda (x)
    (or (and (p x) (f x)) x)))

(define car||id (orid pair? car))
(define cdr||id (orid pair? cdr))

(define (symbolicate . sym||str)
  (define (->str x) ((if (symbol? x) symbol->string identity) x))
  (string->symbol (apply string-append (map ->str sym||str))))

(define (glue result-type . arrays)
  (let ((result (result-type (apply + (map array-length arrays)) 0)))
    (let ((index 0))
      (dolist (arr arrays result)
	(array-for-each
	  (lambda (x)
	     (array-set! result x index)
	     (incf! index)) arr)))))

(define (better-glue result-type . arrays)
  (let ((non-empty-arrays (filter (compose not zero? array-length) arrays)))
    (cond ((null? non-empty-arrays) #())
	  ((null? (cdr non-empty-arrays))
	   (car non-empty-arrays))
	  (else (apply glue result-type non-empty-arrays)))))

(define (all-ops incomplete-ops)
  (define defaults
    `((#:constructor #T)
      (#:prefix #T)
      (#:copier #T)
      (#:predicate #T)
      (#:extend #F)
      (#:named #T)
      (#:public #F)
      (#:type vector)
      (#:offset 0)))

  (map (lambda (k)
	 (or (assq k incomplete-ops)
	     (assq k defaults)))
       (map car defaults)))

(define* (get ops op #:key (kar #T))
  ((if kar car identity) (assq-ref ops op)))

(define (get-constructor extend)
  (if (car extend)
      (primitive-eval
       (if (null? (cdr extend))
	   (symbolicate 'make- (car extend))
	   (cadr extend)))
      (const (cons '() #()))))

(define (constructor struct-name ops slots)
  (let ((constructor (get ops #:constructor))
	(offset (get ops #:offset))
	(type (get ops #:type))
	(extend (get ops #:extend #:kar #F))
	(public (get ops #:public))
	(named (get ops #:named)))
    (let* ((constructor-name
	    (if (T? constructor) (symbolicate 'make- struct-name) constructor))
	   (args-list
	    (map (lambda (s)
		   (if (atom? s)
		       `(,s ,(if (eq? type 'vector) #f 0)) s))
		 slots)))
      `(begin
	 (when ,public (export ,constructor-name))
	 (define* (,constructor-name #:key ,@args-list
		     ,@(if (null? slots) '() '(#:allow-other-keys)) #:rest keys)
	   ((if ,named (λ (s) (cons (cons ',struct-name ; if named, extended also named
					  ',(car||id
					      ((get-constructor extend)))) s)) identity)
	    ((@@ (common-lisp) better-glue) ,(symbolicate 'make- type)
	      ((@@ (common-lisp) cdr||id)
	       (apply ((@@ (common-lisp) get-constructor) ',extend) keys))
	      (make-u8vector ,offset 0) (,type ,@(map car||id slots)))))))))

(define (accessor struct-name ops slot index)
  (let ((prefix (get ops #:prefix))
	(offset (get ops #:offset))
	(extend (get ops #:extend #:kar #F))
	(public (get ops #:public)))
    `(,(if public 'define-public 'define)
      ,(cond ((or (atom? slot) (= (length slot) 2))
	      (if prefix (if (T? prefix)
			     (symbolicate struct-name '- (car||id slot))
			     (symbolicate prefix (car||id slot)))
		  (car||id slot)))
	     (else (caddr slot)))
      (let ((super-offset ,(array-length (cdr||id ((get-constructor extend))))))
	(make-procedure-with-setter
	 (λ (s) (array-ref ((@@ (common-lisp) cdr||id) s)
			   (+ ,index ,offset super-offset)))
	 (λ (s n) (array-set! ((@@ (common-lisp) cdr||id) s) n
			      (+ ,index ,offset super-offset))))))))

(define (accessors struct-name ops slots)
  (if (null? slots) '(noop)
      (let ((index 0))
	(cons 'begin
	      (map (lambda (slot)
		     (begin1 (accessor struct-name ops slot index) (incf! index))) slots)))))

(define (copier struct-name ops)
  (let ((copier (get ops #:copier))
	(type (get ops #:type))
	(public (get ops #:public)))
    (if (not copier) '(noop)
	(let ((copier-name (if (T? copier) (symbolicate 'copy- struct-name) copier)))
	  `(,(if public 'define-public 'define) (,copier-name ,struct-name)
	    (if (pair? ,struct-name)
		(cons (list-copy (car ,struct-name)) (,copier-name (cdr ,struct-name)))
		(let ((new (,(symbolicate 'make- type) (array-length ,struct-name) 0)))
		  (begin (array-copy! ,struct-name new) new))))))))

(define (predicate struct-name ops)
  (let ((predicate (get ops #:predicate))
	(named (get ops #:named))
	(extend (get ops #:extend #:kar #F))
	(public (get ops #:public)))
    (cond ((and predicate named)
	   `(,(if public 'define-public 'define)
	     (,(if (T? predicate) (symbolicate struct-name '?) predicate) s)
	       (and (pair? s) (list? (car s)) (memq ',struct-name (car s)))))
	  (else '(noop)))))

(define-macro (defstruct name-and-ops . slot-descriptions)
  (if (atom? name-and-ops)
      `(defstruct (,name-and-ops) ,@slot-descriptions)
      (let ((name (car name-and-ops))
	    (slots slot-descriptions)
	    (ops (all-ops (cdr name-and-ops))))
	`(begin ,(constructor name ops slots)
		,(accessors name ops slots)
		,(copier name ops)
		,(predicate name ops)))))
