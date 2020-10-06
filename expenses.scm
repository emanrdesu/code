
(define-module (expenses)
  #:use-module (goodies)
  #:use-module (ice-9 format)
  #:use-module(srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)

  #:export (add-expense
	    remove-expenses
	    expenses-summary))

(define data-file "/home/gabriel/.cache/expenses")

(define months
  '((1 jan . january)
    (2 feb . february)
    (3 mar . march)
    (4 apr . april)
    (5 may . may)
    (6 jun . june)
    (7 jul . july)
    (8 aug . august)
    (9 sep . september)
    (10 oct . october)
    (11 nov . november)
    (12 dec . december)))

(define (abbr->month a)
  (or (assq-ref (map cdr months) a) a))

(define (month->abbr m)
  (string->symbol (substring (symbol->string m) 0 3)))

(define (fix-date date)
  (match-let (((abbr day year) date))
    (list (abbr->month abbr) day year)))

(define (cat-filter cat)
  (lambda (ecat)
    (or (not (null? (lset-intersection eq? (filter symbol? cat) ecat)))
	(any (curry lset= eq? ecat)
	     (filter (negate symbol?) cat)))))


(defun (add-expense cat amount #:optional date)
  (cond ((not (rational? amount))
	 (error "amount is an invalid number"))
	((and date (not (any (curry memq (car date)) (map (○ pair->list cdr) months))))
	 (error "month is invalid")))
  (let ((expenses (with-input-from-file data-file read))
	(expense (list (or (and date (fix-date date))
			   ((f..x (○ cdr (curry assv-ref months) date-month)
				  date-day
				  date-year)
			    (current-date)))
		       (if (list? cat) cat (list cat)) 
		       amount)))
    (with-output-to-file data-file
      (thunk (write (cons expense expenses))))))


(defun (remove-expenses dmy #:optional cat)
  (let ((expenses (with-input-from-file data-file read))
	(cat-filter (or (and cat (○ (cat-filter (if (symbol? cat) (list cat) cat)) cadr))
			(const #t))))
    (with-output-to-file data-file
      (thunk
	(write 
	 (match dmy
	   ((month day year)
	    (filter (○ not (conjoin (○ (curry equal? `(,(abbr->month month) ,day ,year))car)
				    cat-filter))
		    expenses))
	   ((month year)
	    (filter (○ not (conjoin (○ (lambda (edmy)
					 (length=? 1
					   (lset-difference eqv? edmy
					     `(,(abbr->month month) ,year))))
				       car)
				    cat-filter))
		    expenses))
	   (year
	    (filter (○ not (conjoin (○ (curry = year) caddr car) cat-filter))
		    expenses))))))))


(defun (expenses-summary #:optional dmy cat)
  (define (display-summary expenses)
    (for-each displayln expenses)
    (unless (null? expenses)
      (format #t "Total: ~a~%" (apply + (map caddr expenses)))))
  (let ((expenses (with-input-from-file data-file read))
	(cat-filter (or (and cat (○ (cat-filter (if (symbol? cat) (list cat) cat)) cadr))
			(const #t))))
    (display-summary 
     (if (not dmy)
	 expenses
	 (match dmy
	   ((month day year)
	    (filter (conjoin (○ (curry equal? `(,(abbr->month month) ,day ,year)) car)
			     cat-filter)
		    expenses))
	   ((month year)
	    (filter (conjoin (○ (lambda (edmy)
				  (length=? 1
					    (lset-difference eqv? edmy
					       `(,(abbr->month month) ,year))))
				car)
			     cat-filter)
		    expenses))
	   (year
	    (filter (conjoin (○ (curry = year) caddr car) cat-filter)
		    expenses)))))))
