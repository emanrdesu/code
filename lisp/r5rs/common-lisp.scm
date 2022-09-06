
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
    ((dolist (var list . ret) body0 . body)
     (begin (for-each (lambda (var) body0 . body) list) . ret))))

(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (var expr . ret) . body)
     (let ((res expr))
       (do ((var 0 (+ var 1)))
	   ((= var res) . ret) . body)))))

(define-syntax setf!
  (syntax-rules ()
    ((setf! var val) (set! var val))
    ((setf! var val . rest)
     (begin (set! var val)
	    (setf! . rest)))))

(define-syntax push!
  (syntax-rules ()
    ((push! x place)
     (set! place (cons x place)))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! place)
     (begin1 (car place) (set! place (cdr place))))))


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

(define (mapcan f . lists)
  (apply append (apply map f lists)))

(define (atom? x)
  (not (or (pair? x) (list? x))))

(define (yes-or-no? question)
  (display question)
  (display " (yes/no): ")
  (case (read)
    ((yes y) #t)
    ((no n) #f)
    (else
     (display "Please enter y, yes, n or no.\n")
     (yes-or-no? question))))
