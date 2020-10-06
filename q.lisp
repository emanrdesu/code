
(defpackage #:q
  (:use #:common-lisp)
  (:export #:make-q #:qpl #:qp #:nulqp #:nulq #:enq #:deq 
	   #:qpush #:qlength #:qfront #:qback
	   #:sync-q #:apply-to-q))

(cl:in-package #:q)

;;; a queue is a data structure that can best be described as a line 
;;; (as in the line to get lunch in public high school),
;;; whoever came in last, gets lunch last, whoever came first, gets lunch first
;;; this queue implementation uses a cons, whose car is the "line" (a list), and cdr is
;;; a reference to the line's last cons (the cons containing "the last person in the line")

(defun make-q (&optional from)
  "Make a queue. FROM must be a proper list, from which the queue is constructed."
  (let ((q (cons t t)))
    (setf (car q) from) (sync-q q) q))

(defun qpl (q)
  "Queue predicate lite. Tests if Q looks like a queue. Use qp for a more precise result."
  (and (consp q)
       (listp (car q))
       (listp (cdr q))))

(defun qp (q)
  "Queue predicate. Returns t if Q is a queue, nil otherwise."
  (and (qpl q) (eq (last (car q)) (cdr q))))

(defun nulqp (q)
  "Null queue predicate. Returns t if Q is empty, nil otherwise."
  (equal q '(nil)))

(defun enq (q x)
  "Add in some X to back of the queue. Returns Q"
  (if (nulqp q)
      (setf (car q) (list x)
	    (cdr q) (car q))
      (setf (cddr q) (list x)
	    (cdr q) (cddr q))) q)

(define-condition nulq (error) ()
  (:report (lambda (c s)
	     (declare (ignore c))
	     (print "the q is null/empty" s))))

(defun deq (q)
  "Delete the first element in Q and return it. If Q is null/empty, raise an error."
  (and (nulqp q) (error 'nulq))
  (prog1 (pop (car q))
    (unless (car q) (sync-q q))))

(defun qpush (q x)
  "Push some X to the front of Q. Returns Q."
  (push x (car q)) q)

(defun qlength (q)
  "Return number of elements in Q"
  (length (car q)))

(defun qfront (q)
  "Return first element in Q, without removing it. Raises error if Q is null/empty"
  (and (nulqp q) (error 'nulq))
  (caar q))

(defun qback (q)
  "Return last element in Q, without removing it. Raises error if Q is null/empty"
  (and (nulqp q) (error 'nulq))
  (cadr q))

(defun sync-q (q)
  "Recompute last cons cell in Q."
  (setf (cdr q) (last (car q))) q)

(defun apply-to-q (q &rest functions)
  "Transform Q using FUNCTIONS, in order. FUNCTIONS should be unary and take in lists."
  (dolist (f functions)
    (setf (car q) (funcall f (car q))))
  (sync-q q) q)
