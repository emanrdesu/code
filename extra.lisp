
(defpackage #:extra
  (:nicknames #:x)
  (:use #:common-lisp #:alexandria)
  (:export #:synonymize-macro #:synonymize-function #:thunk
	      #:reverse-args #:difun #:funmacro #:nq
	      #:save-place #:save-places #:eval-dammit
	   #:def #:define #:mvb #:mvl #:mvs
	      #:debind #:decase #:deecase #:deccase #:id #:^ #:○ #:!
	   #:P #:C #:non-zero-integer-p #:perfect-square-p #:quadratic-solve
	   #:flip #:negate #:tuply #:untup #:curryn #:gx.fx #:x.fx #:fx.x 
	   #:string-null-p #:singletonp #:chunk #:take #:drop #:insert
	      #:replicate #:cartesian-product #:intersperse #:concat
	   #:sow-seed #:span-compound #:span-iterate #:compound #:iterate))

(cl:in-package #:extra)

;;; macros

(defmacro synonymize-macro (synonym existing)
  "Creates a macro called SYNONYM which does what EXISTING does.
Inherits documentation and arglist."
  `(setf (macro-function ',synonym) (macro-function ',existing)))

(defmacro synonymize-function (synonym existing &key setf)
  "Creates a function called SYNONYM which does what EXISTING does.
Inherits documentation. If setf is t, (setf existing) must be defined and be a function."
  `(progn (setf (fdefinition ',synonym) (fdefinition ',existing))
	  (and ,setf (setf (fdefinition '(setf ,synonym))
			   (fdefinition '(setf ,existing))))))

(defmacro thunk (&body body)
  "Return a function with BODY and an &rest argument.
The &rest variable symbol is uninterned, or gensym'd, so there's no way to refer to it."
  (let ((rest (gensym)))
    `(lambda (&rest ,rest)
       (declare (ignore ,rest)) ,@body)))

(defmacro reverse-args (function &rest args)
  "Give to FUNCTION the arguments in ARGS, but in reverse.
FUNCTION can also be a macro."
  `(,function ,@(reverse args)))

(defmacro difun (name function &optional documentation)
  "Defun NAME to apply FUNCTION to arbitrary &rest args."
  `(defun ,name (&rest args) ,documentation
     (apply ,function args)))

(defmacro funmacro (name function)
  "Create a new macro that does what function does. Useful for not having to quote arguments.
Everything is going to be quoted, i.e. not useful when you need (NAME (some-function ...))
because the list (some-function ...) is going to be passed in, not the eval of it.
FUNCTION can be a symbol, in which case the symbol-function is going to be used,
or a list which represents code, than when eval'd, returns a function."
  `(defmacro ,name (&rest args)
     (list* 'funcall ',(if (listp function) function (list 'quote function))
	    (mapcar (curry 'list 'quote) args))))

(defmacro nq (function &rest args)
  "\"No quote\". Call function with quoted ARGS."
  `(,function ,@(mapcar (curry 'list 'quote) args)))

(defmacro save-place ((place &optional (new nil newp)) &body body &environment env)
  "Leave PLACE as if body had never run. NEW, if given,
is the value to be setf'd to place after body has run."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (when (cdr store-vars) (error "Can't expand this place ~S" place))
    (let ((name (gensym)))
      `(let* (,@(mapcar (function list) vars vals)
	      (,(car store-vars) ,reader-form))
	 
	 (let ((,name ,(if newp new (car store-vars)))) ; (or ,new ,(car store-vars))))
	   (unwind-protect (progn ,@body)
	     (setf ,(car store-vars) ,name)
	     ,writer-form))))))

(defmacro save-places (places &body body)
  "Save multiple places with the save-place macro.
Each item in PLACES must be a list, which can be either of 1 or 2 elements.
If its 1 element, then the element must be the place.
If its 2 elements, then the car must be the place, and the cadr must be a value,
which is setf'd to the place after body has run."
  (if (null places)
      `(progn ,@body)
      `(save-place ,(car places)
	 (save-places ,(cdr places) ,@body))))

(defmacro eval-dammit (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

(eval-dammit
  (synonymize-macro def defparameter)
  (synonymize-macro define defparameter)

  (synonymize-macro debind destructuring-bind)
  (synonymize-macro decase destructuring-case)
  (synonymize-macro deccase destructuring-ccase)
  (synonymize-macro deecase destructuring-ecase)

  (synonymize-macro mvb multiple-value-bind)
  (synonymize-macro mvl multiple-value-list)
  (synonymize-macro mvs multiple-value-setq)

  (synonymize-function id identity)
  (synonymize-function ^ expt)
  (synonymize-function ○ compose)
  (synonymize-function ! factorial))

;;; math related

(defun P (n r)
  "Combinatorics nPr, or P(N,R)."
  (/ (! n) (! (- n r))))

(defun C (n r)
  "Combinatorics nCr, or C(N,R), N choose R."
  (/ (P n r) (P r r)))

(defun non-zero-integer-p (n)
  "Tests that n is an integer and not zero."
  (and (integerp n) (not (zerop n))))

(defun perfect-square-p (n)
  "Test if N (a positive integer) can be of form x^2, where x is also an integer."
  (mvb (i e) (truncate n)
    (and (or (positive-integer-p i) (zerop i))
         (zerop e)
	 (= (expt (isqrt i) 2) i))))

(defun quadratic-solve (a b c &optional (as-number t anp))
  "Solve for x in Ax^2 + Bx + C = 0. A, B, and C can't be 0.
AS-NUMBER details whether to return an actual number
or lisp code that when eval'd, results in the same number."
  (assert (every 'non-zero-integer-p (list a b c)))
  (let ((discriminant (- (* b b) (* 4 a c))))
    (if (and (perfect-square-p discriminant) as-number)
	(values (/ (+ (- b) (isqrt discriminant)) (* 2 a))
		(/ (- (- b) (isqrt discriminant)) (* 2 a)))
	(values (funcall (if (and anp as-number) 'eval 'identity)
			 `(/ (+ ,(- b) (sqrt ,discriminant)) ,(* 2 a)))
		(funcall (if (and anp as-number) 'eval 'identity)
			 `(/ (- ,(- b) (sqrt ,discriminant)) ,(* 2 a)))))))

;;; function related

(defun flip (f)
  "Return a function like F, except parameter list is reversed."
  (lambda (&rest args)
    (apply f (reverse args))))

(defun negate (f)
  "Return a function which is the predicate inverse of F."
  (lambda (&rest args)
    (not (apply f args))))

(defun tuply (f)
  "Return a function like F, except it takes a list of arguments instead of just arguments."
  (lambda (args) (apply f args)))

(defun untup (f)
  "Given F, which is tuply'd, return untuply'd F."
  (lambda (&rest args) (funcall f args)))

(defun curryn (f &rest args)
  "Returns a function like F, but the nth args are already set.
ARGS elements can be of form (N . ARG) or (N ARG). smallest N is 1."
  (lambda (&rest rest)
    (apply f
      (reduce
       (lambda (acc x)
	 (insert (ensure-car (cdr x)) (1- (car x)) acc))
       args :initial-value rest))))

(defun gx.fx (g f)
  "Return a function which takes in some x, and returns ((G x) . (F x))."
  (lambda (x)
    (cons (funcall g x) (funcall f x))))

(difun x.fx (curry 'gx.fx 'identity)
  "Return a function which takes in some x, and returns (x . (F x)).")

(difun fx.x (rcurry 'gx.fx 'identity)
  "Return a function which takes in some x, and returns ((F x) . x).")

;;; sequence related

(defun singletonp (seq)
  "A predicate for knowing if SEQ has only 1 element in it."
  (if (listp seq)
      (and (consp seq) (null (cdr seq)))
      (= (length seq) 1)))

(difun string-null-p  (curry 'string-equal "")
  "Check if a string is empty (equal to \"\").")

(defun chunk (n seq)
  "Split SEQ into sequences each with N elements, only if (length SEQ) mod N = 0.
If the mod N /= 0, then only the last element will be less than N elements.
Returned in a list."
  (if (emptyp seq) nil
      (cons (take n seq)
	    (chunk n (drop n seq)))))

(defun take (n seq)
  "Take the first N elements of SEQ and return them as SEQ's type.
If N > (length SEQ), then return SEQ. 
For lists, new cons are constructed and length isn't computed."
  (labels ((take-array (n seq)
	     (if (>= n (length seq))
		 seq
		 (subseq seq 0 n)))
	   (take-list (n list)
	     (if (or (null list) (zerop n)) '()
		 (cons (car list) (take-list (1- n) (cdr list))))))
    (funcall (if (listp seq) #'take-list #'take-array) n seq)))

(defun drop (n seq)
  "Return a subsequence of SEQ without its first N elements."
  (if (listp seq)
      (nthcdr n seq)
      (if (>= n (length seq))
	  (subseq seq 0 0)
	  (subseq seq n))))

(defun insert (x n list)
  "Insert element X into nth position of LIST. smallest N is 0."
  (nconc (take n list) (cons x (drop n list))))

(defun replicate (n x)
  "Return a list with N elements, all of which are X and eq to each other."
  (make-list n :initial-element x))
    
(defun cartesian-product (&rest lists)
  "Return { (r1,r2,...,rn) | r1 <- R1, r2 <- R2, ... rn <- RN }
where n is (length LISTS) and Rk is the kth list in LISTS. 1 <= k <= n.
each list in LISTS should represent a set."
  (reduce
   (lambda (xs ys)
     (apply 'append (mapcar (lambda (x) (mapcar (curry 'cons x) ys)) xs)))
   lists :from-end t  :initial-value '(())))

(defun intersperse (x list &optional (n 1))
  "Return LIST but every other N element(s) is X.
LIST is returned if its nil or has <= N element(s)."
  (if (or (null list) (null (nthcdr n list))) list
      (nconc (take n list) (list x) (intersperse x (drop n list)))))

(difun concat (curry 'concatenate 'string)
  "Concatenate &rest sequences into a string.
All should be strings, or list/vectors with chars.")

;;; miscellaneous

(defun sow-seed ()
  "Randomize the seed for #'random."
  (setf *random-state*
	(make-random-state t)))

(defun span-compound (f init until &key q last)
  "Return (list (F INIT) (F (F INIT)) ...),  list stops when UNTIL returns t for 
(F ... (F INIT)). Q decides if to return a queue or list, LAST decides 
if to include the element for which UNTIL returned t."
  (loop :as val := init :then (funcall f val) :and
            col := (q:make-q) :then (q:enq col val)
        :when (funcall until val)
          :return (funcall (if q #'id #'car)
			   (if last (q:enq col val) col))))

(defun span-iterate (producer until &key q last)
  "Return (list (PRODUCER) (PRODUCER) ...), list stops when UNTIL returns T for (PRODUCER).
PRODUCER should be some i/o procedure or non-deterministic procedure.
Q decides if to return a queue or a list. LAST decides if to include 
the (PRODUCER) which UNTIL returned t for."
  (span-compound (thunk (funcall producer)) (funcall producer) until :q q :last last))

(defun compound (f init until &key prev last)
  "Keep calling F on what it just returned, starting with INIT
until UNTIL returns t for (F (F ... (F INIT)). PREV is a helper argument, should be ignored.
But in the case where UNTIL returns t on (F INIT), you can use PREV as a \"default\"
value to return, or just use LAST.
LAST decides if to return the value for which UNTIl returned t,
or the value for which it last returned nil."
  (if (funcall until init) (if last init prev)
      (compound f (funcall f init) until :prev init :last last)))

(defun iterate (producer until &key last)
  "Keep calling PRODUCER until UNTIL returns t on PRODUCER's value. LAST decides if to return
the (PRODUCER) which UNTIL returned t for, or the (PRODUCER) UNTIL last returned nil for."
  (compound (thunk (funcall producer)) (funcall producer) until :last last))
