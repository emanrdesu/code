
(use-modules
 (srfi srfi-1)
 (srfi srfi-9)
 (ice-9 session)
 (ice-9 popen)
 (ice-9 textual-ports)
 (ice-9 format)
 (ice-9 and-let-star))

(define-record-type function-prototype
  (makefp name return args) fp?
  (name fpname) (return fpret) (args fpargs))

(define (fp= fp1 fp2)
  (string= (fpname fp1) (fpname fp2)))

(define-macro (push x var)
  `(set! ,var (cons ,x ,var)))

(define-macro (pop var)
  (let ((v (make-symbol "")))
    `(if (null? ,var) #f
       (let ((,v (car ,var))) (set! ,var (cdr ,var)) ,v))))

(define-macro (dotimes of . body)
  (case (length of)
    ((2) `(dotimes (,@of (values)) ,@body))
    ((3) (let ((expr (make-symbol "")))
	   `(let ((,expr ,(cadr of)))
	      (do ((,(car of) 0 (1+ ,(car of))))
		  ((= ,(car of) ,expr) ,(caddr of)) ,@body))))))

(define (removen n pred lst)
  (cond ((or (zero? n) (null? lst)) lst)
	((pred (car lst)) (removen (1- n) pred (cdr lst)))
	(else (cons (car lst) (removen n pred (cdr lst))))))

(define (displayln x)
  (display x) (newline))

(define *compiler* "gcc")
(define *code* (list))
(define *mode* #:function)
(define *command-line-args* "")
(define *compiler-args*)
(define *function* (makefp "main" "int" "int argc, char ** argv"))
(define *function-recent* (list *function*))
(define *functions* (list *function*))
(define *undone* (list))
(define *multi-line* #f)
(define *quit* #f)
(define *errors*)
(define *error-keyword* "error")

(define *vars*
  '(*compiler* *code* *mode* *command-line-args*
    *compiler-args* *function* *function-recent*
    *functions* *undone* *multi-line* *error-keyword*)) 

(define (string->fp str)
  (define csw* (list->char-set '(#\*) char-set:whitespace))
  (let* ((pi (string-index str #\())
	 (w*i (string-index-right str csw* 0 pi))
	 (return (and w*i (substring str 0
			    ((if (char=? (string-ref str w*i) #\*) 1+ identity) w*i))))
	 (name (substring str (1+ (or w*i -1)) pi))
	 (args (substring str (1+ pi) (string-index str #\)))))
    (makefp name (or return "") args)))

(define (function-prototype? string)
  (define char-set:not-whitespace
    (char-set-complement char-set:whitespace))
  (and (string-index string #\()
       (string-index string #\))
       (< (string-index string #\()
	  (string-index string #\)))
       (< (string-index string char-set:not-whitespace)
	  (string-index string #\())))

(define* (find-function fname #:optional (functions *functions*))
  (find (λ (f) (string=? fname (fpname f))) functions))

(define* (remove-function fname #:optional (functions *functions*))
  (remove (λ (f) (string=? fname (fpname f))) functions))

(define (remove-function-code fname)
  (remove (λ (c) (and (eq? (car c) 'code)
		      (string=? (fpname (caddr c)) fname))) *code*))

(define (get-function-code fname)
  (map cadr (reverse (filter (λ (x) (and (eq? (car x) 'code)
					 (string= fname (fpname (caddr x))))) *code*))))

(define (display-function f)
  (format #t "~a ~a(~a)" (fpret f) (fpname f) (fpargs f)))


(define* (function #:optional fname)
  "Change current function so as to add new code to new function. If no function is given,
toggle between two most recently used functions. Give function name only, not prototype"
  (define (newf f)
    (push *function* *function-recent*)
    (set! *function* f))
  (if fname
      (let ((f (find-function fname)))
	(if f
	    (or (string=? (fpname *function*) fname) (newf f)) 
	    (format #t "Function \"~a\" is not defined\n\n" f)))
      (newf (pop *function-recent*))))


(define (define-function function-prototype)
  "Define function. Automically changes into new function
Type without ';'. Can't define already defined functions."
  (if (function-prototype? function-prototype)
      (let ((fp (string->fp function-prototype)))
	(if (find-function (fpname fp))
	    (format #t "~\"a\" is already defined. type \",uf ~a\" to undefine it\n\n"
		    (fpname fp) (fpname fp))
	    (begin (push fp *functions*)
		   (function (fpname fp)))))
      (format #t "\"~a\" doesn't look like a proper function prototype. try again\n\n"
	      function-prototype)))


(define (undefine-function fname)
  "Undefine function. Changes current function to old function. main can't be undefined"
  (unless (equal? fname "main")
    (set! *functions* (remove-function fname))
    (set! *code* (remove-function-code fname))
    (set! *function-recent* (remove-function *function-recent*))
    (when (string=? (fpname *function*) fname)
      (set! *function* (pop *function-recent*)))))



(define (list-functions)
  "List user defined functions and main"
  (for-each (λ (x) (display-function x) (display ";\n")) (reverse *functions*)))

(define (global-add code)
  "Add this code to global instead of current function"
  (push `(global ,code) *code*))

(define (global-mode)
  "Change into global mode. New code will be added to global"
  (set! *mode* #:global))

(define (function-mode)
  "Change into function mode. New code will be added to current function"
  (set! *mode* #:function))

(define* (command-line-arguments #:optional (args ""))
  "Set command line arguments to given args. If no given args, then set to nothing"
  (set! *command-line-args* args))
  
(define (set-compiler-args args)
  "Give compiler argument args whenever compiler is used.
Don't set name of output binary (i.e. -o foo) as that will interfere.
This command is supposed to be used for stuff like -O3"
  (set! *compiler-args* args))

(define (set-compiler compiler)
  "Set the compiler to given compiler. Default is gcc. Must be in path."
  (set! *compiler* compiler))

(define (errors)
  "Show errors/warnings from compiler"
  (display *errors*) (newline) (newline))

(define (set-error-keyword keyword)
  "Set the keyword that the compiler outputs to signify error. For gcc this is simply error"
  (set! *error-keyword* keyword))

(define (help)
  "Show this information"
  (define (pretty f)
    (with-output-to-string
      (lambda () (let ((parg (procedure-arguments f)))
		   (and (assq-ref parg 'required)
			(for-each (λ (r) (display r) (display #\space))
				  (assq-ref parg 'required)))
		   (and (assq-ref parg 'optional)
			(for-each (λ (o) (format #t "[~a] " o))
				  (assq-ref parg 'optional)))
		   (and (assq-ref parg 'rest)
			(format #t "[~a*] " (assq-ref parg 'rest)))))))
  (for-each
   (λ (c)
     (let ((p (pretty (cadr c))))
       (format #t ",~a ~a - " (car c) (pretty (cadr c)))
       (for-each
	(λ (k)
	  (for-each display
		    (make-list (+ 1 (string-length (car c)) 1
				  (string-length p) 1 1 1) #\space))
	  (display k) (display #\newline))
	(let ((ss (string-split (procedure-documentation (cadr c)) #\newline)))
	  (displayln (car ss)) (cdr ss))))) +commands+)
  (newline))

(define (help-help)
  "Show all help available"
  (help)
  (HELP))

(define (HELP)
  "Show tips and tricks"
  (for-each (λ (h) (for-each (λ (l) (display l) (newline)) h) (newline)) +help+))

(define (end-repl)
  "Quit REPL"
  (set! *quit* #t))

(define* (undo #:optional (n "1"))
  "Undo last n command(s). n is 1 by default. Not all commands are undoable.
Type ,H for more information"
  (dotimes (i (or (string->number n) (begin (display "invalid argument\n") 0)))
    (let ((p (pop *code*)))
      (and p (push p *undone*)))))

(define* (redo #:optional (n "1"))
  "Redo last n undone command(s). n is 1 by default"
  (dotimes (i (or (string->number n) (begin (display "invalid argument\n") 0)))
    (let ((r (pop *undone*)))
      (and r (push r *code*)))))

(define (list-code)
  "Show all user entered code"
  (let ((c 1))
    (for-each (λ (x) (format #t "~a. ~a\n" c x) (set! c (1+ c)))
	      (map cadr (reverse *code*))))
  (newline))

(define (list-full-code)
  "Show all code as given to the compiler"
  (for-each (λ (x) (format #t "~a\n" x))
	(string-split (all-code) #\newline))
  (newline))

(define (write-code path)
  "Write code (as given to compiler) to given path. Can be absolute or relative"
  (with-output-to-file path
    (lambda () (display (all-code)))))

(define (save-session path)
  "Save session into a file which can be used by ,gs to load the session"
  (with-output-to-file path
    (lambda () (write (map primitive-eval *vars*)))))

(define (get-session path)
  "Load session (from path) as saved by save-session"
  (with-input-from-file path
    (lambda () (map primitive-eval (map (λ (x) (cons 'set! x)) (zip *vars* (read)))))))
  
(define (print-working-directory)
  "Print current working directory"
  (system "pwd") (newline))

(define* (list-directory-contents #:optional (args ""))
  "Print files/directories under current directory"
  (system (format #f "ls ~a" args)) (newline))

(define (change-directory directory)
  "Change current directory"
  (chdir directory))

(define (clear-screen)
  "Clear entire screen"
  (system "clear"))

(define (shell-command command)
  "Run command in the shell."
  (system command))

(define* (delete-recent-code #:optional (n "1"))
  "Delete N most recent code in current function. Or global if in global mode"
  (and-let* ((n (string->number n)))
    (set! *code*
	  (removen n (if (eq? *mode* #:global)
			 (λ (x) (eq? (car x) 'global))
			 (λ (x) (fp= (caddr x) *function*))) *code*))))

(define (delete-user-line . line)
  "Delete certain line(s) from user entered code. line should be a number"
  (let ((t 0) (d (remove not (map string->number (string-split line char-set:whitespace)))))
    (set! *code* 
	  (let loop ((c (reverse *code*)) (b '()))
	    (set! t (1+ t))
	    (if (null? c) b
		(if (memv t d)
		    (loop (cdr c) b)
		    (loop (cdr c) (cons (car c) b))))))))


(define +commands+
  `(("f" ,function)
    ("df" ,define-function)
    ("uf" ,undefine-function)
    ("lf" ,list-functions)
    ("g" ,global-add)
    ("gm" ,global-mode)
    ("fm" ,function-mode)
    ("a" ,command-line-arguments)
    ("ca" ,set-compiler-args)
    ("sc" ,set-compiler)
    ("e" ,errors)
    ("sek" ,set-error-keyword)
    ("h" ,help)
    ("hh" ,help-help)
    ("H" ,HELP)
    ("q" ,end-repl)
    ("u" ,undo)
    ("r" ,redo)
    ("c" ,list-code)
    ("C" ,list-full-code)
    ("wc" ,write-code)
    ("ss" ,save-session)
    ("gs" ,get-session)
    ("pwd" ,print-working-directory) 
    ("ls" ,list-directory-contents)
    ("cd" ,change-directory)
    ("cls" ,clear-screen)
    ("!"  ,shell-command)
    ("dr" ,delete-recent-code)
    ("dl" ,delete-user-line)))

(define +help+
  '(("Code should always be finished with ';'"
    "Most code should just be a line, but multiline code is also accepted."
    "For multi line code , enter like so: ',{ int a = 0; ...  },' without the '"
    "Note that code inside the ,{ }, should not contain the the sequence },"
    "otherwise incorrect parsing will happen")

   ("main's prototype is \"int main(int argc, char ** argv)\"")
   
   ("stdio.h, string.h, ctype.h, and stdlib.h are included by default"
    "Typing #include <somelib.h> will automatically add to global"
    "Same with #define FOO whatever, or any preprocessor directives"
    "If for some reason you want to add the preprocessor directive in a function"
    "add a leading space like so \" #include ...\" and that will be added into global"
    "or current function depending on which mode is on")

   ("Not all commands are undoable. Only code you type in and ,g are undoable"
    ",g is just another way of typing code, so basically only code typed in can be undone"
    "Other commands offer a way to undo stuff. e.g. (,df and ,uf), (,l and ,ul)"
    "Of course, you can't undo an undo, instead, type, ,r (redo)")))


(define (add-code code)
  (push 
   (if (eq? *mode* #:global)
       `(global ,code)
       `(code ,code ,*function*)) *code*) #t)

(define (all-code)
  (define (is x) (λ (y) (eq? (car y) x)))
  (with-output-to-string
    (lambda ()
      (let ((*code* (reverse *code*)))
	(for-each displayln (map cadr (filter (is 'directive) *code*)))
	(newline)
	(for-each displayln (map cadr (filter (is 'global) *code*)))
	(newline)
	(for-each (λ (f)
		    (display-function f) (display " {\n")
		    (for-each displayln (get-function-code (fpname f)))
		    (display "}\n\n"))
		  *functions*)))))

(define* (call cmd #:optional arg)
  (let* ((parg (procedure-arguments cmd))
	 (r (assq-ref parg 'required)))
    (if (and (not (null? r)) (not arg))
	(displayln "Command needs 1 or more arguments to work. Try again")
	(if (not arg) (cmd) (cmd arg)))))

(define (process-line line)
  (define (illegal c)
    (format #f ",~a isn't recognized. type ,h to see all commands available\n\n" c))
  (cond ((eof-object? line) (set! *quit* #t) #f)
	((string-null? line) (add-code "") #t)
	(*multi-line*
	 (if (string-contains line "},")
	     (begin
	       (add-code (let ((a (string-append *multi-line* "\n" line)))
			   (string-trim (substring a 2 (string-contains a "},"))
					char-set:whitespace)))
	       (set! *multi-line* #f) #t)
	     (begin (set! *multi-line* (string-append *multi-line* "\n" line)) #f)))
	((memv (string-ref line 0) '(#\# #\,))
	 (case (string-ref line 0)
	   ((#\#) (push `(directive ,line) *code*))
	   ((#\,) (newline)
	          (let* ((wi (string-index line char-set:whitespace))
			 (cs (substring line 1 (if wi wi (string-length line))))
			 (f (assoc cs +commands+)))
		    (if f
			(let ((trim (string-trim-both
				     (substring line (1+ (string-length (car f))))
				     char-set:whitespace)))
			  (begin (if (string-null? trim)
				     (call (cadr f)) (call (cadr f) trim)) #f))
			(if (>= (string-length line) 2)
			    (if (char=? (string-ref line 1) #\{)
				(if (string-contains line "},")
				    (add-code (string-trim
					       (substring line 2
							  (string-contains line "},"))
					       char-set:whitespace))
				    (begin (set! *multi-line* line) #f))
				(begin (display (illegal cs)) #f))
			    (begin (display (illegal "")) #f)))))))
	(else (add-code line))))

(define (pre-repl)
  (push '(directive "#include <stdio.h>") *code*)
  (push '(directive "#include <string.h>") *code*)
  (push '(directive "#include <stdlib.h>") *code*)
  (push '(directive "#include <ctype.h>") *code*))

(define (repl)
  (define mn 8192)
  (pre-repl)
  (let loop ((line "") (n (random mn)))
    (unless *quit*
      (when (process-line line)
	(with-output-to-file (format #f "/tmp/tmp~a.c" n)
	  (lambda () (display (all-code))))
	(let* ((p (open-input-pipe (format #f "~a -o /tmp/a.out /tmp/tmp~a.c ~a"
					   *compiler* n *compiler-args*)))
	       (o (get-string-all p)))
	  (set! *errors* o)
	  (if (string-contains o *error-keyword*)
	      (displayln "Compiler error, type ,e to see.")
	      (begin (unless (string-null? o)
		       (displayln "Compiler warnings, type ,e to see."))
		     (system "/tmp/a.out")))
	  (close-pipe p)
	  (delete-file (format #f "/tmp/tmp~a.c" n))
	  (delete-file "/tmp/a.out")))
      (loop (get-line (current-input-port)) (random mn)))))

(newline)
(repl)
