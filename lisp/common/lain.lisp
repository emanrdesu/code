
(next-cycle world)
(incf cycles)
(print-world world cycles)))

(defun life (source)
  (let* ((size (length (car source)))
	 (life (make-world
		:size size
		:current (make-array (list size size) :element-type 'bit
				     :initial-contents source)
		:next (make-array (list size size) :element-type 'bit
				  :initial-element 0)
		:numdots 0)))


; This doesn't seem to be used
(defun environment-function (env fn)
  (function-information fn env))

; This worked in alpha but doesn't work anymore.
; I'm not to sure about this one! We should ask the Apple people
(defun environment-macro (env macro)
  (when env
    (let ((info (assoc macro (oddexenv.functions env))))
      (if info
	  (when (eq (cadr info) 'oddmacro)
	    (let ((my-look (cadr info)))
	      (if (listp my-look) my-look (list my-look nil nil nil))))
	  (environment-macro (oddexenv.parent env env) macro)))))
			  
