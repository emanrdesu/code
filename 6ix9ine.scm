
(use-modules (goodies) (ice-9 q) (srfi srfi-1) (ice-9 format))

(def A (char->integer #\A))
(def vowels '(#\A #\E #\I #\O #\U))
(def consonants
  (lset-difference char=?
    (map integer->char (map (curry + A) (iota 26))) vowels))

(def (n->o n) (string->symbol (format #f "~:r" n)))

(defun (6ix9ine #:key (length 4) #:allow-other-keys #:rest nth)
  (def (given? k nth) (memv (symbol->keyword (n->o k)) nth))
  (def get-nth (compose cadr given?))
  (let ((song-name (make-q)))
    (dotimes (i length (list->symbol (car song-name)))
      (enq! song-name
	    (if (given? (1+ i) nth)
		(char-upcase (get-nth (1+ i) nth))
		(randelt (if (even? i) consonants vowels)))))))
