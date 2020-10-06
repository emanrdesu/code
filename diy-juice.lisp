
(defparameter +vape+ "/home/gabriel/.local/share/vape/")
(defparameter +recipes+ (concat +vape+ "recipes"))
(defparameter +owned+ (concat +vape+ "owned"))

(defparameter *recipes* nil)
(defparameter *owned* nil)

(defconstant h2o-g/ml 1)
(defconstant pg-g/ml 519/500)
(defconstant vg-g/ml 63/50)
(defconstant nic-g/ml 101/100)

(eval-dammit
  (synonymize-function recipe-name car :setf t)
  (synonymize-function steep-time caadr :setf t) ; unit is days
  (synonymize-function recommended-vg cadadr :setf t)
  (difun recipe-notes (○ 'third 'second))
  (synonymize-function %.flavors cddr :setf t)
  (synonymize-function flavor cadr :setf t)
  (difun flavors (○ (curry 'mapcar 'flavor) '%.flavors))
  (difun flavors% (○ (tuply '+) (curry 'mapcar 'car) '%.flavors))
  (difun % (curry '* 1/100)))

(defun add-recipe (recipe)
  (push recipe *recipes*))

(funmacro ar (untup 'add-recipe))

(defun recipe (name &optional (recipes *recipes*))
  (find name recipes :key 'recipe-name :test 'equal))

(defun flavor-frequencies (&optional (recipes *recipes*))
  (mapcar 'flavor (apply 'append (mapcar '%.flavors recipes))))

(defun recipes-flavors (&optional (recipes *recipes*))
  (delete-duplicates (flavor-frequencies recipes) :test 'equal))

(defun only-vendors (vendors &optional (flavors *owned*))
  (remove-if-not (rcurry 'member vendors) flavors :key 'car))

(defun flavor-count (flavor &optional (recipes *recipes*))
  (count flavor (flavor-frequencies recipes) :test 'equal))

(defun flavors-counts (&optional (flavors *owned*) (recipes *recipes*))
  (sort (mapcar (x.fx (rcurry 'flavor-count recipes)) flavors) '> :key 'cdr))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((get-flavors (remove &optional (recipes *recipes*))
	   (mapcar 'car
		   (funcall remove (compose (curry '= 1) 'cdr)
			    (mapcar (x.fx (rcurry 'flavor-count recipes))
				    (recipes-flavors recipes))))))
    (difun common-flavors (curry #'get-flavors 'remove-if))
    (difun unique-flavors (curry #'get-flavors 'remove-if-not))))

(defun common-recipes (&optional (recipes *recipes*))
  (let ((uniq (unique-flavors recipes)))
    (remove-if (lambda (r) (intersection (flavors r) uniq :test 'equal)) recipes)))

(defun unique-recipes (&optional (recipes *recipes*))
  (set-difference recipes (common-recipes recipes) :test 'equal))

(defun recipes-with (flavors &optional (recipes *recipes*))
  (remove-if-not (lambda (r) (subsetp flavors (flavors r) :test 'equal)) recipes))

(defun craftable-recipe-p (recipe &optional (flavors *owned*))
  (subsetp (flavors recipe) flavors :test 'equal))

(defun craftable-recipes (&optional (recipes *recipes*) (flavors *owned*))
  (remove-if-not (rcurry 'craftable-recipe-p flavors) recipes))

(defun unused-flavors (&optional (recipes (craftable-recipes)) (flavors *owned*))
  (set-difference flavors (recipes-flavors recipes) :test 'equal))

(defun flavors-needed (recipe &optional (flavors *owned*))
  (set-difference (flavors recipe) flavors :test 'equal))

(defun recipes-achievability (&optional (recipes *recipes*) (flavors *owned*))
  "Return, in order, from most easily achievable recipe (given FLAVORS)
to hardest achievable recipe, a list of with element of form (recipe-name . flavors-needed)"
  (sort (mapcar (gx.fx 'car (rcurry 'flavors-needed flavors))
		(remove-if (rcurry 'craftable-recipe-p flavors) recipes))
	'< :key (compose 'length 'cdr)))


(defun pure-nicotine% (strength)
  (/ strength 10))

(defun nicotine-non-nic%  (strength)
  (- 100 (pure-nicotine% strength)))

(defun nicotine-pg% (strength &key (pg 100))
  (* (% pg) (nicotine-non-nic% strength)))

(defun nicotine-vg% (strength &key (pg 100))
  (* (% (- 100 pg)) (nicotine-non-nic% strength)))

(defun nicotine-g/ml (strength &key (pg 100))
  (assert (<= 0 pg 100))
  (% (+ (* (pure-nicotine% strength) nic-g/ml)
	(* (nicotine-pg% strength :pg pg) pg-g/ml)
	(* (nicotine-vg% strength :pg pg) vg-g/ml))))

(defun mix-recipe (recipe &key (vg 70) (target-ml 30) (nicotine 3)
			       (nicotine-strength 48) (nicotine-pg 100))
  (let* ((nicotine-g/ml (nicotine-g/ml nicotine-strength :pg nicotine-pg))
	 (nicotine-ml (/ (* nicotine target-ml) nicotine-strength))
	 (nicotine-grams (* nicotine-ml nicotine-g/ml))
	 (nicotine% (* (/ nicotine-ml target-ml) 100))
	 (vg% (- vg (* nicotine% (% (- 100 nicotine-pg)))))
	 (vg-ml (* (% vg%) target-ml))
	 (vg-grams (* vg-ml vg-g/ml))
	 (flavors-ml-g-% (mapcar (tuply (lambda (% f &aux (% (% %)))
					  (list f (* % target-ml)
						(* % target-ml h2o-g/ml) (* % 100))))
				 (%.flavors recipe)))
	 (pg% (apply '- 100 vg (* nicotine% (% nicotine-pg))
		    (mapcar 'fourth flavors-ml-g-%)))
	 (pg-ml (* (% pg%) target-ml))
	 (pg-grams (* pg-ml pg-g/ml)))
    (list* (recipe-name recipe)
           (list 'nicotine nicotine-ml nicotine-grams nicotine%)
	   (list 'pg pg-ml pg-grams pg%)
	   (list 'vg vg-ml vg-grams vg%) flavors-ml-g-%)))


(let ((mft (lambda (f)
	     (lambda (mix) (apply '+ (mapcar f (cdr mix)))))))
  (difun mix-ml-total (funcall mft 'second))
  (difun mix-g-total (funcall mft 'third))
  (difun mix-%-total (funcall mft 'fourth)))

(defun print-mix (mix)
  (save-place (*print-pprint-dispatch* (copy-pprint-dispatch))
    (set-pprint-dispatch 'ratio
      (lambda (s n) (format s "~f" n)))  (mapc 'print mix)))
			      
(difun mix (○ 'print-mix 'mix-recipe))

(defun load-recipes ()
  (with-open-file (*standard-input* +recipes+)
    (setf *recipes* (read))))

(defun write-recipes ()
  (with-open-file (*standard-output* +recipes+
		    :direction :output :if-exists :overwrite)
    (prin1 *recipes*)))

(defun load-owned ()
  (with-open-file (*standard-input* +owned+)
    (setf *owned* (read))))

(defun write-owned ()
  (with-open-file (*standard-output* +owned+
		    :direction :output :if-exists :overwrite)
    (prin1 *owned*)))

(load-recipes)
(load-owned)
