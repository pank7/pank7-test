(defun print-number (num)
  (format t "=> ~D~%" num))

;(print-number (car (cdr (list 1 2 3))))

;(print-number (getf (list :a 1 :b 2 :c 3) :b))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun print-cd (cd)
  (format t "| ~{~a: ~a | ~}~%" cd))

(defvar *db* nil)

(defun dump-db ()
  (dolist (cd *db*)
    (print-cd cd)))

(defun add-record (cd) (push cd *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
		  (with-standard-io-syntax
		   (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
;(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
;(add-record (make-cd "Home" "Dixie Chicks" 9 t))
;(add-cds)
;(save-db (prompt-read "DB file name"))

;(load-db (prompt-read "DB file name"))

;(dump-db)

