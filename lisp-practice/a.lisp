;(load (compile-file "lisp-practice.lisp"))
(load "lisp-practice")

(load-db (prompt-read "DB file name"))

(dump-db)
