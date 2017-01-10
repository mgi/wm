(load (merge-pathnames "prog/lisp/quicklisp/setup.lisp" (user-homedir-pathname)))
(push (directory-namestring *load-pathname*) asdf:*central-registry*)
(ql:quickload 'wm)
;; not reached
