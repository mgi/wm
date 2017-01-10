;;; -*- Mode: Lisp -*-
(in-package :asdf-user)

(asdf:defsystem :wm
  :name "Simple Common Lisp X Window Manager"
  :author "Manuel Giraud <manuel@ledu-giraud.fr>"
  :serial t
  :depends-on (:swank
               :clx)
  :components ((:file "wm")))
