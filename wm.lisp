#!/usr/local/bin/sbcl --script
;;; Most simple window manager on earth. It is a fork from the lisp
;;; version of tinywm.

;; Load CLX and make a package
(require 'asdf)
(asdf:load-system :clx)
(defpackage :most.simple.wm
  (:use :common-lisp :xlib :sb-ext))
(in-package :most.simple.wm)

(defvar *display* (open-default-display))
(defvar *root* (screen-root (display-default-screen *display*)))
(defparameter *mouse-mod* '(:mod-1) "Modifier for mouse control")
(defparameter *move* 1 "Mouse button to move a window")
(defparameter *resize* 3 "Mouse button to resize a window")
(defparameter *prefix* '(:control #\t) "Prefix for shortcuts")

(defun mods (l) (butlast l))
(defun kchar (l) (car (last l)))
(defun compile-shortcut (l)
  "Compile a shortcut into a (state . code) form. For
example: (compile-shortcut '(:control #\t)) -> (4 . 44)"
  (let ((c (keysym->keycodes *display* (car (character->keysyms (kchar l))))))
    (cons (apply #'make-state-mask (mods l)) c)))

(defparameter *shortcuts* 
  (list (cons (compile-shortcut '(:shift #\q)) 'quit))
  "Shortcuts alist initialized with the quit command.")

(defmacro defshortcut (key &body body)
  "Define a new shortcut in *shortcuts* alist. The key in this alist
  is in (state . code) form and the associated value is a lambda
  without argument."
  (let ((sc (gensym)))
    `(let ((,sc (compile-shortcut ',key)))
       (pushnew (cons ,sc #'(lambda () ,@body)) *shortcuts* :test #'equal :key #'car))))

(defshortcut (#\c) (sb-ext:run-program "xterm" nil :wait nil :search t))
(defshortcut (#\e) (sb-ext:run-program "emacs" nil :wait nil :search t))
(defshortcut (#\w) (sb-ext:run-program "xxxterm" nil :wait nil :search t))
(defshortcut (#\n) (circulate-window-down *root*))

(defun main ()
  ;; Grab mouse buttons
  (dolist (button (list *move* *resize*))
    (grab-button *root* button '(:button-press) :modifiers *mouse-mod*))

  ;; Grab prefix
  (let ((code (keysym->keycodes *display* (car (character->keysyms (kchar *prefix*))))))
    (grab-key *root* code :modifiers (mods *prefix*)))

  (unwind-protect
       (let (last-button last-x last-y hidden waiting-shortcut)
         (loop named eventloop do
              (event-case 
                  (*display* :discard-p t)
                (:key-press
                 (code state window)
                 (cond (waiting-shortcut
                        (let ((entry (assoc (cons state code) *shortcuts* :test #'equal)))
                          (when entry
                            (let ((fn (cdr entry)))
                              (cond ((functionp fn) (funcall fn))
                                    ((eq fn 'quit) (return-from eventloop))))))
                        (ungrab-keyboard *display*)
                        (setf waiting-shortcut nil))
                       ((equal (cons state code) (compile-shortcut *prefix*))
                        (grab-keyboard *root*)
                        (setf waiting-shortcut t))))
                (:button-press
                 (code child)
                 (when child        ; do nothing if we're not over a window
                   (setf last-button code)
                   (grab-pointer child '(:pointer-motion :button-release))
                   (when (= code *resize*)
                     (warp-pointer child (drawable-width child) 
                                   (drawable-height child)))
                   (let ((lst (multiple-value-list (query-pointer *root*))))
                     (setf last-x (sixth lst)
                           last-y (seventh lst)))))
                (:motion-notify
                 (event-window root-x root-y)
                 (cond ((= last-button *move*)
                        (let ((delta-x (- root-x last-x))
                              (delta-y (- root-y last-y)))
                          (incf (drawable-x event-window) delta-x)
                          (incf (drawable-y event-window) delta-y)
                          (incf last-x delta-x)
                          (incf last-y delta-y)))
                       ((= last-button *resize*)
                        (let ((new-w (max 1 (- root-x (drawable-x event-window))))
                              (new-h (max 1 (- root-y (drawable-y event-window)))))
                          (setf (drawable-width event-window) new-w
                                (drawable-height event-window) new-h)))))
                (:button-release () (ungrab-pointer *display*))
                ((:configure-notify :exposure) () t))))
    (dolist (button (list *move* *resize*))
      (ungrab-button *root* button :modifiers *mouse-mod*))
    (let ((code (keysym->keycodes *display* (car (character->keysyms (kchar *prefix*))))))
      (ungrab-key *root* code :modifiers (mods *prefix*)))
    (close-display *display*)))

(main)
