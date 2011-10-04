#!/usr/local/bin/sbcl --script
;;; Used to be the most simple window manager on earth. It is a fork
;;; from the lisp version of tinywm.

;;; Load CLX and make a package
(require 'asdf)
(asdf:load-system :clx)
(defpackage :most.simple.wm
  (:use :common-lisp :xlib :sb-ext))
(in-package :most.simple.wm)

(defvar *display* (open-default-display))
(defvar *root* (screen-root (display-default-screen *display*)))

(defun mods (l) (butlast l))
(defun kchar (l) (car (last l)))
(defun compile-shortcut (l)
  "Compile a shortcut into a (state . code) form. For
example: (compile-shortcut '(:control #\t)) -> (4 . 44). Works also
for mouse button."
  (let ((k (kchar l))
        (state (apply #'make-state-mask (mods l))))
    (if (characterp k)
        (let ((c (keysym->keycodes *display* (car (character->keysyms k)))))
          (cons state c))
        (cons state k))))
(defun state (l) (car l))
(defun code (l) (cdr l))

(defparameter *shortcuts* 
  (list (cons (compile-shortcut '(:shift #\q)) 'quit))
  "Shortcuts alist initialized with the quit command.")

(defparameter *windows*
  (member-if #'(lambda (w) (and (eql (window-map-state w) :viewable) 
                                (eql (window-override-redirect w) :off)))
             (query-tree *root*))
  "List of managed windows")

(defmacro defshortcut (key &body body)
  "Define a new shortcut in *shortcuts* alist. The key in this alist
  is in (state . code) form and the associated value is a lambda
  without argument."
  (let ((sc (gensym)))
    `(let ((,sc (compile-shortcut ',key)))
       (pushnew (cons ,sc #'(lambda () ,@body)) *shortcuts* :test #'equal :key #'car))))

(defun next (display &optional (way #'1+))
  (when *windows*
    (let* ((cw (input-focus display))
           (ncw (position-if #'(lambda (w) (window-equal w cw)) *windows*))
           (n (length *windows*))
           (next (mod (funcall way ncw) n)))
      (unless (= next ncw)
        (focus (nth next *windows*))))))

(defun focus (window)
  (setf (window-priority window) :above)
  (set-input-focus *display* window :pointer-root))

;;; User settings
(defparameter *prefix* '(:control #\t) "Prefix for shortcuts")
(defparameter *move* '(:mod-1 1) "Mouse button to move a window")
(defparameter *resize* '(:mod-1 3) "Mouse button to resize a window")
(defshortcut (#\c) (run-program "xterm" nil :wait nil :search t))
(defshortcut (#\e) (run-program "emacs" nil :wait nil :search t))
(defshortcut (#\w) (run-program "xxxterm" nil :wait nil :search t))
(defshortcut (:control #\l) (run-program "xlock" nil :wait nil :search t))
(defshortcut (#\n) (next *display*))
(defshortcut (#\p) (next *display* #'1-))

;;; Modifier keypress avoidance code
(defvar *mods-code* (multiple-value-call #'append (modifier-mapping *display*)))

(defun is-modifier (keycode)
  "Return t if keycode is a modifier"
  (find keycode *mods-code* :test #'eql))

;;; Main
(defun main ()
  (let ((prefix (compile-shortcut *prefix*))
        (move (compile-shortcut *move*))
        (resize (compile-shortcut *resize*))
        last-button last-x last-y waiting-shortcut)

    ;; Grab prefix and mouse buttons on root
    (grab-key *root* (code prefix) :modifiers (state prefix))
    (grab-button *root* (code move) '(:button-press) :modifiers (state move))
    (grab-button *root* (code resize) '(:button-press) :modifiers (state resize))

    (setf (window-event-mask *root*) '(:substructure-notify))

    (unwind-protect
         (loop named eventloop do
              (event-case 
               (*display* :discard-p t)
               (:key-press
                (code state)
                (cond (waiting-shortcut
                       (unless (is-modifier code)
                         (let ((entry (assoc (cons state code) *shortcuts* :test #'equal)))
                           (when entry
                             (let ((fn (cdr entry)))
                               (cond ((functionp fn) (funcall fn))
                                     ((eq fn 'quit) (return-from eventloop))))))
                         (ungrab-keyboard *display*)
                         (setf waiting-shortcut nil)))
                      ((and (= state (state prefix)) (= code (code prefix)))
                       (grab-keyboard *root*)
                       (setf waiting-shortcut t))))
               (:button-press
                (code state child)
                (when (and child (eql (window-override-redirect child) :off))
                  (setf last-button code)
                  (grab-pointer child '(:pointer-motion :button-release))
                  (when (and (= code (code resize))
                             (= state (state resize)))
                    (warp-pointer child (drawable-width child) 
                                  (drawable-height child)))
                  (let ((lst (multiple-value-list (query-pointer *root*))))
                    (setf last-x (sixth lst)
                          last-y (seventh lst)))))
               (:motion-notify
                (event-window root-x root-y)
                (cond ((= last-button (cdr move))
                       (let ((delta-x (- root-x last-x))
                             (delta-y (- root-y last-y)))
                         (incf (drawable-x event-window) delta-x)
                         (incf (drawable-y event-window) delta-y)
                         (incf last-x delta-x)
                         (incf last-y delta-y)))
                      ((= last-button (cdr resize))
                       (let ((new-w (max 1 (- root-x (drawable-x event-window))))
                             (new-h (max 1 (- root-y (drawable-y event-window)))))
                         (setf (drawable-width event-window) new-w
                               (drawable-height event-window) new-h)))))
               (:button-release () (ungrab-pointer *display*))
               (:map-notify
                (window override-redirect-p)
                (unless override-redirect-p
                  (pushnew window *windows* :test #'window-equal)
                  (focus window)))
               (:destroy-notify
                (window)
                (setf *windows* (remove window *windows* :test #'window-equal))
                (when *windows*
                    (focus (first *windows*))))))
      (ungrab-button *root* (code move) :modifiers (state move))
      (ungrab-button *root* (code resize) :modifiers (state resize))
      (ungrab-key *root* (code prefix) :modifiers (state prefix))
      (close-display *display*))))

(main)
