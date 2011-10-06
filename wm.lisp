#!/usr/local/bin/sbcl --script
;;; Used to be the most simple window manager on earth. It is a fork
;;; from the lisp version of tinywm.

;;; Load swank and make a server
(require 'asdf)
(asdf:load-system :swank)
(swank:create-server :port 4005 :dont-close t)

;;; Load CLX and make a package
(asdf:load-system :clx)
(defpackage :most.simple.wm
  (:use :common-lisp :xlib :sb-ext))
(in-package :most.simple.wm)

(defvar *display* (open-default-display))
(defvar *root* (screen-root (display-default-screen *display*)))
(defparameter *windows* nil "List of managed windows")
(defparameter *last* nil "Last focused window")

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

(defmacro defshortcut (key &body body)
  "Define a new shortcut in *shortcuts* alist. The key in this alist
  is in (state . code) form and the associated value is a lambda
  without argument."
  (let ((sc (gensym)))
    `(let ((,sc (compile-shortcut ',key)))
       (pushnew (cons ,sc #'(lambda () ,@body)) *shortcuts* :test #'equal :key #'car))))

(defun have-focus ()
  "Return a window (or list of) where the focus is."
  (let ((w (input-focus *display*)))
    (find w *windows* :test #'win=)))

(defun focus-1 (window)
  (when (eql (window-map-state window) :viewable)
    (setf (window-priority window) :above)
    (set-input-focus *display* window :pointer-root)
    (display-finish-output *display*)))

(defun focus (window)
  (setf *last* (have-focus))
  (if (listp window)
      (dolist (w window) (focus-1 w))
      (focus-1 window)))

(defun win= (a b)
  (cond ((and (window-p a) (window-p b))
         (window-equal a b))
        ((and (window-p a) (listp b))
         (loop for w in b thereis (window-equal w a)))
        ((and (listp a) (window-p b))
         (win= b a))
        ((and (listp a) (listp b))
         (loop for w in a thereis (win= w b)))))

(defun next (&optional (way #'1+))
  (when *windows*
    (let* ((cw (input-focus *display*))
           (ncw (or (position-if #'(lambda (w) (win= w cw)) *windows*) 0))
           (n (length *windows*))
           (next (mod (funcall way ncw) n)))
      (focus (nth next *windows*)))))

(defun flast ()
  (if *last*
      (focus *last*)
      (setf *last* (have-focus))))

(defparameter *groupers* (list 
                          #'(lambda (w) 
                              (multiple-value-bind (name class) (get-wm-class w) 
                                (string= class "Idl"))))
  "List of predicates against which windows are grouped")

(defun add-window (window)
  "Add window to the list of managed windows. Take care of grouping."
  (let ((grouper (find-if #'(lambda (f) (funcall f window)) *groupers*)))
    (labels ((radd (item list pred)
               (cond ((and (null list) (functionp pred))
                      (list (list item)))
                     ((null list) (list item))
                     (t (let ((hd (car list))
                              (tl (cdr list)))
                          (cond ((and (listp hd)
                                      (functionp pred)
                                      (funcall pred (car hd)))
                                 (cons (cons item hd) tl))
                                (t (cons hd (radd item tl pred)))))))))
      (setf *windows* (radd window *windows* grouper))
      (find window *windows* :test #'win=))))
  
(defun rrem (item list &key (test #'eql))
  "Recursive remove."
  (unless (null list)
    (let* ((hd (car list))
           (tl (cdr list))
           (rtl (rrem item tl :test test)))
      (cond ((listp hd)
             (let ((rhd (rrem item hd :test test)))
               (if rhd 
                   (cons rhd rtl)
                   rtl)))
            ((funcall test item hd) rtl)
            (t (cons hd rtl))))))

(defun emacs ()
  (let ((emacs (find-if #'(lambda (w) (string= "emacs" (get-wm-class w))) *windows*)))
    (if emacs
        (focus emacs)
        (run-program "emacs" nil :wait nil :search t))))

;;; User shortcuts
(defparameter *prefix* '(:control #\t) "Prefix for shortcuts")
(defparameter *move* '(:mod-1 1) "Mouse button to move a window")
(defparameter *resize* '(:mod-1 3) "Mouse button to resize a window")
(defshortcut (#\c) (run-program "xterm" nil :wait nil :search t))
(defshortcut (#\e) (emacs))
(defshortcut (:mod-1 #\e) (run-program "envi" nil :wait nil :search t))
(defshortcut (#\w) (run-program "xxxterm" nil :wait nil :search t))
(defshortcut (:control #\l) (run-program "xlock" nil :wait nil :search t))
(defshortcut (#\n) (next))
(defshortcut (#\p) (next #'1-))
(defshortcut (:control #\t) (flast))

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

    ;; Populate list of windows
    (loop for w in (query-tree *root*) do
         (when (and (eql (window-map-state w) :viewable) 
                    (eql (window-override-redirect w) :off))
           (add-window w)))

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
                  (focus (add-window window))))
               (:destroy-notify
                (window)
                (setf *windows* (rrem window *windows* :test #'window-equal))
                (when (win= window *last*) (setf *last* nil)))))
      (ungrab-button *root* (code move) :modifiers (state move))
      (ungrab-button *root* (code resize) :modifiers (state resize))
      (ungrab-key *root* (code prefix) :modifiers (state prefix))
      (close-display *display*))))

(main)
