#!/usr/local/bin/sbcl --script
;;; Used to be the most simple window manager on earth. It started as
;;; a fork of the lisp version of tinywm.

(load "prog/lisp/quicklisp/setup.lisp")

;;; Load swank and make a server
(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)

;;; Load CLX and make a package
(ql:quickload :clx)
(defpackage :most.simple.wm
  (:use :common-lisp :xlib :sb-ext))
(in-package :most.simple.wm)

(defvar *display* (open-default-display))
(defvar *root* (screen-root (display-default-screen *display*)))
(defparameter *handlers* (make-list (length xlib::*event-key-vector*)
                                    :initial-element #'(lambda (&rest slots))))
(defparameter *windows* nil "List of managed and mapped windows.")
(defparameter *last* nil "Last focused window.")
(defparameter *curr* nil "Current focused window.")
(defvar *dim* nil "Dimension of current window before fullscreen.")

(defmacro defhandler (event keys &body body)
  (let ((fn-name (gensym))
        (event-slots (gensym)))
    `(labels ((,fn-name (&rest ,event-slots &key ,@keys &allow-other-keys)
                (declare (ignore ,event-slots))
                ,@body))
       (setf (elt *handlers* (position ,event xlib::*event-key-vector*)) #',fn-name))))

(defun mods (l) (butlast l))
(defun kchar (l) (car (last l)))
(defun compile-shortcut (&rest l)
  "Compile a shortcut into a (state . code) form. For
example: (compile-shortcut :control #\t) -> (4 . 44). Works also
for mouse button."
  (let ((k (kchar l))
        (state (apply #'make-state-mask (mods l))))
    (if (characterp k)
        (let ((c (keysym->keycodes *display* (car (character->keysyms k)))))
          (cons state c))
        (cons state k))))
(defun state (l) (car l))
(defun code (l) (cdr l))
(defun sc= (sc state code) (and (= (code sc) code) (= (state sc) state)))

(defparameter *shortcuts*
  (list (cons (compile-shortcut :shift #\q) 'quit))
  "Shortcuts alist initialized with the quit command.")

(defmacro defshortcut (key &body body)
  "Define a new shortcut in *shortcuts* alist. The key in this alist
  is in (state . code) form and the associated value is a lambda
  without argument."
  (let ((sc (gensym)))
    `(let ((,sc (compile-shortcut ,@key)))
       (setf *shortcuts* (remove ,sc *shortcuts* :test #'equal :key #'car))
       (push (cons ,sc #'(lambda () ,@body)) *shortcuts*))))

(defun move (window x y w h)
  "Move a window."
  (with-state (window)
    (setf (drawable-x window) x (drawable-y window) y
          (drawable-width window) w (drawable-height window) h)))

(defgeneric focus (window))

(defmethod focus :before (window)
  (unless (null window)
    (unless (win= window *curr*)
      (setf *last* *curr*)
      (when *dim*
        (apply #'move *curr* *dim*)
        (setf *dim* nil)))
    (setf *curr* window)))

(defun focus-1 (window)
  (when (eql (window-map-state window) :viewable)
    (setf (window-priority window) :above)))

(defmethod focus ((window window))
  (focus-1 window)
  (set-input-focus *display* window :pointer-root))

(defmethod focus ((window list))
  (unless (null window)
    (dolist (w window) (focus-1 w))
    (set-input-focus *display* :pointer-root :pointer-root)
    (let ((focus (first window)))
      (setf (window-priority focus) :above))))

(defmethod win= ((a window) (b window)) (window-equal a b))
(defmethod win= ((a list) (b window)) (loop for w in a thereis (window-equal w b)))
(defmethod win= ((a window) (b list)) (loop for w in b thereis (window-equal w a)))
(defmethod win= ((a list) (b list)) (loop for w in a thereis (win= w b)))

(defun next (&optional (way #'1+) (window *curr*))
  (when *windows*
    (let* ((nw (or (position window *windows* :test #'win=) 0))
           (n (length *windows*))
           (next (mod (funcall way nw) n)))
      (nth next *windows*))))

(defmethod xclass ((w window)) (get-wm-class w))
(defmethod xclass ((w list)) (get-wm-class (first w)))

(defparameter *groupers* (list
                          #'(lambda (w)
                              (multiple-value-bind (name class) (xclass w)
                                (string= class "Idl")))
                          #'(lambda (w)
                              (multiple-value-bind (name class) (xclass w)
                                (or (string= class "Startup")
                                    (string= class "DX")))))
  "List of predicates against which windows are grouped")

(defun plus (window)
  "Add window to the list of managed windows. Take care of grouping
and don't add window already in the list."
  (let ((grouper (find-if #'(lambda (f) (funcall f window)) *groupers*)))
    (labels ((radd (item list pred test)
               (cond ((and (null list) (functionp pred))
                      (list (list item)))
                     ((null list) (list item))
                     (t (let ((hd (car list))
                              (tl (cdr list)))
                          (cond ((funcall test hd item) list)
                                ((and (listp hd)
                                      (functionp pred)
                                      (funcall pred (car hd)))
                                 (cons (cons item hd) tl))
                                (t (cons hd (radd item tl pred test)))))))))
      (setf *windows* (radd window *windows* grouper #'win=))
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

(defgeneric clean (place window)
  (:documentation "Returns `place' (a window or a list of window) where
  `window' has been removed."))
(defmethod clean ((place window) (window window)) (unless (win= place window) place))
(defmethod clean ((place list) (window window)) (rrem window place :test #'window-equal))

(defun minus (window)
  "House keeping when window is unmapped. Returns the window to be
focused."
  (when (member window *windows* :test #'win=)
    (setf *windows* (clean *windows* window))
    (setf *curr* (clean *curr* window))
    (setf *last* (clean *last* window))
    (when (null *last*) (setf *last* (next #'1+ *curr*)))
    (when (null *curr*)
      (setf *curr* *last*)
      *curr*)))

(defun fullscreen ()
  "Toggle fullscreen state of the current window."
  (unless (listp *curr*)
    (cond (*dim*
           (apply #'move *curr* *dim*)
           (setf *dim* nil))
          (t (let* ((screen (display-default-screen *display*))
                    (sw (screen-width screen))
                    (sh (screen-height screen)))
               (setf *dim* (list (drawable-x *curr*) (drawable-y *curr*)
                                 (drawable-width *curr*) (drawable-height *curr*)))
               (move *curr* 0 0 sw sh))))))

(defmacro defror (command)
  "Define a raise or run command."
  (let ((win (gensym))
        (cmdstr (gensym)))
    `(defun ,command ()
       (let* ((,cmdstr (string-downcase (string ',command)))
              (,win (find-if #'(lambda (w)
                                 (string-equal
                                  ,cmdstr
                                  (second (multiple-value-list (xclass w)))))
                             *windows*)))
         (if ,win
             (focus ,win)
             (run-program ,cmdstr nil :wait nil :search t))))))
(defror emacs)
(defror firefox)

(defun send-message (window type &rest data)
  (send-event window :client-message nil :window window
                     :type type :format 32 :data data))

;;; Apps in path
(defun split-string (string &optional (character #\Space))
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
        as j = (position character string :start i)
        collect (subseq string i j)
        while j))

(defun execp (pathname)
  "Return T if the pathname describes an executable file."
  (let ((filename (namestring pathname)))
    (and (or (pathname-name pathname)
             (pathname-type pathname))
         (sb-unix:unix-access filename sb-unix:x_ok))))

(defparameter *apps*
  (let ((paths (split-string (posix-getenv "PATH") #\:)))
    (loop for path in paths
          append (loop for file in (directory (merge-pathnames
                                               (make-pathname :name :wild :type :wild)
                                               (concatenate 'string path "/")))
                       when (execp file) collect (file-namestring file)))))

;;; Modifier keypress avoidance code
(defvar *mods-code* (multiple-value-call #'append (modifier-mapping *display*)))

(defun is-modifier (keycode)
  "Return non-nil if keycode is a modifier"
  (find keycode *mods-code* :test #'eql))

;;; App launcher
(defparameter *abort* (compile-shortcut :control #\g))
(defparameter *this* (compile-shortcut #\Return)
  "Validate THIS app even if it is a prefix of more than one.")

(defun single (list) (and (consp list) (null (cdr list))))

(defun one-char ()
  "Get one char from the user. The keyboard should be grabbed before call."
  (event-case (*display*)
    (:key-press (code state)
                (cond ((is-modifier code) (one-char))
                      ((sc= *abort* state code) 'abort)
                      ((sc= *this* state code) 'this)
                      (t (keycode->character *display* code state))))))

(defun pre-matcher (sofar)
  "Build a matcher to eliminate (with remove-if-not) strings that
don't have the same character at the same place as `sofar'."
  (let* ((n (1- (length sofar)))
         (c (elt sofar n)))
    #'(lambda (str) (and (< n (length str))
                         (char= c (elt str n))))))

(defun in-matcher (sofar)
  "Build a matcher to eliminate (with remove-if-not) strings that
don't contain `sofar'."
  #'(lambda (str) (search sofar str :test #'char=)))

(defun recdo-1 (sofar list fn matcher key)
  (cond ((null list))
        ((single list)
         (funcall fn (car list)))
        (t (let ((char (one-char)))
             (etypecase char
               (character
                (vector-push-extend char sofar)
                (recdo-1 sofar (remove-if-not (funcall matcher sofar) list :key key)
                         fn matcher key))
               (symbol
                (cond ((eql char 'abort))
                      ((eql char 'this) (funcall fn (car list))))))))))

(defun recdo (list fn &key (matcher #'pre-matcher) (key #'identity))
  (let ((sofar (make-array 0 :element-type 'character :fill-pointer t :adjustable t)))
    (recdo-1 sofar list fn matcher key)))

(defun app ()
  (grab-keyboard *root*)
  (unwind-protect
       (recdo *apps* #'(lambda (app) (run-program app nil :wait nil :search t)))
    (ungrab-keyboard *display*)))

(defun finder ()
  (grab-keyboard *root*)
  (unwind-protect (recdo *windows* #'focus :key #'xclass :matcher #'in-matcher)
    (ungrab-keyboard *display*)))

;;; Mouse shorcuts
(defparameter *move* (compile-shortcut :mod-1 1) "Mouse button to move a window")
(defparameter *resize* (compile-shortcut :mod-1 3) "Mouse button to resize a window")
(defparameter *close* (compile-shortcut :control :mod-1 2)
  "Mouse button to close a window")

;;; Key shortcuts
(defparameter *prefix* (compile-shortcut :control #\t) "Prefix for shortcuts")
(defshortcut (#\c) (run-program "xterm" nil :wait nil :search t))
(defshortcut (#\e) (emacs))
(defshortcut (#\w) (firefox))
(defshortcut (:control #\l) (run-program "xlock" nil :wait nil :search t))
(defshortcut (:mod-1 #\e) (run-program "envi" nil :wait nil :search t))
(defshortcut (#\n) (focus (next)))
(defshortcut (#\p) (focus (next #'1-)))
(defshortcut (:control #\n) (focus (next)))
(defshortcut (:control #\p) (focus (next #'1-)))
(defshortcut (:control #\t) (focus *last*))
(defshortcut (#\a) (app))
(defshortcut (#\f) (fullscreen))

(defun send-prefix ()
  (let ((focus (input-focus *display*)))
    (when (win= focus *curr*)
      (send-event focus :key-press (make-event-mask :key-press)
                        :window focus
                        :code (code *prefix*)
                        :state (state *prefix*)))))
(defshortcut (#\t) (send-prefix))

(defvar last-button nil)
(defvar last-x nil)
(defvar last-y nil)
(defvar last-motion nil)
(defvar waiting-shortcut nil)

(defhandler :key-press (code state)
  (unless (is-modifier code)
    (cond (waiting-shortcut
           (let ((fn (cdr (assoc-if #'(lambda (sc) (sc= sc state code)) *shortcuts*))))
             (when (functionp fn) (funcall fn))
             (ungrab-keyboard *display*)
             (setf waiting-shortcut nil)
             fn))
          ((sc= *prefix* state code)
           (grab-keyboard *root*)
           (setf waiting-shortcut t)))))

(defhandler :button-press (code state child)
  (when (and child (eql (window-override-redirect child) :off))
    (cond ((sc= *close* state code)
           (send-message child :WM_PROTOCOLS (intern-atom *display* :WM_DELETE_WINDOW)))
          (t
           (setf last-button code)
           (focus (find child *windows* :test #'win=))
           (grab-pointer child '(:pointer-motion :button-release))
           (when (sc= *resize* state code)
             (warp-pointer child (drawable-width child) (drawable-height child)))
           (let ((lst (multiple-value-list (query-pointer *root*))))
             (setf last-x (sixth lst)
                   last-y (seventh lst)))))))

(defhandler :motion-notify (event-window root-x root-y time)
  (when (or (null last-motion) (> (- time last-motion) (/ 1000 60)))
    (cond ((= last-button (code *move*))
           (let ((delta-x (- root-x last-x))
                 (delta-y (- root-y last-y)))
             (incf (drawable-x event-window) delta-x)
             (incf (drawable-y event-window) delta-y)
             (incf last-x delta-x)
             (incf last-y delta-y)))
          ((= last-button (code *resize*))
           (let ((new-w (max 1 (- root-x (drawable-x event-window))))
                 (new-h (max 1 (- root-y (drawable-y event-window)))))
             (setf (drawable-width event-window) new-w
                   (drawable-height event-window) new-h))))
    (setf last-motion time)))

(defhandler :button-release () (ungrab-pointer *display*))

(defhandler :map-notify (window override-redirect-p)
  (unless override-redirect-p
    (focus (plus window))))

(defhandler :unmap-notify (window) (focus (minus window)))

;;; Main
(defun main ()
  ;; Grab prefix and mouse buttons on root
  (grab-key *root* (code *prefix*) :modifiers (state *prefix*))
  (grab-button *root* (code *move*) '(:button-press) :modifiers (state *move*))
  (grab-button *root* (code *resize*) '(:button-press) :modifiers (state *resize*))
  (grab-button *root* (code *close*) '(:button-press) :modifiers (state *close*))

  ;; Populate list of windows
  (dolist (w (query-tree *root*))
    (when (and (eql (window-map-state w) :viewable)
               (eql (window-override-redirect w) :off))
      (plus w)))

  (intern-atom *display* :_motif_wm_hints)
  (setf (window-event-mask *root*) '(:substructure-notify))

  (unwind-protect
       (do () ((eql (process-event *display* :handler *handlers* :discard-p t) 'quit)))
    (dolist (b (list *move* *resize* *close*))
      (ungrab-button *root* (code b) :modifiers (state b)))
    (ungrab-key *root* (code *prefix*) :modifiers (state *prefix*))
    (close-display *display*)))

(main)
