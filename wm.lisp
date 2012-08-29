;;; Used to be the most simple window manager on earth. It started as
;;; a fork of the lisp version of tinywm.

(load (merge-pathnames "prog/lisp/quicklisp/setup.lisp" (user-homedir-pathname)))

;;; Load swank and make a server (not for clisp because i can't make
;;; create-server return).
#-clisp (ql:quickload :swank)
#-clisp (swank:create-server :port 4005 :dont-close t)

;;; Load CLX and make a package
(ql:quickload :clx)
(defpackage :most.simple.wm
  (:use :common-lisp :xlib))
(in-package :most.simple.wm)

(defvar *display* (open-default-display))
(defvar *root* (screen-root (display-default-screen *display*)))
(defvar *windows* nil "List of managed windows.")
(defvar *last* nil "Last focused window.")
(defvar *curr* nil "Current focused window.")
(defvar *mosaic-p* nil)
(defparameter *handlers* (make-list (length xlib::*event-key-vector*)
                                    :initial-element #'(lambda (&rest slots))))

(defmacro defhandler (event keys &body body)
  (let ((fn-name (gensym (symbol-name event)))
        (event-slots (gensym)))
    `(labels ((,fn-name (&rest ,event-slots &key ,@keys &allow-other-keys)
                (declare (ignore ,event-slots))
                ,@body))
       (setf (elt *handlers* (position ,event xlib::*event-key-vector*)) #',fn-name))))

(defun xclass (window) (multiple-value-bind (name class) (get-wm-class window) class))

(defparameter *groupers* (list
                          #'(lambda (w) (search "Gimp" (xclass w) :test #'char-equal))
                          #'(lambda (w) (string= (xclass w) "Idl"))
                          #'(lambda (w)
                              (let ((class (xclass w)))
                                (or (string= class "Startup")
                                    (string= class "DX")
                                    (string= class "GAR")))))
  "List of predicates against which windows are grouped")

(defun mods (l) (butlast l))
(defun kchar (l) (car (last l)))
(defun compile-shortcut (&rest l)
  "Compile a shortcut into a (state . code) form. For
example: (compile-shortcut :control #\t) -> (4 . 44). Works also
for mouse button."
  (let ((k (kchar l))
        (state (apply #'make-state-mask (mods l))))
    (if (characterp k)
        (let ((c (car (last (multiple-value-list
                             (keysym->keycodes *display* (car (character->keysyms k))))))))
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
       (push (cons ,sc #'(lambda () ,@body)) *shortcuts*))))

(defun move (window x y w h)
  "Move a window."
  (with-state (window)
    (setf (drawable-x window) x (drawable-y window) y
          (drawable-width window) w (drawable-height window) h)))

(defun memove (window &optional x y w h)
  "Memory Move a window (i.e. toggle between current position and the
given one). Without a given new position reset position or does
nothing."
  (let ((dim (getf (window-plist window) 'original-dimension)))
    (cond (dim
           (apply #'move window dim)
           (remf (window-plist window) 'original-dimension))
          (t (when (and x y w h)
               (setf (getf (window-plist window) 'original-dimension)
                     (list (drawable-x window) (drawable-y window)
                           (drawable-width window) (drawable-height window)))
               (move window x y w h))))))

(defun win= (a b)
  (and (typep a 'window) (typep b 'window) (window-equal a b)))

(defun skip-window (c)
  (let ((restart (find-restart 'skip-window)))
    (format *error-output* "~&~A" c)
    (when restart (invoke-restart restart))))

(defun grouper (window)
  "Get the grouper function of a window if there is one."
  (and window (restart-case (find-if #'(lambda (f) (funcall f window)) *groupers*)
                (skip-window () nil))))

(defun transient-for-p (transient parent)
  (let ((pid (window-id parent)))
    (loop for id in (get-property transient :WM_TRANSIENT_FOR) thereis (= id pid))))

(defun focus (window)
  (unless (null window)
    (unless (or (win= window *curr*) (null *curr*))
      (unless (or *mosaic-p* (transient-for-p window *curr*))
        (memove *curr*))
      (setf *last* *curr*))
    (let* ((grouper (grouper window))
           (group (when (functionp grouper)
                    (loop for w in *windows*
                          when (funcall grouper w) collect w))))
      (cond (group
             (dolist (w group) (setf (window-priority w) :above))
             (set-input-focus *display* :pointer-root :pointer-root))
            (t (set-input-focus *display* window :pointer-root)))
      (setf (window-priority window) :above
            *curr* window))))

(defun next (&optional (way #'1+))
  (let* ((grouper (grouper *curr*))
         (windows (if (functionp grouper)
                      (remove-if #'(lambda (w) (and (funcall grouper w)
                                                    (not (win= w *curr*)))) *windows*)
                      *windows*))
         (n (length windows))
         (nw (or (position *curr* windows :test #'win=) 0)))
    (unless (zerop n)
      (nth (mod (funcall way nw) n) windows))))

(defun plus (window)
  "Add window to the list of managed windows."
  (pushnew window *windows* :test #'win=)
  window)

(defun minus (window)
  "House keeping when window is unmapped. Returns the window to be
focused."
  (setf *windows* (remove window *windows* :test #'win=))
  (unless (null *windows*)
    (when (win= *curr* window) (setf *curr* *last*))
    (when (win= *last* window) (setf *last* (next)))
    (when (win= *curr* *last*) (setf *last* (next)))
    *curr*))

(defun fullscreen ()
  "Toggle fullscreen the current window."
  (unless *mosaic-p*
    (let* ((screen (display-default-screen *display*))
           (sw (screen-width screen))
           (sh (screen-height screen)))
      (memove *curr* 0 0 sw sh))))

(defun center ()
  "Toggle center the current window."
  (unless *mosaic-p*
    (let* ((screen (display-default-screen *display*))
           (sw (screen-width screen))
           (sh (screen-height screen))
           (w (drawable-width *curr*))
           (h (drawable-height *curr*)))
      (memove *curr* (- (truncate sw 2) (truncate w 2))
              (- (truncate sh 2) (truncate h 2)) w h))))

(defun mosaic ()
  "Toggle mosaic of windows."
  (let* ((screen (display-default-screen *display*))
         (w (screen-width screen))
         (h (screen-height screen))
         (n (length *windows*))
         (k (ceiling (sqrt n)))
         (dw (truncate w k))
         (dh (truncate h k)))
    (loop for i below n
          for linep = (zerop (mod i k))
          for x = 0 then (if linep 0 (+ x dw))
          for y = 0 then (if linep (+ y dh) y)
          for win = (nth i *windows*)
          do (unless *mosaic-p* (memove win))
             (memove win x y dw dh)))
  (setf *mosaic-p* (not *mosaic-p*)))

(defun run (command)
  #+clisp (ext:run-program command :wait nil)
  #+sbcl (sb-ext:run-program command nil :wait nil :search t))

(defmacro defror (command)
  "Define a raise or run command."
  (let ((win (gensym))
        (cmdstr (gensym)))
    `(defun ,command ()
       (let* ((,cmdstr (string-downcase (string ',command)))
              (,win (find-if #'(lambda (w) (string-equal ,cmdstr (xclass w)))
                             *windows*)))
         (if ,win
             (focus ,win)
             (run ,cmdstr))))))
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
  (declare (ignorable pathname))
  #+sbcl
  (let ((filename (namestring pathname)))
    (and (or (pathname-name pathname)
             (pathname-type pathname))
         (sb-unix:unix-access filename sb-unix:x_ok)))
  #+clisp
  (logand (posix:convert-mode (posix:file-stat-mode (posix:file-stat pathname)))
          (posix:convert-mode '(:xusr :xgrp :xoth)))
  #-(or sbcl clisp) t)

(defun getenv (var) #+sbcl (sb-ext:posix-getenv var) #+clisp (ext:getenv var))

(defparameter *apps*
  (let ((paths (split-string (getenv "PATH") #\:)))
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
  #'(lambda (str) (search sofar str :test #'char-equal)))

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
       (recdo *apps* #'(lambda (app) (run app)))
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
(defshortcut (#\c) (run "xterm"))
(defshortcut (#\e) (emacs))
(defshortcut (#\w) (firefox))
(defshortcut (:control #\l) (run "xlock"))
(defshortcut (:mod-1 #\e) (run "envi"))
(defshortcut (#\n) (focus (next)))
(defshortcut (:control #\n) (focus (next)))
(defshortcut (#\p) (focus (next #'1-)))
(defshortcut (:control #\p) (focus (next #'1-)))
(defshortcut (:control #\t) (focus *last*))
(defshortcut (#\a) (app))
(defshortcut (#\') (finder))
(defshortcut (#\f) (fullscreen))
(defshortcut (#\.) (center))
(defshortcut (:shift #\.) (center))
(defshortcut (#\m) (mosaic))

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
  (when (and child (eql (window-override-redirect child) :off)
             (null (let ((w (find child *windows* :test #'win=)))
                     (when w (getf (window-plist w) 'original-dimension)))))
    (cond ((sc= *close* state code)
           (send-message child :WM_PROTOCOLS (intern-atom *display* :WM_DELETE_WINDOW)))
          (t
           (setf last-button code)
           (focus child)
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

(defun evloop ()
  (do () ((eql (handler-bind ((window-error #'skip-window))
                 (process-event *display* :handler *handlers* :discard-p t)) 'quit))))

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

  (intern-atom *display* :_MOTIF_WM_HINTS)
  (setf (window-event-mask *root*) '(:substructure-notify))

  (setf *last* (setf *curr* (first *windows*)))

  (unwind-protect (evloop)
    (dolist (b (list *move* *resize* *close*))
      (ungrab-button *root* (code b) :modifiers (state b)))
    (ungrab-key *root* (code *prefix*) :modifiers (state *prefix*))
    (close-display *display*)))

(main)
