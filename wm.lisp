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
(defvar *screen* (display-default-screen *display*))
(defvar *root* (screen-root *screen*))
(defvar *windows* nil "List of managed windows.")
(defvar *last* nil "Last focused window.")
(defvar *curr* nil "Current focused window.")
(defvar *rc* (merge-pathnames ".wm.lisp" (user-homedir-pathname)) "User config file.")
(defvar *shortcuts* nil "Shortcuts alist.")
(defvar *handlers* (make-list (length xlib::*event-key-vector*)
                              :initial-element #'(lambda (&rest slots))))

(defun argc ()
  "Return the command line argument count."
  #+sbcl (1- (length sb-ext:*posix-argv*))
  #+clisp (length ext:*args*))

(defun args (n)
  "Return the n-th command line argument. 0 is the first argument."
  (let ((args #+sbcl (rest sb-ext:*posix-argv*)
              #+clisp ext:*args*))
    (read-from-string (elt args n))))

;;; Set screen geometry if provided on command line
(cond ((= (argc) 1)
       (setf (screen-height *screen*) (args 0)))
      ((= (argc) 2)
       (setf (screen-height *screen*) (args 0)
             (screen-width *screen*) (args 1))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))

(defmacro defhandler (event keys &body body)
  (with-gensyms (fn-name event-slots)
    `(labels ((,fn-name (&rest ,event-slots &key ,@keys &allow-other-keys)
                (declare (ignore ,event-slots))
                ,@body))
       (setf (elt *handlers* (position ,event xlib::*event-key-vector*)) #',fn-name))))

(defun xclass (window) (multiple-value-bind (name class)
                           (restart-case (get-wm-class window)
                             (window-error ()
                               (format t "~&No class on ~a~%" window)
                               (values "" "I'm dead")))
                         class))

(defparameter *groupers* (list
                          #'(lambda (w) (search "Gimp" (xclass w) :test #'char-equal)))
  "List of predicates against which windows are grouped")

(defstruct (shortcut (:conc-name)) (states nil) (code 0))

(defmethod print-object ((sc shortcut) stream)
  (let* ((state (make-state-keys (first (states sc))))
         (code (code sc))
         (k (if (< code 4) code (keycode->character *display* code 0))))
    (when (member :shift state)
      (setf state (remove :shift state)
            k (char-upcase k)))
    (format stream "~a~a" (concatenate 'string
                                       (when (member :control state) "C-")
                                       (when (member :mod-1 state) "M-")) k)))

(defun compile-shortcut (&rest l)
  "Compile a shortcut. Usage: (compile-shortcut :control #\t) for a
keyboard shortcut or (compile-shortcut :mod-1 1) for a mouse
shortcut. Takes care of CapsLock and NumLock combination."
  (let* ((k (car (last l)))
         (mods (butlast l))
         (states (list (apply #'make-state-mask mods)
                       (apply #'make-state-mask (cons :lock mods))
                       (apply #'make-state-mask (cons :mod-2 mods))
                       (apply #'make-state-mask (append '(:lock :mod-2) mods)))))
    (if (characterp k)
        (let ((code (car (last (multiple-value-list
                                (keysym->keycodes *display* (car (character->keysyms k))))))))
          (make-shortcut :states states :code code))
        (make-shortcut :states states :code k))))

(defun sc= (sc state code) (and (= (code sc) code) (member state (states sc))))
(defun sc-equal (sc1 sc2) (equalp sc1 sc2))

(defmacro defshortcut (key &body body)
  "Define a new shortcut in *shortcuts* alist. The key in this alist
  is a shortcut structure and the associated value is a lambda without
  argument."
  (with-gensyms (sc asc fn)
    `(let* ((,sc (compile-shortcut ,@key))
            (,asc (assoc ,sc *shortcuts* :test #'sc-equal)))
       (labels ((,fn () ,@body))
         (if ,asc
             (rplacd ,asc #',fn)
             (push (cons ,sc #',fn) *shortcuts*))))))

(defun correct-size (window &optional x y width height dx dy dw dh)
  "Correct a window's dimensions with its sizehints."
  (let ((hints (wm-normal-hints window)))
    (when hints (let* ((min-w (or (wm-size-hints-min-width hints) 1))
                       (min-h (or (wm-size-hints-min-height hints) 1))
                       (inc-w (or (wm-size-hints-width-inc hints) 1))
                       (inc-h (or (wm-size-hints-height-inc hints) 1))
                       (base-w (or (wm-size-hints-base-width hints) 0))
                       (base-h (or (wm-size-hints-base-height hints) 0)))
                  (when x (setf x (* inc-w (truncate x inc-w))))
                  (when y (setf y (* inc-h (truncate y inc-h))))
                  (when width
                    (decf width base-w)
                    (setf width (max min-w (+ (* inc-w (truncate width inc-w)) base-w))))
                  (when height
                    (decf height base-h)
                    (setf height (max min-h (+ (* inc-h (truncate height inc-h)) base-h))))
                  (when dx (setf dx (* inc-w (truncate dx inc-w))))
                  (when dy (setf dy (* inc-h (truncate dy inc-h))))
                  (when dh (setf dh (* inc-h (truncate dh inc-h))))
                  (when dw (setf dw (* inc-w (truncate dw inc-w))))))
    (values x y width height dx dy dw dh)))

(defun move (window &key x y width height dx dy dw dh)
  "Move a window with respect to sizehints. Returns effective size
values."
  (multiple-value-bind (x y width height dx dy dw dh)
      (correct-size window x y width height dx dy dw dh)
    ;; if not provided get current geometry
    (unless x (setf x (drawable-x window)))
    (unless y (setf y (drawable-y window)))
    (unless width (setf width (drawable-width window)))
    (unless height (setf height (drawable-height window)))

    ;; dx, dy, dw and dh are rewritten in absolute form
    (when dx (incf x dx))
    (when dy (incf y dy))
    (when dw (let ((new-w (+ width dw)))
	       (cond ((minusp new-w)
		      (incf x new-w)
		      (setf width (abs new-w)))
		     (t (setf width new-w)))))
    (when dh (let ((new-h (+ height dh)))
	       (cond ((minusp new-h)
		      (incf y new-h)
		      (setf height (abs new-h)))
		     (t (setf height new-h)))))
    (with-state (window)
      (setf (drawable-x window) x
    	    (drawable-y window) y
    	    (drawable-width window) width
    	    (drawable-height window) height))
    (values x y width height dx dy dw dh)))

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
                     (list :x (drawable-x window) :y (drawable-y window)
                           :width (drawable-width window) :height (drawable-height window)))
               (move window :x x :y y :width w :height h))))))

(defun win= (a b)
  (and (typep a 'window) (typep b 'window) (window-equal a b)))

(defun grouper (window)
  "Get the grouper function of a window if there is one."
  (and window (restart-case (find-if #'(lambda (f) (funcall f window)) *groupers*)
		(window-error () nil))))

(defun transient-for-p (transient parent)
  (let ((pid (window-id parent)))
    (unless (= (window-id transient) pid)
      (loop for id in (get-property transient :WM_TRANSIENT_FOR)
	 thereis (= id pid)))))

(defun focus (window)
  (unless (null window)
    (unless (or (null *curr*)
                (transient-for-p window *curr*))
      (memove *curr*))
    (let* ((grouper (grouper window))
           (group (when (functionp grouper)
                    (sort (loop for w in *windows*
                                when (funcall grouper w) collect w) #'< :key #'window-id))))
      (cond (group
             (unless (member *curr* group :test #'win=)
               (setf *last* *curr*
                     *curr* (first group)))
             (dolist (w group) (setf (window-priority w) :above))
             (set-input-focus *display* :pointer-root :pointer-root))
            (t
             (unless (win= *curr* window)
               (setf *last* *curr*
                     *curr* window))
             (set-input-focus *display* window :pointer-root)))
      (setf (window-priority window) :above))))

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

(defun insert-by-group (window lst)
  "Insert window in the given window list respecting group affinity."
  (cond ((null lst) (list window))
        ((member window lst :test #'win=) lst)
        (t (let ((grouper (grouper window))
                 (w (car lst)))
             (if (and (functionp grouper)
                      (funcall grouper w))
                 (cons window lst)
                 (cons w (insert-by-group window (cdr lst))))))))

(defun plus (window)
  "Add window to the list of managed windows."
  (setf *windows* (insert-by-group window *windows*))
  window)

(defun minus (window)
  "House keeping when window is unmapped. Returns the window to be
focused."
  (setf *windows* (remove window *windows* :test #'win=))
  (cond ((null *windows*) (setf *curr* nil *last* nil))
        (:otherwise
         (when (win= *curr* window) (setf *curr* *last*))
         (when (win= *last* window) (setf *last* (next)))
         (when (win= *curr* *last*) (setf *last* (next #'1-)))
         *curr*)))

(defun managed-p (window)
  (member window *windows* :test #'win=))

(defun fullscreen (&key permanent-p)
  "Toggle fullscreen the current window."
  (let ((sw (screen-width *screen*))
        (sh (screen-height *screen*)))
    (if permanent-p
        (move *curr* :x 0 :y 0 :width sw :height sh)
        (memove *curr* 0 0 sw sh))))

(defun center ()
  "Toggle center the current window."
  (let ((sw (screen-width *screen*))
        (sh (screen-height *screen*))
        (w (drawable-width *curr*))
        (h (drawable-height *curr*)))
    (memove *curr* (- (truncate sw 2) (truncate w 2))
            (- (truncate sh 2) (truncate h 2)) w h)))

(defun split-string (string &optional (character #\Space))
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
        as j = (position character string :start i)
        collect (subseq string i j)
        while j))

(defun x11-capitalize (string)
  "Returns a capitalize string according to X11 class name
convention."
  (if (char= (elt string 0) #\x)
      (concatenate 'string "X" (string-capitalize (subseq string 1)))
      (string-capitalize string)))

(defun run (command)
  (let ((com (split-string command)))
    #+clisp (ext:run-program (first com) :arguments (rest com) :wait nil)
    #+sbcl (sb-ext:run-program (first com) (rest com) :wait nil :search t)))

(defun raise-or-run (program)
  "Raise (an existing window) or run a program."
  (let ((win (find (x11-capitalize program) *windows* :test #'string-equal :key #'xclass)))
    (if win
	(focus win)
	(run program))))

(defun send-message (window type &rest data)
  (send-event window :client-message nil :window window
                     :type type :format 32 :data data))

;;; Apps in path
(defun execp (pathname)
  "Return T if the pathname describes an executable file."
  (declare (ignorable pathname))
  #+sbcl
  (let ((filename (namestring pathname)))
    (and (or (pathname-name pathname)
             (pathname-type pathname))
         (sb-unix:unix-access filename sb-unix:x_ok)))
  #+clisp
  (not (zerop (logand (posix:convert-mode (posix:file-stat-mode (posix:file-stat pathname)))
		      (posix:convert-mode '(:xusr :xgrp :xoth)))))
  #-(or sbcl clisp) t)

(defun getenv (var) #+sbcl (sb-ext:posix-getenv var) #+clisp (ext:getenv var))

(defparameter *apps*
  (let ((paths (split-string (getenv "PATH") #\:)))
    (loop for path in paths
          append (loop for file in (directory (merge-pathnames
                                               #+sbcl (make-pathname :name :wild :type :wild)
					       #+clisp (make-pathname :name :wild)
                                               (concatenate 'string path "/")))
                       when (execp file) collect (file-namestring file)))))

;;; Modifier keypress avoidance code
(defvar *mods-code* (multiple-value-call #'append (modifier-mapping *display*)))

(defun is-modifier (keycode)
  "Return non-nil if keycode is a modifier"
  (member keycode *mods-code*))

;;; App launcher, class finder, etc.
(defparameter *abort* (compile-shortcut :control #\g))
(defparameter *this* (compile-shortcut #\Return)
  "Validate THIS value even if it is ambiguous on more than one.")

(defun single-p (list) (and (consp list) (null (cdr list))))

(defun one-char ()
  "Get one char from the user. The keyboard should be grabbed before call."
  (event-case (*display*)
    (:key-press (state code)
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
        ((single-p list)
         (funcall fn (car list)))
        (t (let ((char (one-char)))
             (etypecase char
               (character
                (vector-push-extend char sofar)
                (recdo-1 sofar (remove-if-not (funcall matcher sofar) list :key key)
                         fn matcher key))
               (symbol
                (cond ((eql char 'abort))
                      ((eql char 'this)
                       (funcall fn (car list))))))))))

(defun recdo (list fn &key (matcher #'pre-matcher) (key #'identity))
  (let ((sofar (make-array 0 :element-type 'character :fill-pointer t :adjustable t)))
    (recdo-1 sofar list fn matcher key)))

(defun app ()
  (grab-keyboard *root*)
  (unwind-protect (recdo *apps* #'run)
    (ungrab-keyboard *display*)))

(defun finder ()
  (grab-keyboard *root*)
  (unwind-protect (recdo *windows* #'focus :key #'xclass :matcher #'in-matcher)
    (ungrab-keyboard *display*)))

(defun banish ()
  "Banish mouse pointer."
  (warp-pointer *root* (screen-width *screen*) (screen-height *screen*)))

;;; Default keyboard prefix and mouse shorcuts
(defparameter *prefix* (compile-shortcut :control #\t) "Prefix for shortcuts")
(defparameter *quit* (compile-shortcut :shift #\q) "Shortcut to quit")
(defparameter *move* (compile-shortcut :mod-1 1) "Mouse button to move a window")
(defparameter *resize* (compile-shortcut :mod-1 3) "Mouse button to resize a window")
(defparameter *close* (compile-shortcut :control :mod-1 2) "Mouse button to close a window")

(defun send-prefix (window)
  (send-event window :key-press (make-event-mask :key-press)
                     :window window
                     :code (code *prefix*)
                     :state (first (states *prefix*))))

(defun grab-it (shortcut &key inverse-p)
  "Grab a given key or button shortcut in all its
states. Use :inverse-p key to ungrab."
  (let ((code (code shortcut)))
    (if (< code 4)                      ;it's a mouse button
	(if inverse-p
            (dolist (s (states shortcut))
              (ungrab-button *root* code :modifiers s))
            (dolist (s (states shortcut))
              (grab-button *root* code '(:button-press) :modifiers s)))
        (let ((grab/ungrab (if inverse-p #'ungrab-key #'grab-key)))
          (dolist (s (states shortcut))
            (funcall grab/ungrab *root* code :modifiers s))))))

(defun grab-all ()
  "Grab prefix and mouse buttons on root."
  (grab-it *prefix*)
  (dolist (b (list *move* *resize* *close*))
    (grab-it b)))

(defun ungrab-all ()
  (dolist (b (list *move* *resize* *close*))
    (grab-it b :inverse-p t))
  (grab-it *prefix* :inverse-p t))

(defparameter *cursor*
  (let* ((white (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))
         (black (xlib:make-color :red 0.0 :green 0.0 :blue 0.0))
         (cursor-font (xlib:open-font *display* "cursor")))
    (xlib:create-glyph-cursor :source-font cursor-font
                              :source-char 64
                              :mask-font cursor-font
                              :mask-char 65
                              :foreground black
                              :background white)))

(defun grab-mouse (window event-mask)
  "Grab pointer with a cursor change that states that we're talking to
the window manager."
  (grab-pointer window event-mask :cursor *cursor*))

(defun ungrab-mouse () (ungrab-pointer *display*))

(defun load-rc ()
  (ungrab-all)
  (ignore-errors (load *rc*))
  (grab-all)
  (display-finish-output *display*))

;;; Keyboard shortcuts
(defshortcut (:shift #\r) (load-rc))
(defshortcut (#\c) (raise-or-run "xterm"))
(defshortcut (:shift #\c) (run "xterm"))
(defshortcut (#\e) (raise-or-run "emacs"))
(defshortcut (#\w) (raise-or-run "firefox"))
(defshortcut (:control #\l) (run "xlock"))
(defshortcut (#\n) (focus (next)))
(defshortcut (:control #\n) (focus (next)))
(defshortcut (#\p) (focus (next #'1-)))
(defshortcut (:control #\p) (focus (next #'1-)))
(defshortcut (#\a) (app))
(defshortcut (#\b) (banish))
(defshortcut (#\') (finder))
(defshortcut (#\f) (fullscreen))
(defshortcut (:shift #\f) (fullscreen :permanent-p t))
(defshortcut (#\.) (center))
(defshortcut (:shift #\.) (center))

(defvar last-button nil)
(defvar last-x nil)
(defvar last-y nil)
(defvar last-motion nil)
(defvar waiting-shortcut nil)

(defhandler :key-press (state code)
  (unless (is-modifier code)
    (cond (waiting-shortcut
           (cond ((sc= *quit* state code) 'quit)
                 (t
                  (cond ((sc= *prefix* state code) (focus *last*))
                        ((= code (code *prefix*)) (send-prefix *curr*))
                        (t (let ((fn (cdr (assoc-if #'(lambda (sc) (sc= sc state code))
                                                    *shortcuts*))))
                             (when (functionp fn) (funcall fn)))))
                  (ungrab-keyboard *display*)
                  (ungrab-mouse)
                  (setf waiting-shortcut nil))))
          ((sc= *prefix* state code)
           (grab-keyboard *root*)
           (grab-mouse *root* nil)
           (setf waiting-shortcut t)))))

(defhandler :button-press (state code child x y)
  (when (and child (eql (window-override-redirect child) :off)
             (null (let ((w (find child *windows* :test #'win=)))
                     (when w (getf (window-plist w) 'original-dimension)))))
    (cond ((sc= *close* state code)
           (send-message child :WM_PROTOCOLS (intern-atom *display* :WM_DELETE_WINDOW)))
	  (t
	   (cond ((sc= *move* state code)
		  (setf last-x x last-y y))
		 ((sc= *resize* state code)
		  (multiple-value-bind (x y width height)
		      (move child :width (drawable-width child)
				  :height (drawable-height child))
		    ;; here i use [last-x; last-y] as the [x; y]
		    ;; position of the current window
		    (setf last-x x last-y y)
		    (warp-pointer child width height))))
           (setf last-button code)
	   (focus child)
	   (grab-mouse child '(:pointer-motion :button-release))))))

(defhandler :motion-notify (event-window root-x root-y time)
  (when (or (null last-motion) (> (- time last-motion) (/ 1000 60)))
    (let ((delta-x (- root-x last-x))
	  (delta-y (- root-y last-y)))
      (cond ((= last-button (code *move*))
	     (multiple-value-bind (x y width height dx dy)
		 (move event-window :dx delta-x :dy delta-y)
	       (incf last-x dx)
               (incf last-y dy)))
	    ((= last-button (code *resize*))
	     (let (pos)
	       (if (plusp delta-x)
		   (setf (getf pos :x) last-x
			 (getf pos :width) delta-x)
		   (setf (getf pos :x) (+ last-x delta-x)
			 (getf pos :width) (abs delta-x)))
	       (if (plusp delta-y)
		   (setf (getf pos :y) last-y
			 (getf pos :height) delta-y)
		   (setf (getf pos :y) (+ last-y delta-y)
			 (getf pos :height) (abs delta-y)))
	       (apply #'move event-window pos)))))
    (setf last-motion time)))

(defhandler :button-release () (ungrab-mouse))

(defhandler :map-notify (window override-redirect-p)
  (unless override-redirect-p
    (focus window)))

(defhandler :unmap-notify (window)
  (when (managed-p window)
    (focus (minus window))))

(defhandler :map-request (parent send-event-p window)
  (format t "~&map-request: ~a ~a ~a~%" parent send-event-p window)
  (restart-case (map-window (plus window))
    (window-error ()
      (format t "~&ZZZ ~a~%" window)
      'mmkay)))

(defhandler :configure-request (stack-mode window x y width height border-width value-mask)
  (let ((list-mask (loop for i below 4
                         when (= (ldb (byte 1 i) value-mask) 1)
                           nconc (case i
                                   (0 (list :x x))
                                   (1 (list :y y))
                                   (2 (list :width width))
                                   (3 (list :height height))))))
    (format t "~&configure-request: ~a ~a ~s~%" window value-mask list-mask)
    (restart-case (when list-mask (apply #'move window list-mask))
      (window-error ()
        (format t "~&XXX ~a~%" window)
        'wont-move))))

;; Restart functions
(defun window-error (c)
  (declare (ignore c))
  (let ((restart (find-restart 'window-error)))
    (when restart
      (invoke-restart restart))))

(defun evloop ()
  (do () ((eql (process-event *display* :handler *handlers* :discard-p t) 'quit))))

(defun main ()
  (load-rc)

  ;; Populate list of windows
  (dolist (w (query-tree *root*))
    (when (and (eql (window-map-state w) :viewable)
               (eql (window-override-redirect w) :off))
      (plus w)))

  (intern-atom *display* :_MOTIF_WM_HINTS)
  (setf (window-event-mask *root*) '(:substructure-notify :substructure-redirect))

  (setf *last* (setf *curr* (first *windows*)))
  (focus *curr*)

  (unwind-protect (handler-bind ((window-error #'window-error)) (evloop))
    (ungrab-all)
    (close-display *display*)))

(main)
