;;; Used to be the most simple window manager on earth. It started as
;;; a fork of the lisp version of tinywm.

;;; Make a swank server (not for clisp because i can't make
;;; create-server to return).
#-clisp (swank:create-server :port 4005 :dont-close t)

(defpackage :most.simple.wm
  (:use :common-lisp))
(in-package :most.simple.wm)

(defvar *display* (xlib:open-default-display))
(defvar *screen* (xlib:display-default-screen *display*))
(defvar *root* (xlib:screen-root *screen*))
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
  "Return the n-th command line argument as a string. 0 is the first
argument."
  (let ((args #+sbcl (rest sb-ext:*posix-argv*)
              #+clisp ext:*args*))
    (elt args n)))

;;; Set screen geometry if provided on command line
(cond ((= (argc) 1)
       (setf (xlib:screen-height *screen*) (parse-integer (args 0))))
      ((= (argc) 2)
       (setf (xlib:screen-height *screen*) (parse-integer (args 0))
             (xlib:screen-width *screen*) (parse-integer (args 1)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))

(defmacro defhandler (event keys &body body)
  (let ((fn-name (gensym (symbol-name event)))
	(event-slots (gensym)))
    `(labels ((,fn-name (&rest ,event-slots &key ,@keys &allow-other-keys)
		(declare (ignore ,event-slots))
		,@body))
       (setf (elt *handlers* (position ,event xlib::*event-key-vector*)) #',fn-name))))

(defun xclass (window) (multiple-value-bind (name class)
                           (restart-case (xlib:get-wm-class window)
                             (window-error (c)
                               (format t "~&No class on ~a~%" window)
                               (values "" "I'm dead")))
                         class))

(defparameter *groupers* (list
                          #'(lambda (w) (search "Gimp" (xclass w) :test #'char-equal)))
  "List of predicates against which windows are grouped")

(defstruct (shortcut (:conc-name)) (states nil) (code 0))

(defun compile-shortcut (&rest l)
  "Compile a shortcut. Usage: (compile-shortcut :control #\t) for a
keyboard shortcut or (compile-shortcut :mod-1 1) for a mouse
shortcut. Takes care of CapsLock and NumLock combination."
  (let* ((k (car (last l)))
         (mods (butlast l))
         (states (list (apply #'xlib:make-state-mask mods)
                       (apply #'xlib:make-state-mask (cons :lock mods))
                       (apply #'xlib:make-state-mask (cons :mod-2 mods))
                       (apply #'xlib:make-state-mask (append '(:lock :mod-2) mods)))))
    (if (characterp k)
        (let ((code (car (last (multiple-value-list
                                (xlib:keysym->keycodes *display* (car (xlib:character->keysyms k))))))))
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

(defun on-p (switch window) (getf (xlib:window-plist window) switch))
(defun on (switch window &optional (value t)) (setf (getf (xlib:window-plist window) switch) value))
(defun off (switch window) (remf (xlib:window-plist window) switch))
(defun toggle (switch window &optional value)
  (if (on-p switch window) (off switch window) (on switch window value)))

(defun correct-size (window &optional x y width height dx dy dw dh)
  "Correct a window's dimensions with its sizehints."
  (let ((hints (xlib:wm-normal-hints window)))
    (when hints (let* ((min-w (or (xlib:wm-size-hints-min-width hints) 1))
                       (min-h (or (xlib:wm-size-hints-min-height hints) 1))
                       (inc-w (or (xlib:wm-size-hints-width-inc hints) 1))
                       (inc-h (or (xlib:wm-size-hints-height-inc hints) 1))
                       (base-w (or (xlib:wm-size-hints-base-width hints) 0))
                       (base-h (or (xlib:wm-size-hints-base-height hints) 0)))
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
    (unless x (setf x (xlib:drawable-x window)))
    (unless y (setf y (xlib:drawable-y window)))
    (unless width (setf width (xlib:drawable-width window)))
    (unless height (setf height (xlib:drawable-height window)))

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
    (let ((win-center (on-p :center-pinned window)))
      (when win-center
	  (setf x (- (first win-center) (truncate width 2))
		y (- (second win-center) (truncate height 2)))))
    (unless (on-p :pinned window)
      (setf (xlib:drawable-x window) x
	    (xlib:drawable-y window) y
	    (xlib:drawable-width window) width
	    (xlib:drawable-height window) height))
    (values x y width height dx dy dw dh)))

(defun win= (a b)
  (and (typep a 'xlib:window) (typep b 'xlib:window) (xlib:window-equal a b)))

(defun grouper (window)
  "Get the grouper function of a window if there is one."
  (and window (restart-case (find-if #'(lambda (f) (funcall f window)) *groupers*)
		(window-error (c) nil))))

(defun get-transients-of (window)
  (loop for w in (xlib:query-tree *root*)
	nconc (loop for id in (xlib:get-property w :WM_TRANSIENT_FOR)
		    when (= id (xlib:window-id window))
		      collect w)))

(defun %focus (window)
  "Low level focus for *one* managed window."
  (setf (xlib:window-priority window) :above)
  (dolist (w (get-transients-of window))
    (setf (xlib:window-priority w) :above)))

(defun focus (window)
  (unless (or (null window))
    (let* ((grouper (grouper window))
           (group (when (functionp grouper)
                    (sort (loop for w in *windows*
                                when (funcall grouper w) collect w) #'< :key #'xlib:window-id))))
      (cond (group
             (unless (member *curr* group :test #'win=)
               (setf *last* *curr*
                     *curr* (first group)))
             (dolist (w group) (%focus w))
             (xlib:set-input-focus *display* :pointer-root :pointer-root))
            (t
             (unless (win= *curr* window)
               (setf *last* *curr*
                     *curr* window))
             (xlib:set-input-focus *display* window :pointer-root)))
      (%focus window))))

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
  (unless (or (eql (xlib:window-override-redirect window) :on)
	      (xlib:get-property window :WM_TRANSIENT_FOR))
    (pushnew window *windows* :test #'win=))
  window)

(defun minus (window)
  "House keeping when window is removed from managed windows. Returns
the window to be focused."
  (setf *windows* (remove window *windows* :test #'win=))
  (cond ((null *windows*) (setf *curr* nil *last* nil))
        (:otherwise
         (when (win= *curr* window) (setf *curr* *last*))
         (when (win= *last* window) (setf *last* (next)))
         (when (win= *curr* *last*) (setf *last* (next #'1-)))
         *curr*)))

(defun managed-p (window)
  "Return window from the managed windows list if it is here."
  (find window *windows* :test #'win=))

(defun fullscreen (&key pinned-p)
  "Set the current window fullscreen."
  (let ((sw (xlib:screen-width *screen*))
        (sh (xlib:screen-height *screen*)))
    (move *curr* :x 0 :y 0 :width sw :height sh)
    (when pinned-p (on :pinned *curr*))))

(defun window-center (window)
  (let ((x (xlib:drawable-x window))
	(y (xlib:drawable-y window))
	(width (xlib:drawable-width window))
	(height (xlib:drawable-height window)))
    (list (+ x (truncate width 2))
	  (+ y (truncate height 2)))))

(defun center (&key center-pinned-p)
  "Center the current window."
  (let ((sw (xlib:screen-width *screen*))
        (sh (xlib:screen-height *screen*))
        (w (xlib:drawable-width *curr*))
        (h (xlib:drawable-height *curr*)))
    (move *curr* :x (- (truncate sw 2) (truncate w 2))
                 :y (- (truncate sh 2) (truncate h 2)))
    (when center-pinned-p (on :center-pinned *curr* (window-center *curr*)))))

(defun wash-sticky-position ()
  (off :center-pinned *curr*)
  (off :pinned *curr*))

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
    #+sbcl (restart-case (sb-ext:run-program (first com) (rest com) :wait nil :search t)
                         (simple-error (c) (format t "~&Cannot execute ~s~%" (first com))))))

(defun raise-or-run (program &optional class)
  "Raise (an existing window) or run a program."
  (let ((win (find (if class class (x11-capitalize program))
                   *windows* :test #'string-equal :key #'xclass)))
    (if win
	(focus win)
	(run program))))

(defun send-message (window type &rest data)
  (xlib:send-event window :client-message nil :window window
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
(defvar *mods-code* (multiple-value-call #'append (xlib:modifier-mapping *display*)))

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
  (xlib:event-case (*display*)
    (:key-press (state code)
                (cond ((is-modifier code) (one-char))
                      ((sc= *abort* state code) 'abort)
                      ((sc= *this* state code) 'this)
                      (t (xlib:keycode->character *display* code state))))))

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
  (xlib:grab-keyboard *root*)
  (unwind-protect (recdo *apps* #'run)
    (xlib:ungrab-keyboard *display*)))

(defun finder ()
  (xlib:grab-keyboard *root*)
  (unwind-protect (recdo *windows* #'focus :key #'xclass :matcher #'in-matcher)
    (xlib:ungrab-keyboard *display*)))

(defun banish ()
  "Banish mouse pointer."
  (xlib:warp-pointer *root* (xlib:screen-width *screen*) (xlib:screen-height *screen*)))

;;; Default keyboard prefix and mouse shorcuts
(defparameter *prefix* (compile-shortcut :control #\t) "Prefix for shortcuts")
(defparameter *quit* (compile-shortcut :shift #\q) "Shortcut to quit")
(defparameter *move* (compile-shortcut :mod-1 1) "Mouse button to move a window")
(defparameter *resize* (compile-shortcut :mod-1 3) "Mouse button to resize a window")
(defparameter *close* (compile-shortcut :control :mod-1 2) "Mouse button to close a window")

(defun send-prefix (window)
  (xlib:send-event window :key-press (xlib:make-event-mask :key-press)
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
              (xlib:ungrab-button *root* code :modifiers s))
            (dolist (s (states shortcut))
              (xlib:grab-button *root* code '(:button-press) :modifiers s)))
        (let ((grab/ungrab (if inverse-p #'xlib:ungrab-key #'xlib:grab-key)))
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
  (xlib:grab-pointer window event-mask :cursor *cursor*))

(defun ungrab-mouse () (xlib:ungrab-pointer *display*))

(defun load-rc ()
  (ungrab-all)
  (ignore-errors (load *rc*))
  (grab-all)
  (xlib:display-finish-output *display*))

;;; Keyboard shortcuts
(defshortcut (:shift #\r) (load-rc))
(defshortcut (#\c) (raise-or-run "xterm"))
(defshortcut (:control #\c) (run "xterm"))
(defshortcut (#\e) (raise-or-run "emacsclient -c -a emacs" "Emacs"))
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
(defshortcut (:shift #\f) (fullscreen :pinned-p t))
(defshortcut (#\/) (center))
(defshortcut (#\.) (center :center-pinned-p t))
(defshortcut (:shift #\.) (center :center-pinned-p t))
(defshortcut (:shift #\p) (toggle :pinned *curr*))
(defshortcut (:shift #\c) (toggle :center-pinned *curr* (window-center *curr*)))
(defshortcut (:shift #\w) (wash-sticky-position))

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
                  (xlib:ungrab-keyboard *display*)
                  (ungrab-mouse)
                  (setf waiting-shortcut nil))))
          ((sc= *prefix* state code)
           (xlib:grab-keyboard *root*)
           (grab-mouse *root* nil)
           (setf waiting-shortcut t)))))

;; In the two following handlers, the top `window' binding is required
;; else we're using a fresh instance of window without its plist set
;; for instance
(defhandler :button-press (state code child x y)
  (let ((window (managed-p child)))
    (when (and window (eql (xlib:window-override-redirect window) :off))
      (cond ((sc= *close* state code)
             (send-message window :WM_PROTOCOLS (xlib:intern-atom *display* :WM_DELETE_WINDOW)))
            (t
             (cond ((sc= *move* state code)
                    (setf last-x x last-y y))
                   ((sc= *resize* state code)
                    (multiple-value-bind (x y width height) (move window)
                      ;; here i use [last-x; last-y] as the [x; y]
                      ;; position of the current window
                      (setf last-x x last-y y)
                      (xlib:warp-pointer window width height))))
             (setf last-button code)
             (focus window)
             (grab-mouse window '(:pointer-motion :button-release)))))))

(defhandler :motion-notify (event-window root-x root-y time)
  (let ((window (managed-p event-window)))
    (xlib:with-state (window)
      (when (or (null last-motion) (> (- time last-motion) (/ 1000 60)))
	(let ((delta-x (- root-x last-x))
	      (delta-y (- root-y last-y)))
	  (cond ((= last-button (code *move*))
		 (multiple-value-bind (x y width height dx dy) (move window :dx delta-x :dy delta-y)
		   (declare (ignore x y width height))
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
		   (multiple-value-bind (x y) (apply #'move window pos)
		     (setf last-x x last-y y))))))
	(setf last-motion time)))))

(defhandler :button-release () (ungrab-mouse))

(defhandler :map-notify (window override-redirect-p)
  (unless override-redirect-p
    (focus (managed-p window))))

(defhandler :unmap-notify (window)
  (when (managed-p window)
    (focus (minus window))))

(defhandler :map-request (parent send-event-p window)
  (format t "~&map-request: ~a ~a ~a~%" parent send-event-p window)
  (restart-case (xlib:map-window (plus window))
    (window-error (c)
      (format t "~&map-request: ~a ~a~%" c window)
      'processed)
    (value-error (c)
      (format t "~&map-request: ~a ~a~%" c window)
      'processed)))

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
      (window-error (c)
        (format t "~&configure-request: ~a ~a~%" c window)
        'processed)
      (value-error (c)
        (format t "~&configure-request: ~a ~a~%" c window)
        'processed))))

;; Restart functions
(defmacro defrestart (name)
  (let ((fname (intern (concatenate 'string "RESTART-" (symbol-name name)))))
    (with-gensyms (c restart)
      `(defun ,fname (,c)
         (let ((,restart (find-restart ',name)))
           (when ,restart
             (invoke-restart ,restart ,c)))))))

(defrestart window-error)
(defrestart simple-error)
(defrestart value-error)

(defun evloop ()
  (do () ((eql (xlib:process-event *display* :handler *handlers* :discard-p t) 'quit))))

(defun main ()
  (load-rc)

  ;; Populate list of windows with already mapped windows
  (dolist (w (xlib:query-tree *root*))
    (when (eql (xlib:window-map-state w) :viewable)
      (plus w)))

  (xlib:intern-atom *display* :_MOTIF_WM_HINTS)
  (setf (xlib:window-event-mask *root*) '(:substructure-notify :substructure-redirect))

  (setf *last* (setf *curr* (first *windows*)))
  (focus *curr*)

  (unwind-protect (handler-bind ((xlib:window-error #'restart-window-error)
                                 (xlib:value-error #'restart-value-error)
                                 (simple-error #'restart-simple-error))
                    (evloop))
    (ungrab-all)
    (xlib:close-display *display*)))

(main)
