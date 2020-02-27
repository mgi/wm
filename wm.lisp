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
                             (window-error (c) (declare (ignore c)) (values "" "I'm probably dead")))
                         (declare (ignore name))
                         class))

(defparameter *families* (list
                          #'(lambda (w) (search "Gimp" (xclass w) :test #'char-equal)))
  "List of predicates to make some windows of the same family.")

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

(defun sc= (sc state code) (and (code sc) (= (code sc) code) (member state (states sc))))
(defun sc-equal (sc1 sc2) (equalp sc1 sc2))

(defmacro define-shortcut (key &body body)
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

(defun %group (window char)
  (labels ((set-char (window)
             (setf (getf (xlib:window-plist window) :group) char)))
    (let ((family (family window)))
      (if (consp family)
          (mapcar #'set-char family)
          (set-char window)))))

(defun pinned-p (window) (getf (xlib:window-plist window) :pinned))
(defun pin (window) (setf (getf (xlib:window-plist window) :pinned) t))
(defun unpin (window) (remf (xlib:window-plist window) :pinned))
(defun toggle-pin ()
  (if (pinned-p *curr*) (unpin *curr*) (pin *curr*)))

(defun correct-size (window x y width height dx dy dw dh)
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
    (unless (pinned-p window)
      (setf (xlib:drawable-x window) x
            (xlib:drawable-y window) y
            (xlib:drawable-width window) width
            (xlib:drawable-height window) height))
    (values x y width height dx dy dw dh)))

(defun win= (a b)
  (and (xlib:window-p a) (xlib:window-p b) (xlib:window-equal a b)))

(defun family (window)
  "Get members of the family and group of a window."
  (let ((family-fun (and window (restart-case (find-if #'(lambda (f) (funcall f window)) *families*)
                                  (window-error (c) (declare (ignore c)) nil))))
        (group-tag (getf (xlib:window-plist window) :group))
        result)
    (when (functionp family-fun)
      (dolist (w *windows*)
        (when (funcall family-fun w)
          (pushnew w result :test #'win=))))
    (when group-tag
      (dolist (w *windows*)
        (when (let ((tag (getf (xlib:window-plist w) :group)))
                (and tag (string= tag group-tag) (not (win= window w))))
          (pushnew w result :test #'win=))))
    result))

(defun get-transients-of (window)
  (restart-case
      (loop for w in (xlib:query-tree *root*)
            nconc (loop for id in (xlib:get-property w :WM_TRANSIENT_FOR)
                        when (= id (xlib:window-id window))
                          collect w))
    (window-error (c) (declare (ignore c)) nil)
    (match-error (c)
      (format t "~&get-transients-of: ~a ~a~%" c window)
      nil)))

(defun focus (window)
  "Focus one window and its family if needed."
  (when window
    (labels ((up (win)
               (setf (xlib:window-priority win) :above)
               (dolist (w (get-transients-of win))
                 (setf (xlib:window-priority w) :above))))
      (let ((family (sort (family window) #'< :key #'xlib:window-id)))
        (cond (family
               ;; focus every other family members and set input
               ;; focus as follow mouse
               (unless (member *curr* family :test #'win=)
                 (setf *last* *curr*
                       *curr* (first family)))
               (dolist (w family) (up w))
               (xlib:set-input-focus *display* :pointer-root :pointer-root))
              (t
               ;; set input focus to window
               (unless (win= *curr* window)
                 (setf *last* *curr*
                       *curr* window))
               (xlib:set-input-focus *display* window :pointer-root)))
        (up window)))))

(defun next (&optional (way #'1+))
  (let* ((family (family *curr*))
         (windows (set-difference *windows* family :test #'win=))
         (n (length windows))
         (nw (or (position *curr* windows :test #'win=) 0)))
    (unless (zerop n)
      (nth (mod (funcall way nw) n) windows))))

(defun transient-for-managed-p (window)
  (loop for id in (xlib:get-property window :WM_TRANSIENT_FOR)
          thereis (find id *windows* :key #'xlib:window-id)))

(defun plus (window)
  "Add window to the list of managed windows."
  (unless (or (eql (xlib:window-override-redirect window) :on)
              (transient-for-managed-p window))
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
    (xlib:with-state (*curr*)
      (move *curr* :x 0 :y 0 :width sw :height sh))
    (when pinned-p (pin *curr*))))

(defun center ()
  "Center the current window."
  (when *curr*
    (let ((sw (xlib:screen-width *screen*))
          (sh (xlib:screen-height *screen*))
          (w (xlib:drawable-width *curr*))
          (h (xlib:drawable-height *curr*)))
      (xlib:with-state (*curr*)
        (move *curr* :x (- (truncate sw 2) (truncate w 2))
                     :y (- (truncate sh 2) (truncate h 2)))))))

(defun wash-sticky-position ()
  (unpin *curr*))

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

(defun raise-or-launch (program &optional class)
  "Raise (an existing window) or launch a program."
  (let ((win (find (if class class (x11-capitalize program))
                   *windows* :test #'string-equal :key #'xclass)))
    (if win
        (focus win)
        (uiop:launch-program program))))

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
  (unwind-protect (recdo *apps* #'uiop:launch-program)
    (xlib:ungrab-keyboard *display*)))

(defun finder ()
  (xlib:grab-keyboard *root*)
  (unwind-protect (recdo *windows* #'focus :key #'xclass :matcher #'in-matcher)
    (xlib:ungrab-keyboard *display*)))

(defun group ()
  (xlib:grab-keyboard *root*)
  (unwind-protect (recdo (list "a" "b" "c") #'(lambda (c) (%group *curr* c)))
    (xlib:ungrab-keyboard *display*)))

(defun ungroup () (remf (xlib:window-plist *curr*) :group))

(defun banish ()
  "Banish mouse pointer."
  (xlib:warp-pointer *root* (xlib:screen-width *screen*) (xlib:screen-height *screen*)))

;;; Default keyboard prefix and mouse shorcuts
(defparameter *prefix* (compile-shortcut :control #\t) "Prefix for shortcuts")
(defparameter *quit* (compile-shortcut :shift #\q) "Shortcut to quit")
(defparameter *move* (compile-shortcut :mod-1 1) "Mouse button to move a window")
(defparameter *center-resize* (compile-shortcut :mod-1 2) "Mouse button to resize a window from its center")
(defparameter *resize* (compile-shortcut :mod-1 3) "Mouse button to resize a window")
(defparameter *close* (compile-shortcut :mod-1 :shift 3) "Mouse button to close a window")

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
  (dolist (b (list *move* *center-resize* *resize* *close*))
    (grab-it b)))

(defun ungrab-all ()
  (dolist (b (list *move* *center-resize* *resize* *close*))
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
(define-shortcut (:shift #\r) (load-rc))
(define-shortcut (#\c) (raise-or-launch "xterm"))
(define-shortcut (:control #\c) (uiop:launch-program "xterm"))
(define-shortcut (#\e) (raise-or-launch (getenv "EDITOR") "Emacs"))
(define-shortcut (#\w) (raise-or-launch "firefox"))
(define-shortcut (:control #\l) (uiop:launch-program "pkill -USR1 xidle"))
(define-shortcut (#\n) (focus (next)))
(define-shortcut (:control #\n) (focus (next)))
(define-shortcut (#\p) (focus (next #'1-)))
(define-shortcut (:control #\p) (focus (next #'1-)))
(define-shortcut (#\a) (app))
(define-shortcut (#\b) (banish))
(define-shortcut (#\') (finder))
(define-shortcut (#\f) (fullscreen))
(define-shortcut (#\g) (group))
(define-shortcut (#\u) (ungroup))
(define-shortcut (:shift #\f) (fullscreen :pinned-p t))
(define-shortcut (#\.) (center))
(define-shortcut (:shift #\.) (center))
(define-shortcut (:shift #\p) (toggle-pin))
(define-shortcut (:shift #\w) (wash-sticky-position))

(defvar last-button nil)
(defvar last-x nil)
(defvar last-y nil)
(defvar last-motion nil)
(defvar waiting-shortcut nil)

(defun funcall-shortcut (state code)
  (let ((fn  (cdr (assoc-if #'(lambda (sc) (sc= sc state code)) *shortcuts*))))
    (when (functionp fn) (funcall fn))))

(defhandler :key-press (state code)
  (unless (is-modifier code)
    (cond (waiting-shortcut
           (cond ((sc= *quit* state code) 'quit)
                 (t
                  (cond ((sc= *prefix* state code) (focus *last*))
                        ((= code (code *prefix*)) (send-prefix *curr*))
                        (t (funcall-shortcut state code)))
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
                    (let ((x (xlib:drawable-x window))
                          (y (xlib:drawable-y window))
                          (width (xlib:drawable-width window))
                          (height (xlib:drawable-height window)))
                      ;; here i use [last-x; last-y] as the [x; y]
                      ;; position of the current window
                      (setf last-x x last-y y)
                      (unless (pinned-p window)
                        (xlib:warp-pointer window width height))))
                   ((sc= *center-resize* state code)
                    (setf last-x x last-y y)))
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
                 (move window :width delta-x :height delta-y))
                ((= last-button (code *center-resize*))
                 ;; In order to take size-hints into account, we need
                 ;; to first move the window (x,y) and then, in a
                 ;; second time, wide/narrow the window
                 (let* ((move-1 (multiple-value-list (move window :dx (- delta-x)
                                                                  :dy (- delta-y))))
                        (dx (nth 4 move-1))
                        (dy (nth 5 move-1))
                        (move-2 (multiple-value-list (move window :dw (* 2 (- dx))
                                                                  :dh (* 2 (- dy)))))
                        (dw (nth 6 move-2))
                        (dh (nth 7 move-2)))
                   (incf last-x (truncate (- dw dx) 2))
                   (incf last-y (truncate (- dh dy) 2))))))
        (setf last-motion time)))))

(defhandler :button-release () (ungrab-mouse))

(defhandler :map-notify (window override-redirect-p)
  (unless override-redirect-p
    (focus (managed-p window))))

(defhandler :destroy-notify (window)
  (when (managed-p window)
    (focus (minus window))))

(defhandler :map-request (window)
  (restart-case (xlib:map-window (plus window))
    (window-error (c)
      (format t "~&map-request: ~a ~a~%" c window)
      'processed)
    (value-error (c)
      (format t "~&map-request: ~a ~a~%" c window)
      'processed)))

(defhandler :configure-request (window x y width height value-mask)
  (let ((list-mask (loop for i below 4
                         when (= (ldb (byte 1 i) value-mask) 1)
                           nconc (case i
                                   (0 (list :x x))
                                   (1 (list :y y))
                                   (2 (list :width width))
                                   (3 (list :height height))))))
    (xlib:with-state (window)
      (restart-case (when list-mask (apply #'move window list-mask))
        (window-error (c)
          (format t "~&configure-request: ~a ~a~%" c window)
          'processed)
        (value-error (c)
          (format t "~&configure-request: ~a ~a~%" c window)
          'processed)))))

#+clx-ext-randr
(defun crtc-size-of-output (output)
  (multiple-value-bind (status timestamp crtc) (xlib:rr-get-output-info *display* output 0)
    (when (eq status :success)
      (multiple-value-bind (status timestamp x y width height) (xlib:rr-get-crtc-info *display* crtc timestamp)
        (declare (ignore timestamp x y))
        (when (eq status :success)
          (values width height))))))

#+clx-ext-randr
(defhandler :rr-screen-change-notify (root-window width height)
  ;; Honor the primary output or defaults to the new given width and
  ;; height
  (let ((primary (xlib:rr-get-output-primary root-window)))
    (cond ((zerop primary)
           (setf (xlib:screen-width *screen*) width
                 (xlib:screen-height *screen*) height))
          (t (multiple-value-bind (width height) (crtc-size-of-output primary)
               (setf (xlib:screen-width *screen*) width
                     (xlib:screen-height *screen*) height))))))

#+clx-ext-randr
(defhandler :rr-crtc-change-notify (rotation)
  (let ((rotation (first (xlib:make-rotation-keys rotation))))
    (case rotation
      ((:rotate-0 :rotate-180) (when (> (xlib:screen-height *screen*) (xlib:screen-width *screen*))
                                 (rotatef (xlib:screen-width *screen*) (xlib:screen-height *screen*))))
      ((:rotate-90 :rotate-270) (when (> (xlib:screen-width *screen*) (xlib:screen-height *screen*))
                                  (rotatef (xlib:screen-width *screen*) (xlib:screen-height *screen*)))))))

;; Restart functions
(defmacro defrestart (name)
  (let ((fname (intern (concatenate 'string "RESTART-" (symbol-name name)))))
    (with-gensyms (c restart)
      `(defun ,fname (,c)
         (let ((,restart (find-restart ',name)))
           (when ,restart
             (invoke-restart ,restart ,c)))))))

(defrestart window-error)
(defrestart drawable-error)
(defrestart match-error)
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

  #+clx-ext-randr
  (xlib:rr-select-input *root* '(:screen-change-notify-mask :crtc-change-notify-mask))

  (setf *last* (setf *curr* (first *windows*)))
  (focus *curr*)

  (unwind-protect (handler-bind ((xlib:window-error #'restart-window-error)
                                 (xlib:drawable-error #'restart-drawable-error)
                                 (xlib:match-error #'restart-match-error)
                                 (xlib:value-error #'restart-value-error)
                                 (simple-error #'restart-simple-error))
                    (evloop))
    (ungrab-all)
    (xlib:close-display *display*)))

(main)
