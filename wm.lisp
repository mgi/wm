(require 'asdf)
(asdf:oos 'asdf:load-op 'clx)
(shadow 'char-width)
(use-package :xlib)
(defparameter *mods*    '(:mod-1))
(defparameter *move*    1)
(defparameter *resize*  3)
(defparameter *lower*   4)
(defparameter *raise*   5)
(defparameter *display* 0)            ; set this to an integer to do testing with xnest

(defun open-default-display (&optional display-name)
  "Open a connection to DISPLAY-NAME if supplied, or to the appropriate
default display as given by GET-DEFAULT-DISPLAY otherwise.

OPEN-DISPLAY-NAME always attempts to do display authorization.  The
hostname is resolved to an address, then authorization data for the
(protocol, host-address, displaynumber) triple is looked up in the
file given by AUTHORITY_PATHNAME (typically $HOME/.Xauthority).  If
the protocol is :local, or if the hostname resolves to the local host,
authority data for the local machine's actual hostname - as returned
by gethostname(3) - is used instead."
  (destructuring-bind (host display screen protocol)
      (get-default-display display-name)
    (declare (ignore screen))
    (open-display host :display display :protocol protocol)))

(defun main ()
  (let* ((display (if *display*
                      (open-display "" :display *display*)
                      (open-default-display)))
         (screen (first (display-roots display)))
         (root (screen-root screen)))

    (dolist (button (list *move* *resize* *lower* *raise*))
      (grab-button root button '(:button-press) :modifiers *mods*))

    (unwind-protect
        (let (last-button last-x last-y)
          (do () (nil)                  ; infinite loop
            (event-case (display :discard-p t)
             ;; for key-press and key-release, code is the keycode
             ;; for button-press and button-release, code is the button number
             (:button-press (code child event-window)
              (cond ((= code *raise*)
                     (circulate-window-up root))
                    ((= code *lower*)
                     (circulate-window-down root))
                    ((or (= code *move*)
                         (= code *resize*))
                     (when child        ; do nothing if we're not over a window
                       (setf last-button code)
                       (grab-pointer child '(:pointer-motion :button-release))
                       (let ((lst (multiple-value-list (query-pointer root))))
                         (setf last-x (sixth   lst)
                               last-y (seventh lst)))))))
             (:motion-notify
              (event-window root-x root-y)
              ;; while(XCheckTypedEvent(display, MotionNotify, &ev));
              (let ((delta-x (- root-x last-x))
                    (delta-y (- root-y last-y)))
                (cond ((= last-button *move*)
                       ;; (incf (drawable-x event-window) delta-x)
                       ;; (incf (drawable-y event-window) delta-y)
                       (setf (drawable-x event-window) root-x
                             (drawable-y event-window) root-y))
                      ((= last-button *resize*)
                       ;; (incf (drawable-width event-window) delta-x)
                       ;; (incf (drawable-height event-window) delta-y)
                       (setf (drawable-width event-window)
                             (max 1 (- root-x (drawable-x event-window)))
                             (drawable-height event-window)
                             (max 1 (- root-y (drawable-y event-window))))))))
             (:button-release ()
              (ungrab-pointer display)))))
      (dolist (button (list *move* *resize* *lower* *raise*))
        (ungrab-button root button :modifiers *mods*))
      (close-display display))))

(main)
