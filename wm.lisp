#!/usr/local/bin/sbcl --script
;;; Most simple window manager on earth. It is a fork from the lisp
;;; version of tinywm.

;; Load CLX stuff
(require 'asdf)
(asdf:oos 'asdf:load-op 'clx)
(use-package :xlib)

;; Global parameters
(defparameter *mods* '(:mod-1))      ; :mod-4 for windows key
(defparameter *move* '(1))
(defparameter *resize* '(2))
(defparameter *lower* '(3 4))
(defparameter *raise* '(5))
(defparameter *f1* #xffbe
  "F1 keysym.")
(defparameter *f12* #xffc9
  "F12 keysym.")

(defun main ()
  (let* ((display (open-display ""))
         (screen (first (display-roots display)))
         (root (screen-root screen)))

    ;; Grab mouse buttons
    (dolist (button (list *move* *resize* *lower* *raise*))
      (mapcar #'(lambda (b)
                  (grab-button root b '(:button-press) :modifiers *mods*))
              button))

    ;; Grab shortcut
    (grab-key root (keysym->keycodes display *f1*))
    (grab-key root (keysym->keycodes display *f12*))

    (unwind-protect
        (let (last-button last-x last-y)
          (loop named eventloop do
            (event-case 
             (display :discard-p t)
             ;; for key-press and key-release, code is the keycode
             ;; for button-press and button-release, code is the button number
             (:key-press
              (code)
              (cond ((= code (keysym->keycodes display *f1*))
                     (sb-ext:run-program (posix-getenv "TERM") nil :wait nil :search t))
                    ((= code (keysym->keycodes display *f12*))
                     (return-from eventloop))))
             (:button-press 
              (code child)
              (cond ((member code *raise*)
                     (circulate-window-up root))
                    ((member code *lower*)
                     (circulate-window-down root))
                    ((or (member code *move*)
                         (member code *resize*))
                     (when child        ; do nothing if we're not over a window
                       (setf last-button code)
                       (grab-pointer child '(:pointer-motion :button-release))
                       (when (member code *resize*)
                         (warp-pointer child (drawable-width child) 
                                       (drawable-height child)))
                       (let ((lst (multiple-value-list (query-pointer root))))
                         (setf last-x (sixth   lst)
                               last-y (seventh lst)))))))
             (:motion-notify
              (event-window root-x root-y)
              (cond ((member last-button *move*)
                     (let ((delta-x (- root-x last-x))
                           (delta-y (- root-y last-y)))
                       (incf (drawable-x event-window) delta-x)
                       (incf (drawable-y event-window) delta-y)
                       (incf last-x delta-x)
                       (incf last-y delta-y)))
                    ((member last-button *resize*)
                     (let ((new-w (max 1 (- root-x (drawable-x event-window))))
                           (new-h (max 1 (- root-y (drawable-y event-window)))))
                       (setf (drawable-width event-window) new-w
                             (drawable-height event-window) new-h)))))
             (:button-release ()
                              (ungrab-pointer display))
             (:configure-notify () t)
             (:exposure () t))))
      (dolist (button (list *move* *resize* *lower* *raise*))
        (mapcar #'(lambda (b)
                    (ungrab-button root b :modifiers *mods*))
                button))
      (close-display display))))

(main)
