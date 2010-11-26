#!/usr/local/bin/sbcl --script
;;; Load CLX stuff
(require 'asdf)
(asdf:oos 'asdf:load-op 'clx)
(use-package :xlib)

;;; Most simple window manager on earth (fork from the lisp version of
;;; tinywm).
(defparameter *mods* '(:mod-1))      ; :mod-4 for windows key
(defparameter *move* '(1))
(defparameter *resize* '(2))
(defparameter *lower* '(3 4))
(defparameter *raise* '(5))
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

    ;; Grab F12 to launch emacs back
    (grab-key root (keysym->keycodes display *f12*))

    (unwind-protect
        (let (last-button last-x last-y)
          (loop
            (event-case 
             (display :discard-p t)
             ;; for key-press and key-release, code is the keycode
             ;; for button-press and button-release, code is the button number
             (:key-press
              ()
              ;; Not really portable isn't it
              (sb-ext:run-program "emacs" nil :wait nil :search t))
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
                              (ungrab-pointer display)))))
      (dolist (button (list *move* *resize* *lower* *raise*))
        (mapcar #'(lambda (b)
                    (ungrab-button root b :modifiers *mods*))
                button))
      (close-display display))))

(main)
