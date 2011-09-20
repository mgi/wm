#!/usr/local/bin/sbcl --script
;;; Most simple window manager on earth. It is a fork from the lisp
;;; version of tinywm.

;; Load CLX stuff
(require 'asdf)
(asdf:load-system :clx)
(use-package :xlib)

;; Global parameters
(defparameter *mods* '(:control :mod-1) "Global modifiers")
(defparameter *move* 1 "Mouse button to move a window")
(defparameter *resize* 2 "Mouse button to resize a window")
(defparameter *term* #\Return "Key to launch a terminal")
(defparameter *web* #\w "Key to launch web browser")
(defparameter *quit* #\q "Key to quit")
(defparameter *circulate* #\Tab "Key to circulate windows")
(defparameter *hide* #\h)

(defun main ()
  (let* ((display (open-display ""))
         (screen (first (display-roots display)))
         (root (screen-root screen)))

    ;; Grab mouse buttons
    (dolist (button (list *move* *resize*))
      (grab-button root button '(:button-press) :modifiers *mods*))

    ;; Grab keyboard shortcuts
    (dolist (key (list *term* *web* *quit* *circulate* *hide*))
      (grab-key root (keysym->keycodes display (car (character->keysyms key)))
                :modifiers *mods*))

    (unwind-protect
        (let (last-button last-x last-y hidden)
          (loop named eventloop do
            (event-case 
             (display :discard-p t)
             ;; for key-press and key-release, code is the keycode
             ;; for button-press and button-release, code is the button number
             (:key-press
              (code)
              (cond ((char= (keycode->character display code 0) *term*)
                     (sb-ext:run-program "xterm" nil :wait nil :search t))
                    ((char= (keycode->character display code 0) *web*)
                     (sb-ext:run-program "xxxterm" nil :wait nil :search t))
                    ((char= (keycode->character display code 0) *circulate*)
                     (circulate-window-up root))
                    ((char= (keycode->character display code 0) *hide*)
                     (cond (hidden
                            (mapc #'(lambda (w) (map-window w)) hidden)
                            (setf hidden nil))
                           (t
                            (setf hidden (loop for w in (query-tree root)
                                              when (eql (window-map-state w) :viewable)
                                              collect w))
                            (mapc #'(lambda (w) (unmap-window w)) hidden))))
                    ((char= (keycode->character display code 0) *quit*)
                     (return-from eventloop))))
             (:button-press
              (code child)
              (when child        ; do nothing if we're not over a window
                (setf last-button code)
                (grab-pointer child '(:pointer-motion :button-release))
                (when (= code *resize*)
                  (warp-pointer child (drawable-width child) 
                                (drawable-height child)))
                (let ((lst (multiple-value-list (query-pointer root))))
                  (setf last-x (sixth lst)
                        last-y (seventh lst)))))
             (:motion-notify
              (event-window root-x root-y)
              (cond ((= last-button *move*)
                     (let ((delta-x (- root-x last-x))
                           (delta-y (- root-y last-y)))
                       (incf (drawable-x event-window) delta-x)
                       (incf (drawable-y event-window) delta-y)
                       (incf last-x delta-x)
                       (incf last-y delta-y)))
                    ((= last-button *resize*)
                     (let ((new-w (max 1 (- root-x (drawable-x event-window))))
                           (new-h (max 1 (- root-y (drawable-y event-window)))))
                       (setf (drawable-width event-window) new-w
                             (drawable-height event-window) new-h)))))
             (:button-release () (ungrab-pointer display))
             ((:configure-notify :exposure) () t))))
      (dolist (button (list *move* *resize*))
        (ungrab-button root button :modifiers *mods*))
      (dolist (key (list *term* *quit* *circulate*))
        (ungrab-key root (keysym->keycodes display key) :modifiers *mods*))
      (close-display display))))

(main)
