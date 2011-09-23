#!/usr/local/bin/sbcl --script
;;; Most simple window manager on earth. It is a fork from the lisp
;;; version of tinywm.

;; Load CLX and make a package
(require 'asdf)
(asdf:load-system :clx)
(defpackage :most.simple.wm
  (:use :common-lisp :xlib :sb-ext))
(in-package :most.simple.wm)

;; Global parameters
(defparameter *prefix* '((:control) . #\t))
(defparameter *mouse-mod* '(:mod-1) "Modifier for mouse control")
(defparameter *move* 1 "Mouse button to move a window")
(defparameter *resize* 3 "Mouse button to resize a window")
(defparameter *term* #\c "Key to launch a terminal")
(defparameter *web* #\w "Key to launch web browser")
(defparameter *quit* #\q "Key to quit")
(defparameter *circulate* #\n "Key to circulate windows")
(defparameter *hide* #\h)

(defun mods (x) (if (consp x) (car x) 0))
(defun key (x) (if (consp x) (cdr x) x))

(defun main ()
  ;; XXX only parse DISPLAY in ":<display num>" form
  (let* ((display (open-display "" :display (parse-integer 
                                             (posix-getenv "DISPLAY") :start 1)))
         (screen (first (display-roots display)))
         (root (screen-root screen))
         (kwin (create-window :parent root :x 0 :y 0 :width 1 :height 1)))

    ;; Grab mouse buttons
    (dolist (button (list *move* *resize*))
      (grab-button root button '(:button-press) :modifiers *mouse-mod*))

    ;; Grab prefix and key shortcuts on root
    (dolist (key (list *prefix* *term* *web* *quit* *circulate* *hide*))
      (let ((code (keysym->keycodes display (car (character->keysyms (key key))))))
        (grab-key root code :modifiers (mods key))))

    (unwind-protect
        (let (last-button last-x last-y hidden processing-key-p)
          (loop named eventloop do
            (event-case 
             (display :discard-p t)
             ;; for key-press and key-release, code is the keycode
             ;; for button-press and button-release, code is the button number
             (:key-press
              (code state window)
              (cond (processing-key-p
                     (cond ((char= (keycode->character display code 0) (key *term*))
                            (sb-ext:run-program "xterm" nil :wait nil :search t))
                           ((char= (keycode->character display code 0) (key *web*))
                            (sb-ext:run-program "xxxterm" nil :wait nil :search t))
                           ((char= (keycode->character display code 0) (key *circulate*))
                            (circulate-window-down root))
                           ((char= (keycode->character display code 0) (key *hide*))
                            (cond (hidden
                                   (mapc #'(lambda (w) (map-window w)) hidden)
                                   (setf hidden nil))
                                  (t
                                   (setf hidden (loop for w in (query-tree root)
                                                   when (eql (window-map-state w) :viewable)
                                                   collect w))
                                   (mapc #'(lambda (w) (unmap-window w)) hidden))))
                           ((char= (keycode->character display code 0) (key *quit*))
                            (return-from eventloop)))
                     (setf processing-key-p nil))
                    ((char= (keycode->character display code 0) (key *prefix*))
                     (setf processing-key-p t))
                    (t (princ window)
                       (terpri)
                       (send-event :pointer-window :key-press
                                   (make-event-mask :key-press)
                                   :display display
                                   :root root
                                   :code code
                                   :state state))))
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
        (ungrab-button root button :modifiers *mouse-mod*))
      (dolist (key (list *prefix* *term* *web* *quit* *circulate* *hide*))
        (let ((code (keysym->keycodes display (car (character->keysyms (key key))))))
          (ungrab-key root code :modifiers (mods key))))
      (close-display display))))

(main)
