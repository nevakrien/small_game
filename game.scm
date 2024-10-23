;; The contents of this demo file are made available under the CC0 1.0
;; Universal Public Domain Dedication. See LICENSE-CC0.txt or visit
;; http://creativecommons.org/publicdomain/zero/1.0/


;;; This is a demo program showing some basic SDL functionality.
;;;
;;; This program demonstrates the following concepts:
;;;
;;; - Initializing SDL
;;; - Creating and configuring a window
;;; - Using a record type to manage game scene objects
;;; - Creating surfaces, rects, and colors
;;; - Filling a surface with a color
;;; - Blitting one surface onto another
;;; - Updating the window surface
;;; - A basic event loop that responds to user input.
;;;
;;; Controls:
;;;
;;; - Mouse click / drag: Move Smiley 1.
;;; - Arrow keys: Move Smiley 2.
;;; - Space: Randomize Smiley colors.
;;; - V: Toggle verbose printing of events to console.
;;; - Escape, Q, or close button: Quit


(cond-expand
  (chicken-4
   (use (prefix sdl2 sdl2:)
        miscmacros))
  (chicken-5
   (import (chicken condition)
           (chicken format)
           (rename (chicken random)
                   (pseudo-random-integer random))
           (prefix sdl2 sdl2:)
           miscmacros)))


;;; Initialize the parts of SDL that we need.
(sdl2:set-main-ready!)
(sdl2:init! '(video events joystick))

;; Automatically call sdl2:quit! when program exits normally.
(on-exit sdl2:quit!)

;; Call sdl2:quit! and then call the original exception handler if an
;; unhandled exception reaches the top level.
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))


(printf "Compiled with SDL version ~A~N" (sdl2:compiled-version))
(printf "Running with SDL version ~A~N" (sdl2:current-version))
(printf "Using sdl2 egg version ~A~N" (sdl2:egg-version))



;;; Create a new window.
(define window
  (sdl2:create-window!
   "SDL Basics"                         ; title
   'centered  100                       ; x, y
   800  600                             ; w, h
   '(shown resizable)))                 ; flags

;;; Restrict the window from being made too small or too big, for no
;;; reason except to demonstrate this feature.
(set! (sdl2:window-maximum-size window) '(1024 768))
(set! (sdl2:window-minimum-size window) '(200 200))

(printf "Window position: ~A, size: ~A, max size: ~A, min size: ~A~N"
        (receive (sdl2:window-position window))
        (receive (sdl2:window-size window))
        (receive (sdl2:window-maximum-size window))
        (receive (sdl2:window-minimum-size window)))



;;; A record type for an object that has a surface and x/y coordinates
;;; representing the object's center.
(define-record-type obj
  (make-obj surface x y)
  obj?
  (surface obj-surface (setter obj-surface))
  (x       obj-x       (setter obj-x))
  (y       obj-y       (setter obj-y)))

;; Create rect the same size as obj's surface, centered on its x/y.
(define (obj-rect obj)
  (let ((w (sdl2:surface-w (obj-surface obj)))
        (h (sdl2:surface-h (obj-surface obj))))
    (sdl2:make-rect (- (obj-x obj) (/ w 2))
                   (- (obj-y obj) (/ h 2))
                   w
                   h)))

;;; Blit the obj's surface to the destination surface. The obj will be
;;; drawn centered on its x and y coordinates.
(define (draw-obj! obj dest)
  (sdl2:blit-surface! (obj-surface obj) #f dest (obj-rect obj)))

(define-record-type smiley
  (internal-make-smiley obj color)  ;; Internal constructor for the record
  smiley?
  (obj    smiley-obj    (setter smiley-obj))    ;; Field for the obj
  (color  smiley-color  (setter smiley-color))) ;; Field for the color

;; Public constructor for smiley that takes x, y, and color
(define (make-smiley x y color)
  ;; Create obj using the provided x, y, and color
  (let ((obj (make-obj (make-smile-surf color) x y))) 
    ;; Use the internal constructor to create the smiley record
    (internal-make-smiley obj color)))  ;; Call the internal constructor with obj and color


;; Helper function to change the color of the smiley, handle freeing, and setting the surface.
(define (set-smiley-color! smiley new-color)
  ;; Free the old surface
  (sdl2:free-surface! (obj-surface (smiley-obj smiley)))
  
  ;; Create a new surface with the new color
  (let ((new-surface (make-smile-surf new-color)))
    ;; Update the object's surface with the new surface
    (set! (obj-surface (smiley-obj smiley)) new-surface)
    
    ;; Update the color field of the smiley
    (set! (smiley-color smiley) new-color)))



;; Getters for x and y
(define (get-smiley-x smiley)
  (obj-x (smiley-obj smiley)))

(define (get-smiley-y smiley)
  (obj-y (smiley-obj smiley)))

;; Setters for x and y
(define (set-smiley-x! smiley value)
  (set! (obj-x (smiley-obj smiley)) value))

(define (set-smiley-y! smiley value)
  (set! (obj-y (smiley-obj smiley)) value))

;; Increment x and y
(define (inc-smiley-x! smiley amount)
  (set! (obj-x (smiley-obj smiley)) (+ (obj-x (smiley-obj smiley)) amount)))

(define (inc-smiley-y! smiley amount)
  (set! (obj-y (smiley-obj smiley)) (+ (obj-y (smiley-obj smiley)) amount)))

;; Decrement x and y
(define (dec-smiley-x! smiley amount)
  (set! (obj-x (smiley-obj smiley)) (- (obj-x (smiley-obj smiley)) amount)))

(define (dec-smiley-y! smiley amount)
  (set! (obj-y (smiley-obj smiley)) (- (obj-y (smiley-obj smiley)) amount)))

;; Reset both x and y to specific values
(define (set-smiley-xy! smiley new-x new-y)
  (set-smiley-x! smiley new-x)
  (set-smiley-y! smiley new-y))

;; Helper function to clamp a value to the 0-255 range
(define (clamp-add a b)
  (max 0 (min (+ a b) 255)))

;; Helper function to generate a random number in the range [-delta, delta]
(define (random-delta delta)
  (+ (- (random (* 2 delta)) delta) 1) )

;; Mutate the smiley's color by a random delta in the range [-delta, delta]
(define (mutate-smiley! smiley delta)
  (let* ((old-color (smiley-color smiley))  ;; Get the current color
         (new-r (clamp-add (sdl2:color-r old-color) (random-delta delta)))  ;; Mutate R component
         (new-g (clamp-add (sdl2:color-g old-color) (random-delta delta)))  ;; Mutate G component
         (new-b (clamp-add (sdl2:color-b old-color) (random-delta delta)))  ;; Mutate B component
         (new-color (sdl2:make-color new-r new-g new-b 255)))  ;; Create new color with full alpha
    ;; Set the new mutated color
    (set-smiley-color! smiley new-color)))

(define (make-random-color)
  ;; 50 is the minimum so that the color doesn't get too dark.
  (sdl2:make-color (+ 50 (random 175))
                  (+ 50 (random 175))
                  (+ 50 (random 175))
                  255))

;;; Make a new surface with a smiley face of the given color.
(define (make-smile-surf main-color)
  (let ((dest   (sdl2:make-surface 100 100 32))
        (shadow (sdl2:make-color 0 0 0 120)))
    ;; Draw the partially transparent black shadow
    (sdl2:fill-rect! dest (sdl2:make-rect 10 10 90 90) shadow)
    ;; Draw the head (using the main color)
    (sdl2:fill-rect! dest (sdl2:make-rect  0  0 90 90) main-color)
    ;; "Cut out" the eyes and mouth. Filling with a transparent color
    ;; replaces the alpha, it does not blend with the old color.
    (sdl2:fill-rects! dest
                     (list (sdl2:make-rect 25 20 10 20)
                           (sdl2:make-rect 55 20 10 20)
                           (sdl2:make-rect 15 50 10 10)
                           (sdl2:make-rect 25 60 40 10)
                           (sdl2:make-rect 65 50 10 10))
                     shadow)
    ;; Enable RLE for faster blitting.
    (sdl2:surface-rle-set! dest #t)
    dest))

;; Replace the smiley's surface and color with a new random color smiley face.
(define (randomize-smiley! smiley)
  ;; Generate a new random color
  (let ((new-color (make-random-color)))
    ;; Use the helper to set both the surface and the color
    (set-smiley-color! smiley new-color)))



;;; Create a couple smileys!
(define smiley1 (make-smiley 300 300 (make-random-color)))
(define smiley2 (make-smiley 500 300 (make-random-color)))



;;; Draw (or redraw) the entire scene. It would be more efficient to
;;; only redraw the parts of the scene that have changed, but since
;;; this is just a demo program we don't want to get too complex.
(define (draw-scene!)
  (let ((window-surf (sdl2:window-surface window)))
    ;; Clear the whole screen using a blue background color
    (sdl2:fill-rect! window-surf #f (sdl2:make-color 0 80 160))
    ;; Draw the smileys
    (draw-obj! (smiley-obj smiley2) window-surf)
    (draw-obj! (smiley-obj smiley1) window-surf)
    ;; Refresh the screen
    (sdl2:update-window-surface! window)))


;;; Simple event loop. It just repeats over and over (until the
;;; variable done is set to #t), getting a single event from SDL and
;;; then performing some actions depending on what kind of event it
;;; is. In a real program, your event loop would probably be more
;;; complex and better structured than this simple example.
(let ((done #f)
      (verbose? #f)
      (tick-interval 100)  ;; Time between ticks in milliseconds
      (last-tick-time (sdl2:get-ticks)))  ;; Record the time at which the last tick happened

  ;; Main event loop
  (while (not done)
    (let ((current-time (sdl2:get-ticks))
          (ev (sdl2:wait-event-timeout! 10)))  ;; Wait for an event, timeout in 1ms

      ;; Tick event: Check if enough time has passed since the last tick
      (when (>= (- current-time last-tick-time) tick-interval)
        ;; Trigger tick event (you can place any action here)
        (mutate-smiley! smiley1 30)
        (mutate-smiley! smiley2 20)
        (draw-scene!)  ;; Redraw scene after mutation
        (set! last-tick-time current-time))  ;; Reset the last tick time

      ;; Handle any SDL events (including mouse, keypresses, etc.)
      (when ev  ;; If there is an event, handle it
        (case (sdl2:event-type ev)
          ;; Window exposed, resized, etc.
          ((window)
           (draw-scene!))

          ;; User requested app quit (e.g., clicked the close button).
          ((quit)
           (set! done #t))

          ;; Mouse button pressed
          ((mouse-button-down)
           ;; Move smiley1 to the mouse position.
           (set-smiley-xy! smiley1
                           (sdl2:mouse-button-event-x ev)
                           (sdl2:mouse-button-event-y ev))
           (draw-scene!))

          ;; Keyboard key pressed.
          ((key-down)
           (case (sdl2:keyboard-event-sym ev)
             ((escape q)
              (set! done #t))
             ;; Space bar randomizes smiley colors
             ((space)
              (randomize-smiley! smiley1)
              (randomize-smiley! smiley2)
              (draw-scene!))

             ;; Arrow keys control smiley2
             ((left)
              (dec-smiley-x! smiley2 20)
              (draw-scene!))
             ((right)
              (inc-smiley-x! smiley2 20)
              (draw-scene!))
             ((up)
              (dec-smiley-y! smiley2 20)
              (draw-scene!))
             ((down)
              (inc-smiley-y! smiley2 20)
              (draw-scene!)))))))))
