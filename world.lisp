(in-package :gol)

;;; main world class, inits graphics, inits physics, tracks objects, sets the
;;; simulation, etc etc etc
(defclass world ()
  ((paused :accessor world-paused :initform nil)
   (life :accessor world-life :initform nil)
   (window :accessor world-window)))

;;; starts the world. inits SDL window/opengl, runs main game loop (world-run).
;;; this will eventually be a level up, since we'll have menus and shit which
;;; aren't technically part of the world, so all graphics initialization will
;;; not be done here...just physics and stuff. but for now, this works fine.
(defmethod world-init ((w world))
  ;; start SDL
  (sdl:with-init ()
    ;; create the main window
    (setf (world-window w)
          (sdl:window *config-graphics-window-x* *config-graphics-window-y*
                      :title-caption "A CHOIR BOY"
                      :opengl t
                      :opengl-attributes '((:sdl-gl-doublebuffer 1)
                                           (:sdl-gl-red-size 8)
                                           (:sdl-gl-green-size 8)
                                           (:sdl-gl-blue-size 8)
                                           (:sdl-gl-depth-size 16))))
    ;; set up key repeat (so we can hold a key for rapid fire)
    ;(sdl:enable-key-repeat 200 30)
    ;; who knows - figure out what this does
	  (gl:enable :texture-2d :blend)
	  (gl:blend-func :src-alpha :one-minus-src-alpha)
    ;; set up 2D rendering in opengl
	  (let ((vport (coerce (gl:get-integer :viewport) 'list)))
	    (gl:matrix-mode :projection)
	    (gl:push-matrix)
	    (gl:load-identity)
	    (gl:ortho 0 (nth 2 vport) 0 (nth 3 vport) -1 1)   ; here's what does the actual 2D
	    (gl:matrix-mode :modelview)
	    (gl:push-matrix)
	    (gl:load-identity))
    ;; disable depth-testing (not needed in 2d)
	  (gl:disable :depth-test)
	  (gl:clear-color 0 0 0 0)
    ;; run the world...this calls our game loop
	  (world-run w)))

;;; add a life form to the world (or remove)
(defmethod world-toggle-life ((w world) (x number) (y number))
  (let ((found-life nil))
    (dolist (life (world-life w))
      (when (and (eql x (nth 0 life))
                 (eql y (nth 1 life)))
        (setf found-life t)
        (return)))
    (if found-life
        (world-remove-life w x y)
        (world-add-life w x y))))

(defmethod world-add-life ((w world) (x number) (y number))
  (push (list x y) (world-life w)))

(defmethod world-remove-life ((w world) (x number) (y number))
  (setf (world-life w) (remove-if #'(lambda (life)
                                      (if (and (eql x (nth 0 life))
                                               (eql y (nth 1 life)))
                                          t
                                          nil))
                                  (world-life w))))


(defmethod world-process-life ((w world)))

;;; step the world! runs the physics simulation and processes any pre/post sim
;;; callbacks.
(defmethod world-step ((w world))
  ;; clear the screen
  (gl:clear :color-buffer :depth-buffer)

  ;; draw our boxy friends
  (dolist (l (world-life w)) (draw-life l))

  ;; make our life run
  (unless (world-paused w)
    (world-process-life w))

  ;; draw the grid
  (draw-grid)

  ;; refresh the display (aka show all the changes we just made)
  (sdl:update-display))

;;; run the world...this is the main game loop, and includes event handling.
(defmethod world-run ((w world))
  ;; poll our events
  (sdl:with-events (:poll)
	  (:quit-event () t)
	  (:video-expose-event () (sdl:update-display))
    ;; process keyboard input
	  (:key-down-event (:key key)
      (when (sdl:key= key :sdl-key-minus)
        (if (> *config-graphics-grid-res* 0)
          (decf *config-graphics-grid-res* 1)))
      (when (sdl:key= key :sdl-key-equals)
        (incf *config-graphics-grid-res* 1))
      (when (sdl:key= key :sdl-key-r)
            ;; remove all life
            )
	  	(when (sdl:key= key :sdl-key-q)
	  	      (sdl:push-quit-event))
	  	(when (sdl:key= key :sdl-key-escape)
	  	      (sdl:push-quit-event)))
    (:key-up-event (:key key))
    (:mouse-button-down-event (:button button :state state :x x :y y)
      ;; add life in this sector =]
      (when (eql button 1)
        (let ((x (truncate (/ x *config-graphics-grid-res*)))
              (y (truncate (/ (- *config-graphics-window-y* y) *config-graphics-grid-res*))))
          (world-toggle-life w x y))))
    (:mouse-motion-event (:state button :x x :y y :x-rel x-rel :y-rel y-rel)
      (when (eql button 1)
        (let ((x (truncate (/ x *config-graphics-grid-res*)))
              (y (truncate (/ (- *config-graphics-window-y* y) *config-graphics-grid-res*))))
          (world-add-life w x y))))
    ;; when not processing events, step the world
	  (:idle ()
      (world-step w))))

;;; the sky is falling...free all memory and quit. 
(defmethod world-end ((w world))
  (sdl:quit-sdl))



