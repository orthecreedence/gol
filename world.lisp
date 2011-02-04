(in-package :gol)

;;; main GOL "algorithm"
(defparameter *world-algorithms-original* 
  (lambda (is-alive life-count)
    (if is-alive
      (if (<= 2 life-count 3) t nil)
      (if (eql life-count 3) t nil))))

;;; main world class, inits graphics and keeps track of life
(defclass world ()
  ((paused :accessor world-paused :initform nil)
   (life :accessor world-life :initform nil)
   (window :accessor world-window)))

;;; create the main SDL window and context blah blah, and init opengl. once our
;;; graphical pals are set up, just to the main loop (world-run)
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
	  (gl:clear-color .04 .04 .04 0)
    ;; run the world...this calls our game loop
	  (world-run w)))

;;; toggle life at a given grid point
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

;;; add life at a given grid point
(defmethod world-add-life ((w world) (x number) (y number))
  (push (list x y) (world-life w)))

;;; remove life at a given grid point
(defmethod world-remove-life ((w world) (x number) (y number))
  (setf (world-life w) (remove-if #'(lambda (life)
                                      (if (and (eql x (nth 0 life))
                                               (eql y (nth 1 life)))
                                          t
                                          nil))
                                  (world-life w))))

;;; process life...decide who lives and dies based on gol rules. takes a lambda
;;; which provides the actual algorithm to use
(defmethod world-process-life ((w world) fn)
  ;; create an initial lookup table (populated below) and a next-gen container
  ;; for storing all life who made it past this gen
  (let ((grid-lookup (make-hash-table))
        (next-gen nil))
    ;; create helper functions to make the real processing a lot easier
    (labels ((get-xy-hash-key (x y) (intern (format nil "~dx~d" x y)))
             (get-life-hash-key (life) (get-xy-hash-key (nth 0 life) (nth 1 life)))
             (life-exists-here (x y lookup) (if (gethash (get-xy-hash-key x y) lookup) t nil))
             (count-surrounding-life (x y lookup)
               (let ((surrounding `((,(1- x) ,(1- y)) (,x ,(1- y)) (,(1+ x) ,(1- y))
                                    (,(1- x) ,y) (,(1+ x) ,y)
                                    (,(1- x) ,(1+ y)) (,x ,(1+ y)) (,(1+ x) ,(1+ y))))
                     (life-count 0))
                 (dolist (pair surrounding)
                   (let ((x (nth 0 pair))
                         (y (nth 1 pair)))
                     (when (life-exists-here x y lookup)
                       (incf life-count))))
                 life-count)))
      (dolist (life (world-life w))
        (setf (gethash (get-life-hash-key life) grid-lookup) life))
      ;; horibly inefficient...loop over all grid points and calculate whether to
      ;; add life, take it away, or stay the same
      ;; TODO: find an efficient way of finding grid items to look at...aka don't
      ;; brute force
      (dotimes (y *config-grid-y*)
        (dotimes (x *config-grid-x*)
          (let ((life-here (life-exists-here x y grid-lookup))
                (surrounding-count (count-surrounding-life x y grid-lookup)))
            (if (funcall fn life-here surrounding-count)
                (push `(,x ,y) next-gen))))))
                
    (setf (world-life w) next-gen)))


;;; There are no carbon-bnased life forms in this sector (not after calling this, anyway)
(defmethod world-reset ((w world))
  (setf (world-life w) nil))

;;; step the world! runs the physics simulation and processes any pre/post sim
;;; callbacks.
(defmethod world-step ((w world) &aux (dt (sdl:dt)))
  ;; clear the screen
  (gl:clear :color-buffer :depth-buffer)

  ;; draw our boxy friends
  (dolist (life (world-life w))
    (let ((x (nth 0 life))
          (y (nth 1 life)))
      (draw-life x y)))
  ;; make our life run
  (unless (world-paused w)
    (world-process-life w *world-algorithms-original*))
  ;; draw the grid
  (draw-grid)
  ;; refresh the display (aka show all the changes we just made)
  (sdl:update-display))

;; toggle pause
(defmethod world-toggle-pause ((w world))
  (if (world-paused w)
      (setf (world-paused w) nil)
      (setf (world-paused w) t)))

;;; run the world...this is the main game loop, and includes event handling.
(defmethod world-run ((w world))
  ;; poll our events
  (sdl:with-events (:poll)
	  (:quit-event () t)
	  (:video-expose-event () (sdl:update-display))
    ;; process keyboard input
	  (:key-down-event (:key key)
      (when (sdl:key= key :sdl-key-minus)
        (when (> *config-grid-res* 0)
          (decf *config-grid-res* 1)
          (recalculate-grid-coords)))
      (when (sdl:key= key :sdl-key-equals)
        (incf *config-grid-res* 1)
        (recalculate-grid-coords))
      (when (sdl:key= key :sdl-key-p)
        (world-toggle-pause w))
      (when (sdl:key= key :sdl-key-r)
        (world-reset w))
	  	(when (sdl:key= key :sdl-key-q)
	  	  (sdl:push-quit-event))
	  	(when (sdl:key= key :sdl-key-escape)
	  	  (sdl:push-quit-event)))
    (:key-up-event (:key key))
    (:mouse-button-down-event (:button button :state state :x x :y y)
      (let ((x (truncate (/ x *config-grid-res*)))
            (y (truncate (/ (- *config-graphics-window-y* y) *config-grid-res*))))
        (cond ((eql button 1)
               (world-add-life w x y))
              ((>= button 3)
               (world-remove-life w x y)))))
    (:mouse-motion-event (:state button :x x :y y :x-rel x-rel :y-rel y-rel)
      (unless (eql button 0)
        (let ((x (truncate (/ x *config-grid-res*)))
              (y (truncate (/ (- *config-graphics-window-y* y) *config-grid-res*))))
          (cond ((eql button 1)
                 (world-add-life w x y))
                ((>= button 3)
                 (world-remove-life w x y))))))
    ;; when not processing events, step the world
	  (:idle ()
      (world-step w))))

;;; the sky is falling...free all memory and quit. 
(defmethod world-end ((w world))
  (sdl:quit-sdl))



