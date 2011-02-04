(in-package :gol)

;;; main world class, inits graphics and keeps track of life
(defclass world ()
  ((paused :accessor world-paused :initform t)
   (life :accessor world-life :initform nil)
   (view-x :accessor world-view-x :initform 0)
   (view-y :accessor world-view-y :initform 0)
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
    (sdl:enable-key-repeat 200 30)
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

;;; main GOL "algorithm" ...if a slot is switched on (alive), it only stays
;;; alive if it has either two or three neighbors. if a slot is empty, it can
;;; only be turned on if it has exactly 3 neighbors.
(defparameter *gol-algorithm-original*
  (lambda (life)
    ;; create an initial lookup table (populated below) and a next-gen container
    ;; for storing all life who made it past this gen. we also init 
    ;; "slots-to-check" which will be populated with actual slots we want to
    ;; iterate over check calculating the next generation (as opposed to 
    ;; looping over the entire grid slot by slot)
    (let ((grid-lookup (make-hash-table :size (length life)))
          (slots-to-check (make-hash-table :test #'equal :size (* 8 (length life))))
          (next-gen nil))
      ;; create helper functions to make the real processing a lot easier. the
      ;; names should hopefully be self-explanatory
      (labels ((get-xy-hash-key (x y) (intern (format nil "~dx~d" x y)))
               (get-life-hash-key (life) (get-xy-hash-key (nth 0 life) (nth 1 life)))
               (life-exists-here (x y lookup) (if (gethash (get-xy-hash-key x y) lookup) t nil))
               (get-surrounding-slots (x y)
                 `((,(1- x) ,(1- y)) (,x ,(1- y)) (,(1+ x) ,(1- y))
                  (,(1- x) ,y) (,(1+ x) ,y)
                  (,(1- x) ,(1+ y)) (,x ,(1+ y)) (,(1+ x) ,(1+ y))))
               (count-surrounding-life (x y lookup)
                 ;; create a list containing the x/y coords to check for life,
                 ;; and an initial count of how many life forms
                 (let ((surrounding (get-surrounding-slots x y))
                       (life-count 0))
                   ;; loop over our surrounding coordinates list and check each
                   ;; point for life, increasing life-count if it's found
                   (dolist (pair surrounding)
                     (let ((x (nth 0 pair))
                           (y (nth 1 pair)))
                       (when (life-exists-here x y lookup)
                         (incf life-count))))
                   life-count)))
        ;; populate our (empty) hash lookup such that active life forms can be
        ;; looked up by specific x,y coordinates.
        (dolist (l life)
          (let ((x (nth 0 l))
                (y (nth 1 l)))
            ;; loop over slots that may be affected and add them to our
            ;; makeshift "set"
            (dolist (slot (get-surrounding-slots x y))
              (setf (gethash `(,(nth 0 slot) ,(nth 1 slot)) slots-to-check) 1))
            (setf (gethash (get-life-hash-key l) grid-lookup) l)))
        ;; loop over any slots that may possibly be affected during processing
        (loop for pair being the hash-keys in slots-to-check using (hash-value v) do
          (let ((x (nth 0 pair))
                (y (nth 1 pair)))
            (when (let ((life-here (life-exists-here x y grid-lookup))
                        (surrounding-count (count-surrounding-life x y grid-lookup)))
                    (if life-here
                        (if (<= 2 surrounding-count 3) t nil)
                        (if (eql surrounding-count 3) t nil)))
              (push `(,x ,y) next-gen)))))
      next-gen)))

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
  (setf (world-life w) (funcall fn (world-life w))))


;;; There are no carbon-bnased life forms in this sector (not after calling this, anyway)
(defmethod world-reset ((w world))
  (setf (world-paused w) t)
  (setf (world-view-x w) 0)
  (setf (world-view-y w) 0)
  (setf (world-life w) nil))

;;; step the world! runs the physics simulation and processes any pre/post sim
;;; callbacks.
(defmethod world-step ((w world))
  ;; clear the screen
  (gl:clear :color-buffer :depth-buffer)

  ;; draw our boxy friends
  (dolist (life (world-life w))
    (let ((x (nth 0 life))
          (y (nth 1 life)))
      (draw-life x y)))
  ;; make our life run
  (unless (world-paused w)
    (world-process-life w *gol-algorithm-original*))
  ;; draw the grid
  (draw-grid)
  ;; center at 0,0 instead of x / 2, y / 2
  (gl:load-identity)
  (let ((x (+ (/ *config-graphics-window-x* 2) (world-view-x w)))
        (y (+ (/ *config-graphics-window-y* 2) (world-view-y w)))
        (z 0))
    (gl:translate x y z))
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
      (when (sdl:key= key :sdl-key-left)
        (incf (world-view-x w) 3))
      (when (sdl:key= key :sdl-key-right)
        (decf (world-view-x w) 3))
      (when (sdl:key= key :sdl-key-up)
        (decf (world-view-y w) 3))
      (when (sdl:key= key :sdl-key-down)
        (incf (world-view-y w) 3))
      (when (sdl:key= key :sdl-key-minus)
        (when (> *config-grid-res* 2)
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
    ;(:key-up-event (:key key))
    (:mouse-button-down-event (:button button :x x :y y)
      (process-mouse-action w button x y))
    (:mouse-motion-event (:state button :x x :y y)
      (unless (eql button 0)
        (process-mouse-action w button x y)))
    ;; when not processing events, step the world
	  (:idle ()
      (world-step w))))

(defun process-mouse-action (w button x y)
  (let ((x (truncate (/ (- (- x *config-grid-res*) (/ *config-graphics-window-x* 2)) *config-grid-res*)))
        (y (truncate (/ (- (- *config-graphics-window-y* (+ y *config-grid-res*)) (/ *config-graphics-window-y* 2)) *config-grid-res*))))
    (cond ((eql button 1)
           (world-add-life w x y))
          ((>= button 3)
           (world-remove-life w x y)))))

;;; the sky is falling...free all memory and quit. 
(defmethod world-end ((w world))
  (sdl:quit-sdl))



