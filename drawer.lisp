(in-package :gol)

;;; draws the gaming grid
;;; TODO: continue grid to follow world x,y (give it a center and calculate 
;;; from 0,0)
(defun draw-grid ()
  (gl:color .1 .1 .1)
  (gl:push-matrix)
  (gl:translate 0 0 0)
  (let ((x-start (- (/ *config-graphics-window-x* 2)))
        (y-start (- (/ *config-graphics-window-y* 2))))
    (dotimes (y (ceiling (/ *config-grid-y* 2)))
      (let ((y (* y *config-grid-res*)))
        (gl:begin :lines)
        (gl:vertex x-start y)
        (gl:vertex *config-graphics-window-y* y)
        (gl:end)
        (gl:begin :lines)
        (gl:vertex x-start (- y))
        (gl:vertex *config-graphics-window-y* (- y))
        (gl:end)))
    (dotimes (x (ceiling (/ *config-grid-x* 2)))
      (let ((x (* x *config-grid-res*)))
        (gl:begin :lines)
        (gl:vertex x y-start)
        (gl:vertex x *config-graphics-window-y*)
        (gl:end)
        (gl:begin :lines)
        (gl:vertex (- x) y-start)
        (gl:vertex (- x) *config-graphics-window-y*)
        (gl:end)))
    (gl:pop-matrix)))

;; draws a life form at an x/y coordinate
(defun draw-life (x y)
  (gl:color .2 .8 .2)
  (gl:push-matrix)
  (let ((x (* x *config-grid-res*))
        (y (* y *config-grid-res*))
        (res *config-grid-res*))
    (gl:translate x y 0)
    (gl:begin :polygon)
    (gl:vertex 0 0)
    (gl:vertex res 0)
    (gl:vertex res res)
    (gl:vertex 0 res)
    (gl:end))
  (gl:pop-matrix))





