(ql:quickload :ftw)
(defpackage pong
  (:use :cl :ftw :cffi))
(in-package :pong)

(defparameter *client-w* 500)
(defparameter *client-h* 400)

(defparameter *paddle-w* 10)
(defparameter *paddle-h* 50)
(defparameter *paddle-h/2* (floor *paddle-h* 2))

(defparameter *paddle-p* nil)
(defparameter *paddle-e* nil)
(defparameter *ball* nil)
(defparameter *paddles* nil)
(defvar *keystate* nil)
(defvar *game-start* nil)
(defvar *score-p* 0)
(defvar *score-e* 0)
(defvar *test-img* nil)

(defstruct keystate
  (up nil)
  (down nil))

(defstruct paddle
  (x1 0)
  (x2 0)
  (y1 0)
  (y2 0)
  (vy 0))

(defstruct ball
  (x 0)
  (y 0)
  (r 0)
  (spd 0);; :type short-float)
  (vx 0) ;;:type short-float)
  (vy 0)) ;;:type short-float))

(defun load-images ()
  (setf *test-img* (load-image "./img/test.bmp" :type :bitmap
			       :flags '(:load-from-file :create-dib-section))))

(defun create-paddles ()
  (let* ((space 10)
         (px1 space)
         (px2 (+ px1 *paddle-w*))
         (py1 (- (floor *client-h* 2) (floor *paddle-h* 2)))
         (py2 (+ py1 *paddle-h*))
         (ex2 (- *client-w* space))
         (ex1 (- ex2 *paddle-w*)))
    (setf *paddle-p* (make-paddle :x1 px1 :x2 px2 :y1 py1 :y2 py2 :vy 4)
          *paddle-e* (make-paddle :x1 ex1 :x2 ex2 :y1 py1 :y2 py2 :vy 4)
          *paddles* (list *paddle-p* *paddle-e*))))

(defun create-keystate ()
  (setf *keystate* (make-keystate)))

(defun create-ball ()
  (let* ((r 10)
         (x (+ (paddle-x2 *paddle-p*) r))
         (y (floor *client-h* 2)))
    (setf *ball* (make-ball :x x :y y :r r :vx 4 :vy 4 :spd 5))))


(defun init-game ()
  (load-images)
  (setf *game-start* nil)
  (create-paddles)
  (create-keystate)
  (create-ball))


(defun gonyu (num)
  (multiple-value-bind (a b)
     (floor num 1)
   (if (>= b 0.5)
       (1+ a)
       a)))



(defun draw-test (hdc hdc2)
  (select-object hdc2 *test-img*)
  (bit-blt hdc 0 0 hdc2 0 0 :width 32 :height 32 :raster-op :srccopy))
  ;; (transparent-blt hdc 0 0 hdc2 0 0 :width-source 32
  ;; 		   :height-source 32
  ;; 		   :width-dest 32 :height-dest 32
  ;; 		   :transparent-color (encode-rgb 0 255 0)))

(defun draw-center-line (hdc)
  (let* ((w 6)
         (w/2 (floor w 2))
         (left (- (floor *client-w* 2) w/2))
         (right (+ left w))
         (h 20))
    (select-object hdc (get-stock-object :white-brush))
    (loop :for i :from 0 :by 40 :below *client-h*
      :do (rectangle hdc left i right (+ i h)))))

(defun draw-score (hdc)
  (let* ((font (create-font "Cica" :height 40))
        (hold-font (select-object hdc font)))
    (set-bk-mode hdc :transparent)
    (set-text-color hdc (encode-rgb 255 255 255))
    (text-out hdc (format nil "~d" *score-p*) 100 10)
    (text-out hdc (format nil "~d" *score-e*) 350 10)
    (select-object hdc hold-font)
    (delete-object font)))

(defun draw-paddles (hdc)
  (dolist (paddle *paddles*)
    (with-slots (x1 x2 y1 y2) paddle
      (select-object hdc (get-stock-object :white-brush))
      (rectangle hdc x1 y1 x2 y2))))

(defun draw-ball (hdc)
  (with-slots (x y r) *ball*
    (select-object hdc (get-stock-object :white-brush))
    (ellipse hdc (- x r) (- y r) (+ x r) (+ y r))))


(defun update-paddle-e ()
  (with-slots (x y r) *ball*
    (with-slots (x1 x2 y1 y2 vy) *paddle-e*
      (let ((paddle-mid (+ y1 (floor *paddle-h* 2))))
        (cond
          ((and (>= paddle-mid y)
                (> y1 0))
           (decf y1 vy))
          ((and (< paddle-mid y)
                (< (+ y1 *paddle-h*) *client-h*))
           (incf y1 vy)))
        (setf y2 (+ y1 *paddle-h*))))))

(defun update-paddle-p ()
  (with-slots (up down) *keystate*
    (with-slots (x1 x2 y1 y2 vy) *paddle-p*
      (when up
        (decf y1 vy))
      (when down
        (incf y1 vy))
      (let ((y2temp (+ y1 *paddle-h*)))
        (cond
          ((> 0 y1)
           (setf y1 0
                 y2 (+ y1 *paddle-h*)))
          ((> y2temp *client-h*)
           (setf y2 *client-h*
                 y1 (- y2 *paddle-h*)))
          (t (setf y2 (+ y1 *paddle-h*))))))))



(defun ball-collide-window ()
  (with-slots (x y r vx vy) *ball*
    (cond
      ((> (+ y r) *client-h*)
       (setf y (- *client-h* r)
             vy (- vy)))
      ((> 0 (- y r))
       (setf y r
             vy (- vy)))
      ((> (+ x r) *client-w*)
       (incf *score-p*)
       (init-game))
      ((> 0 (- x r))
       (incf *score-e*)
       (init-game)))))


(defun change-ball-angle (paddle)
  (with-slots (x y r vy vx spd) *ball*
    ;;(declare (short-float vy vx spd))
    (with-slots (x1 x2 y1 y2) paddle
      (let* ((paddle-mid (+ y1 *paddle-h/2*))
             (dist (abs (- paddle-mid y)))
             (hoge (max (* (/ dist *paddle-h/2*) 70) 30))
             (rad (* hoge (/ pi 180)))
             (vy1 (gonyu (* spd (sin rad))))
             (vx1 (gonyu (* spd (cos rad)))))
        ;;(declare (short-float rad vx1 vy1 hoge))
        (when (>= paddle-mid y)
          (setf vy1 (- vy1)))
        (when (> vx 0)
          (setf vx1 (- vx1)))
        (setf vy vy1
              vx vx1)
        (when (> r spd)
          (incf spd 0.1))))))


(defun ball-collide-paddle-p ()
  (with-slots (x y r vx vy) *ball*
    (with-slots (x1 x2 y1 y2) *paddle-p*
      (when (and (>= y2 y y1)
                 (>= (+ x2 r) x x1))
        (change-ball-angle *paddle-p*)
        (setf x (+ x2 r))))))

(defun ball-collide-paddle-e ()
  (with-slots (x y r vx vy) *ball*
    (with-slots (x1 x2 y1 y2) *paddle-e*
      (when (and (>= y2 y y1)
                 (>= x2 x (- x1 r)))
        (change-ball-angle *paddle-e*)
        (setf x (- x1 r))))))


(defun update-ball ()
  (with-slots (x y r vx vy) *ball*
    (incf x vx)
    (incf y vy)
    (ball-collide-window)
    (ball-collide-paddle-p)
    (ball-collide-paddle-e)))


(defun update-game ()
  (when *game-start*
    (update-paddle-p)
    (update-paddle-e)
    (update-ball)))


(defun init-client-rect (hwnd)
  (let* ((rw (get-window-rect hwnd))
         (rc (get-client-rect hwnd))
         (new-w (+ *client-w*
                   (- (- (rect-right rw) (rect-left rw))
                      (- (rect-right rc) (rect-left rc)))))
         (new-h (+ *client-h*
                   (- (- (rect-bottom rw) (rect-top rw))
                      (- (rect-bottom rc) (rect-top rc))))))
    (set-window-pos hwnd nil 0 0 new-w new-h '(:no-move))))

(defun pong-create (hwnd)
  (init-client-rect hwnd)
  (init-game)
  (set-timer :hwnd hwnd
             :elapse 30
             :replace-timer 1))


(defmacro with-double-buffering-2 ((var hwnd) &body body)
  "Evaluate body in a WITH-PAINT context where VAR is bound to an in-memory HDC
which is blitted onto the hwnd's DC as the final step. This prevents flickering 
when drawing lots of small items on the screen."
  (alexandria:with-gensyms (gbm gold gwidth gheight ghdc gps)
    `(with-paint (,hwnd ,ghdc ,gps)
       (let ((,gwidth (rect-right (paintstruct-paint ,gps)))
	     (,gheight (rect-bottom (paintstruct-paint ,gps))))
	 (with-compatible-dc (,var ,ghdc)
	   (let* ((,gbm (create-compatible-bitmap ,ghdc ,gwidth ,gheight))
		  (,gold (select-object ,var ,gbm)))
	     (unwind-protect (progn ,@body)	     
	       (transparent-blt ,ghdc 0 0 ,var 0 0 
			:width-dest ,gwidth
			:height-dest ,gheight
			:width-source ,gwidth
			:height-source ,gheight
			:transparent-color (encode-rgb 0 255 0))	     
	       (select-object ,var ,gold)
	       (delete-object ,gbm))))))))

(defun pong-paint (hwnd)
  (with-double-buffering-2 (hdc hwnd)
    (draw-center-line hdc)
    (draw-score hdc)
    (draw-paddles hdc)
    (draw-ball hdc)
    (with-compatible-dc (hmemdc hdc)
      (draw-test hdc hmemdc))))

(defun pong-timer (hwnd)
  (update-game)
  (invalidate-rect hwnd nil t)
  )

(defun pong-keydown (hwnd wparam)
  (with-slots (up down) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
        (:up (setf up t))
        (:down (setf down t))
        (:keyz (setf *game-start* t))
        (:keyq (destroy-window hwnd))))))

(defun pong-keyup (wparam)
  (with-slots (up down) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
        (:up (setf up nil))
        (:down (setf down nil))))))


(defwndproc pong-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (pong-create hwnd))
    ((const +wm-paint+)
     (pong-paint hwnd))
    ((const +wm-timer+)
     (pong-timer hwnd))
    ((const +wm-keydown+)
     (pong-keydown hwnd wparam))
    ((const +wm-keyup+)
     (pong-keyup wparam))
    ((const +wm-destroy+)
     (delete-object *test-img*)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))



(defun pong ()
  (register-class "PONG" (callback pong-wndproc)
                  :cursor (load-cursor :arrow)
                  :background (get-stock-object :black-brush))
  (let ((hwnd (create-window "PONG"
                             :window-name "Pong"
                             :ex-styles (logior-consts +ws-ex-composited+)
                             :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                             :x 1300 :y 100 :width 400 :height 300))
        (msg (make-msg)))
    (show-window hwnd)
    (update-window hwnd)
    (do ((done nil))
        (done)
      (let ((r (get-message msg)))
        (cond
          ((zerop r) (setf done t))
          (t
           (translate-message msg)
           (dispatch-message msg)))))))
