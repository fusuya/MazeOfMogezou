(in-package :mazeofmogezou)

;;マップデータをクリア
(defun clear-mapdate (donjon)
  (with-slots (blocks enemies field items path yuka stop-list walls door) donjon
    (setf blocks nil
	  walls nil
	  enemies nil
	  field nil
	  items nil
	  path nil
	  yuka nil
	  stop-list nil
	  door nil)))

;;マップを壁で埋める
(defun full-block-map (donjon)
  (with-slots (field tate yoko) donjon
    (loop for i from 0 below tate do
	 (loop for j from 0 below yoko do
	      (if (or (= i 0) (= j 0) (= i (1- tate)) (= j (1- yoko)))
		  (setf (aref field i j) +hard-block+) ;;壊せない壁
		  (setf (aref field i j) +soft-block+)))))) ;;壊せる壁

;;ボスフロア
(defun create-boss-stage (donjon)
  (with-slots (field tate yoko) donjon
    (loop for i from 0 below tate do
	 (loop for j from 0 below yoko do
	      (if (or (= i 0) (= j 0) (= i (1- tate)) (= j (1- yoko)))
		  (setf (aref field i j) 40) ;;壊せない壁
		  (setf (aref field i j) 0)))))) ;;壊せる壁

(defun rand1234 (lst lst1)
  (if (null lst1)
      lst
      (let* ((n (random (length lst1)))
             (m (nth n lst1)))
       (push m lst)
       (rand1234 lst (remove m lst1)))))

(defun rem-blocks (x y donjon dir)
  (with-slots (field) donjon
    (case dir
      (1
       (if (= 30 (aref field (- y 1) x))
	   (setf (aref field (- y 1) x) 0)))
      (2
       (if (= 30 (aref field (+ y 1) x))
	   (setf (aref field (+ y 1) x) 0)))
      (3
       (if (= 30 (aref field y (+ x 1)))
	   (setf (aref field y (+ x 1)) 0)))
      (4
       (if (= 30 (aref field y (1- x)))
	   (setf (aref field y (1- x)) 0))))))


(defun random-rem-blocks (donjon)
  (with-slots (blocks) donjon
    (when blocks
      (loop :repeat 10
	    :do (let ((blo (nth (random (length blocks)) blocks)))
		  (setf blocks
			(remove blo blocks :test #'equal)))))))


(defun recursion (y x donjon dir)
  (with-slots (field path stop-list tate yoko) donjon
  (let ((lst (rand1234 '() '(1 2 3 4)))
        (stop? t))
     (loop for i in lst do
      (case i
         (1 ;;上
           (if (< 0 (- y 2)) ;;2マス先が迷路の外か
            (cond
                ((= (aref field (- y 2) x) +soft-block+) ;;2マス先が壁
                 (setf (aref field (- y 2) x) +yuka+)
                 (setf (aref field (- y 1) x) +yuka+)
                 (push (list x (- y 2)) path)
                 (push (list x (- y 1)) path)
                 (setf stop? nil)
                 (recursion (- y 2) x donjon 1)))))
      ;;(return))
         (2 ;;下
           (if (> tate (+ y 2)) ;;2マス先が迷路の外か
            (cond
                ((= (aref field (+ y 2) x) +soft-block+)
                 (setf (aref field (+ y 2) x) +yuka+)
                 (setf (aref field (+ y 1) x) +yuka+)
                 (push (list x (+ y 2)) path)
                 (push (list x (+ y 1)) path)
                 (setf stop? nil)
                 (recursion (+ y 2) x donjon 2)))))
      ;;(return))
         (3 ;;右
           (if (> yoko (+ x 2)) ;;2マス先が迷路の外か
            (cond
                ((= (aref field y (+ x 2)) +soft-block+)
                 (setf (aref field y (+ x 2)) +yuka+)
                 (setf (aref field y (+ x 1)) +yuka+)
                 (push (list (+ x 2) y) path)
                 (push (list (+ x 1) y) path)
                 (setf stop? nil)
                 (recursion y (+ x 2) donjon 3)))))
      ;;(return))
         (4 ;;左
           (if (< 0 (- x 2)) ;;2マス先が迷路の外か
            (cond
                ((= (aref field y (- x 2)) +soft-block+)
                 (setf (aref field y (- x 2)) +yuka+)
                 (setf (aref field y (- x 1)) +yuka+)
                 (push (list (- x 2) y) path)
                 (push (list (- x 1) y) path)
                 (setf stop? nil)
                 (recursion y (- x 2) donjon 4)))))))
    (if stop? ;;行き止まりだったら
     (progn
    ;;(scr-format "y=~d x=~d~%" y x);;テスト用
        (rem-blocks x y donjon dir)
        (push (list x y) stop-list) ;;行き止まりの座標リスト
        ;;(setf (aref (donjon-map map) y x) 3)
	)))))

;;numとは異なるlen内の乱数を返す((diff-num 0 1)だと無限ループになる)
(defun diff-num (num len)
  (let ((hoge (random len)))
    (if (= hoge num)
	(diff-num num len)
	hoge)))

;;マップに鍵とドアをセット
(defun set-key-door (donjon)
  (with-slots (stop-list field stage enemies lastfloor) donjon
    (let* ((len (length stop-list))
	   (k (random len));; (b (diff-num k len))
	   (pos (nth k stop-list))
	   (obj (if (>= stage lastfloor) +orb+ +door+)))
      (setf (aref field (cadr pos) (car pos)) obj
	    stop-list  (remove pos stop-list :test #'equal))
      ;;宝箱
      ;;(when (> 30 stage )
      (loop :for pos :in stop-list
      	    :when (>= 6 (random 10))
      	      :do (push (make-instance 'chest :posx (* (car pos) *obj-w*) :atk-pos-x 0 :atk-pos-y 0
      					      :posy (* (cadr pos) *obj-h*) :pos pos :level 15
      					      :moto-w *obj-w* :moto-h *obj-h* :hp 1 :maxhp 1
      					      :w *obj-w* :h *obj-h* :w/2 (floor *obj-w* 2) :h/2 (floor *obj-h* 2)
      					      :obj-type :chest :img-h +chest+ :img 0)
      			enemies))
    )))

;;確率部分をnum以上のモノを1下げる
(defun rate-decf (num rate-lst)
  (loop :for i :in rate-lst
     :collect (if (> (cdr i) num)
		  (cons (car i) (decf (cdr i)))
		  i)))

;;出現率調整
(defun adjust-enemy-rate ()
  (with-slots (donjon) *game*
    (setf (enemy-rate donjon)
	  (rate-decf 30 (enemy-rate donjon)))))

;;出現する敵 階層によって出現率を変える
(defun appear-enemy ()
  (case (weightpick *enemy-appear-rate*)
    (:slime (make-instance 'slime))
    (:orc (make-instance 'orc))
    (:bubble (make-instance 'bubble))
    (:skeleton (make-instance 'skeleton))
    (:hydra (make-instance 'hydra))
    (:brigand (make-instance 'brigand))
    (:dragon (make-instance 'dragon))
    (:yote1 (make-instance 'yote1))))


;;
(defun random-enemy ()
  (case (random 6)
    (1 :slime)
    (2 :orc)
    (3 :brigand)
    (4 :hydra)
    (5 :dragon)
    (0 :yote1)))


(defmethod set-enemy-status ((e slime) stage)
  (with-slots (level str def pos atk-spd w h moto-w moto-h
	       hp maxhp ido-spd expe w/2 h/2 img-h img) e
    (setf hp      (+ 6 (floor (random stage) 3))
	  maxhp   hp
	  level   (+ 1 (floor (random stage) 3))
	  str     level
	  def     (+ 1 (floor (random stage) 3))
	  expe    (+ 3 (floor (random stage) 3))
	  img-h   +slime-anime+)))

(defmethod set-enemy-status ((e bubble) stage)
  (with-slots (level str def pos atk-spd w h moto-w moto-h
	       hp maxhp ido-spd expe w/2 h/2 img-h img) e
    (setf hp      (+ 2 (floor (random stage) 2))
	  maxhp   hp
	  level   (+ 1 (floor (random stage) 2))
	  str     level
	  def     (+ 2 (floor (random stage) 2))
	  expe    (+ 4 (floor (random stage) 3))
	  img-h   +bubble-anime+)))

(defmethod set-enemy-status ((e skeleton) stage)
  (with-slots (level str def pos atk-spd w h moto-w moto-h
	       hp maxhp ido-spd expe w/2 h/2 img-h img) e
    (setf hp      (+ 5 (floor (random stage) 2))
	  maxhp   hp
	  level   (+ 4 (floor (random stage) 2))
	  str     level
	  def     (+ 1 (floor (random stage) 3))
	  expe    (+ 5 (floor (random stage) 3))
	  img-h   +skeleton-anime+)))

(defmethod set-enemy-status ((e orc) stage)
  (with-slots (level str def pos atk-spd w h moto-w moto-h
	       hp maxhp ido-spd expe w/2 h/2 img-h img) e
    (setf hp      (+ 10 (floor (random stage) 2))
	  maxhp   hp
	  level   (+ 4 (floor (random stage) 2))
	  str     level
	  def     (+ 1 (floor (random stage) 2))
	  expe    (+ 5 (floor (random stage) 3))
	  img-h   +orc-anime+)))

(defmethod set-enemy-status ((e brigand) stage)
  (with-slots (level str def pos atk-spd w h moto-w moto-h
	       hp maxhp ido-spd expe w/2 h/2 img-h img range sight) e
    (setf hp      (+ 6 (floor (random stage) 1.2))
	  maxhp   hp
	  level   (+ 2 (floor (random stage) 2))
	  str     level
	  def     (+ 2 (floor (random stage) 2))
	  expe    (+ 7 (floor (random stage) 2))
	  range   (+ 2 (random 7))
	  sight   (+ 1 (random 5))
	  img-h   +brigand-anime+)))

(defmethod set-enemy-status ((e hydra) stage)
  (with-slots (level str def pos atk-spd w h moto-w moto-h
	       hp maxhp ido-spd expe w/2 h/2 img-h img) e
    (setf hp      (+ 12 (floor (random stage) 1))
	  maxhp   hp
	  level   (+ 2 (floor (random stage) 2))
	  str     level
	  def     (+ 5 (floor (random stage) 1.3))
	  expe    (+ 10 (floor (random stage) 2))
	  img-h   +hydra-anime+)))

(defmethod set-enemy-status ((e dragon) stage)
  (with-slots (level str def pos atk-spd w h moto-w moto-h
	       hp maxhp ido-spd expe w/2 h/2 img-h img range) e
    (setf hp      (+ 20 (floor (random stage) 1.4))
	  maxhp   hp
	  level   (+ 5 (floor (random stage) 1.3))
	  str     level
	  def     (+ 6 (floor (random stage) 1.3))
	  expe    (+ 20 (floor (random stage) 2))
	  range   (+ 4 (random 7))
	  img-h   +dragon-anime+)))

(defmethod set-enemy-status ((e yote1) stage)
  (declare (ignore stage))
  (with-slots (level str def pos atk-spd w h moto-w moto-h
	       hp maxhp ido-spd expe w/2 h/2 img-h img) e
    (setf hp      3
	  maxhp   hp
	  level   3)
	  str     level
	  def     50
	  expe    300
	  img-h   +yote-anime+))

;;敵生成
(defun create-enemy (e-pos donjon)
  (let ((e (appear-enemy)))
    (with-slots (x y dir img pos posx posy sight) e
      (setf x (car e-pos)
	    posx (* x *obj-w*)
	    y (cadr e-pos)
	    posy (* y *obj-h*)
	    pos e-pos
	    dir +down+
	    sight (+ 2 (random 6))
	    img 1)
      (set-enemy-status e (stage donjon))
      e)))
  


;;敵を配置する
(defun set-enemies (donjon)
  (with-slots (path enemies) donjon
    (let ((enemy-num (+ 3 (random (+ 3 (floor (stage donjon) 5)))))) ;;1フロアに出る敵の数
      (loop for i from 0 to enemy-num do
	   (let* ((e-pos (nth (random (length path)) path))
		  (e (create-enemy e-pos donjon)))
	     (push e enemies)
	     (setf path
		   (remove e-pos path :test #'equal)))))))

;;ボス配置
;; (defun set-boss (donjon)
;;   (let* ((e-pos (list (floor (yoko donjon) 2) 1))
;; 	 (boss (make-instance 'enemy :x (* (car e-pos) *blo-w46*)
;; 			      :y (* (cadr e-pos) *blo-h46*)
;; 			      :moto-w 64 :moto-h 64
;; 			      :str (+ 10 (floor (level *p*) 2))
;; 			      :def (+ 20 (floor (level *p*) 5))
;; 			      :hp (+ 90 (level *p*))
;; 			      :maxhp (+ 90 (* (level *p*) 2))
;; 			      :ido-spd 2 :expe 0
;; 			      :w 64 :h 64 :atk-spd 80
;; 			      :w/2 32 :h/2 32
;; 			      :obj-type :boss
;; 			      :img 1)))
;;     (push boss (enemies donjon))))

;;マップ設定
(defun set-map (donjon moto)
  (setf (tate donjon) 11
        (yoko donjon) 11)
  (loop for i from 0 below (tate donjon) do
       (loop for j from 0 below (yoko donjon) do
	    (setf (aref (field donjon) i j) (aref moto i j)))))



;;おbジェクトの位置設定
(defun set-obj-info (donjon)
  (with-slots (tate yoko field walls blocks yuka items door) donjon
    (loop for y from 0 below tate do
	 (loop for x from 0 below yoko do
	      (let* ((obj-num (aref field y x)))
		(cond
		  ((= +yuka+ obj-num)
		   (push (list x y) yuka))
		  
		  ((= +hard-block+ obj-num)
		   (push (list x y) walls))
		  ((= +soft-block+ obj-num)
		   (if (> (random 7) 1)
		       (push (list x y) blocks)
		       (progn (push (list x y) yuka)
			      (setf (aref field y x) 0))))
		  ((= +door+ obj-num)
		   (push (list x y) yuka)
		   (setf door (make-instance 'obj :pos (list x y) :img obj-num)))
		  (t
		   (push (list x y) yuka)
		   (push
		    (make-instance 'obj :pos (list x y) :img obj-num)
		    items))))))))


;;迷路マップ生成
(defun create-maze ()
  (with-slots (donjon p) *game*
  (with-slots (field tate yoko stop-list path) donjon
    (let* ((x 0)
	   (startx 0)
	   (y 0)
	   (starty 0))
      (clear-mapdate donjon)
      (setf field (make-array (list tate yoko)));;マップ配列作成
      (full-block-map donjon) ;;マップをブロックで埋める
      (cond
	((= (stage p) 30) ;; 100階は固定マップ
	 (create-boss-stage donjon)
	 (setf (pos p) (list (floor yoko 2) (- tate 2))))
	;;(set-boss map))
	(t
	 ;;奇数座標を初期位置にする
	 (setf x (random (floor yoko 2))
	       y (random (floor tate 2))
	       startx (+ (* x 2) 1)
	       starty (+ (* y 2) 1))
	 (setf (aref field starty startx) +yuka+) ;;初期位置を通路にする
	 (recursion starty startx donjon 0) ;;迷路生成
	 (loop until (<= 2 (length stop-list))
	    do
	    ;; 行き止まりが 1 つしか無かったのでやりなおし
	      (full-block-map donjon)
	      (setf stop-list nil)
	      (setf (aref field starty startx) +yuka+)
	      (recursion starty startx donjon +yuka+))
	 ;;(setf (aref (donjon-map map) starty startx) 1) ;;主人公の位置
	 (setf (pos p) (list  startx  starty) ;;初期位置
	       (x p) (car (pos p))
	       (posx p) (* (x p) *obj-w*)
	       (y p) (cadr (pos p))
	       (posy p) (* (y p) *obj-h*))
	 ;;パーティーの位置に敵を配置しないようにする
	 (setf (path donjon) (remove (pos p) path :test #'equal))
	 ;;(random-rem-blocks donjon)
	 (set-enemies  donjon) ;;敵を配置
	 (set-key-door donjon) ;;鍵とドアを配置
	 (set-obj-info donjon))))
          ;;(d-map-map mapn)))
          ;;(test-show-map (d-map-map mapn))))
	 )))
