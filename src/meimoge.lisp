(in-package :mazeofmogezou)
;;TODO ダッシュ
;;ブラシ生成
(defun set-brush ()
  (setf *brush* (make-array 7 :initial-contents
                              (list
                                (create-solid-brush (encode-rgb 128 0 160))                                (create-solid-brush (encode-rgb 255 0 0))
                                (create-solid-brush (encode-rgb 1 255 0))
                                (create-solid-brush (encode-rgb 0 0 255))
                                (create-solid-brush (encode-rgb 255 255 0))
                                (create-solid-brush (encode-rgb 0 255 255))
                                (create-solid-brush (encode-rgb 255 0 255))))))

(defun delete-font ()
  (delete-object *font140*)
  (delete-object *font90*)
  (delete-object *font70*)
  (delete-object *font40*)
  (delete-object *font30*)
  (delete-object *font20*))

(defun delete-object-array (arr)
  (loop for i across arr
     do (delete-object i)))

(defun delete-brush ()
  (delete-object-array *brush*))

(defun delete-images ()
  (delete-object *p-img*)
  (delete-object *objs-img*)
  (delete-object *anime-monsters-img*)
  (delete-object *arrow-img*)
  (delete-object *waku-img*))





(defun init-keystate ()
  (with-slots (left right down up z x c enter shift w esc a b ctrl) *keystate*
    (setf left nil right nil down nil up nil z nil x nil w nil c nil enter nil
	  esc nil a nil b nil)))


;;プレイヤーの初期装備
(defun init-player-equip-item ()
  (with-slots (p) *game*
    (let ((armor  (shallow-copy-object (aref *weapondescs* +a_Leather_shield+)))
	  (weapon (shallow-copy-object (aref *weapondescs* +w_wood_sword+))))
      (setf (equiped weapon) t
	    (equiped armor) t
	    (weapon p) weapon
	    (armor p) armor)
      (push armor (item p))
      (push weapon (item p)))))
      
    

;;ゲーム初期化
(defun init-game ()
  (init-item-drop-rate-list)
  (setf *game* (make-instance
		'game :state :title
		:bgm-p nil
		:bgm-alias *op-bgm*
		:p (make-instance 'player :w *p-w* :h *p-h* :str 5 :def 2 :stage 1 :state :title
					  :hp 30 :maxhp 30 :name *name* :atk-spd 4 :orb 0 :move-spd 2
					  :moto-w *p-w* :moto-h *p-h* :atk-now nil :ido-spd 2 :level 1
					  :w/2 (floor *obj-w* 2) :h/2 (floor *obj-h* 2) :hammer 3
					  :dir +down+ :item nil :armor nil :weapon  nil :arrow-num 4
					  :anime-max 3 :anime-interval 20  :anime-now 0
					  :img +down+)
		:donjon  (make-instance
			  'donjon :tate *tate-block-num* :yoko *yoko-block-num* :stage 1
			  :drop-weapon (copy-tree *sword-drop-rate-list*)
			  :drop-armor (copy-tree *armor-drop-rate-list*)
				  :enemy-rate (copy-tree *enemy-appear-rate*))))
  (init-player-equip-item)
  (init-keystate))

;;効果音ならす
(defun sound-play (path)
  (play-sound path '(:filename :async)))


;;キー押したとき
(defun moge-keydown (hwnd wparam)
  (with-slots (left right down up z x c enter shift w esc a b ctrl) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
	(:keya (setf a t))
	(:keyb (setf b t))
        (:left (setf left t))
	(:shift (setf shift t))
	(:control (setf ctrl t))
        (:right (setf right t))
        (:down (setf down t))
        (:up (setf up t))
        (:return (setf enter t))
        (:keyz (setf z t))
        (:keyx (setf x t))
	(:keyc (setf c t))
	(:keyw (setf w t))
	(:escape  (send-message hwnd (const +wm-close+) nil nil)) ;; quit
        (:keyq 
         )))))

;;キー話したとき
(defun moge-keyup (wparam)
  (with-slots (left right down up z x c enter shift w esc a b ctrl) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
	(:keya (setf a nil))
	(:control (setf ctrl nil))
	(:keyb (setf b nil))
        (:left (setf left nil))
	(:shift (setf shift nil))
        (:right (setf right nil))
        (:down (setf down nil))
        (:up (setf up nil))
        (:return (setf enter nil))
        (:keyx (setf x nil))
	(:keyc (setf c nil))
	(:keyw (setf w nil))
	(:escape (setf esc nil))
        (:keyz (setf z nil))))))






;;1以上のランダムな数
(defun randval (n)
  (1+ (random n)))

(defun rand+- (n)
  (let ((a (1+ (random n))))
    (if (= (random 2) 1)
	a
	(- a))))

;;'(:up :down :right :left)
(defun rand-dir (lst new-lst)
  (if (null lst)
      new-lst
      (let ((hoge (nth (random (length lst)) lst)))
	(rand-dir (remove hoge lst) (cons hoge new-lst)))))





;;プレイヤーに隣接しているか
(defun enemy-can-atk? (e)
  (with-slots (p) *game*
  (loop :for new :in *tonari*
     :for dir :in *tonari-dir*
     :when (equal (mapcar #'+ (pos e) new) (pos p))
     :return dir)))


;;ブロック削除
(defun remove-block (b)
  (with-slots (donjon) *game*
    (with-slots (blocks) donjon
      (setf blocks (remove b blocks :test #'equal)))))

(defun hit-hantei (pos &key (player nil) (walls nil) (blocks nil)
			 (items nil) (enemies nil) (door nil))
  (with-slots (p donjon) *game*
    (when player
      (when (equal pos (pos p))
	(return-from hit-hantei t)))
    (when walls
      (when (find pos (walls donjon) :test #'equal)
	(return-from hit-hantei t)))
    (when blocks
      (let ((blo (find pos (blocks donjon) :test #'equal)))
	(when blo
	  (return-from hit-hantei blo))))
    (when items
      (let ((item (find pos (items donjon) :test #'equal :key #'pos)))
	(when item
	  (return-from hit-hantei item))))
    (when enemies
      (let ((e (find pos (enemies donjon) :test #'equal :key #'pos)))
	(when e
	  (return-from hit-hantei e))))
    (when (and door (door donjon))
      (return-from hit-hantei (equal (pos (door donjon)) pos)))
    nil))

;;ボスを倒したら
(defun go-ending ()
  (with-slots (state endtime diffculty cursor) *game*
    (setf state :ending
	  cursor 0
	  endtime (get-internal-real-time))
    (let ((new-clear-time (- endtime *start-time* ))
	  (old-clear-time (nth diffculty *donjon-clear-time-list*)))
      (when (or (= old-clear-time 0)
                (> old-clear-time new-clear-time))
	(setf (nth diffculty *donjon-clear-time-list*) new-clear-time)
	(write-clear-time )))))

;;同じ武器が3つあったらレベルアップ
(defun auto-levelup-item (new-item p)
  (when (>= (count-if #'(lambda (x) (and (equal (name x) (name new-item))
                                       (= (level x) (level new-item)))) (item p))
          1)
      (setf (item p) (remove-if #'(lambda (x) (and (equal (name x) (name new-item))
                                                          (= (level x) (level new-item)))) (item p)))
      (incf (level new-item))
      (if (eq (type-of new-item) 'weapondesc)
          (incf (damage new-item) (level new-item))
          (incf (def new-item) (level new-item)))
      (auto-levelup-item new-item p)))

;;取得したアイテム名を表示するためのもの
(defun set-get-item (p name)
  (setf (getitem p) (make-instance 'itemtext :name name
                                             :posx (max 0
							(- (posx p)
							   (* (length name) 10)))
				             :posy (posy p)
                                             :maxy (- (posy p) 20))))

;;装備アイテム取得
(defun get-equip-item (p drop-item-list)
  (let* ((drop (weightpick drop-item-list))
         (new-item (shallow-copy-object (aref *weapondescs* drop))))
    (setf (new new-item) t)
    (set-get-item p (name new-item))
    (auto-levelup-item new-item p)
    (push new-item (item p))))






;;bgm 
(defun change-bgm (new-alias)
  (with-slots (bgm-alias) *game*
    (let ((st (bgm-status bgm-alias)))
      (cond
	((equal st "stopped")
	 (setf bgm-alias new-alias))
	(t
	 (bgm-stop bgm-alias)
	 (setf bgm-alias new-alias)
	 (bgm-play bgm-alias))))))

(defun get-potion (p)
  (set-get-item p "ポーション")
  (incf (potion p)))

(Defun get-arrow (p)
  (set-get-item p "矢")
  (incf (arrow-num p)))

(defun get-hammer (p)
  (set-get-item p "ハンマー")
  (incf (hammer p)))

;;プレイヤーとフロアにあるアイテムの当たり判定
(defun player-hit-item (p)
  (with-slots (donjon) *game*
    (with-slots (items stage drop-weapon drop-armor) donjon
      (let ((item (find (pos p) items :key #'pos :test #'equal)))
	(when item
	  (sound-play *get-item-wav*)
	  (cond
	    ((= (img item) +chest+)
	     (let* ((n (random 12)))
	       (cond
		 ((>= 1 n 0)   (get-arrow p))
		 ((>= 3 n 2)   (get-potion p))
		 ((>= 6 n 4)   (get-equip-item p drop-armor))
		 ((>= 10 n 7) (get-equip-item p drop-weapon))
		 ((>=  n 11)   (get-hammer p)))))
	    ((= (img item) +potion+)
	     (get-potion p))
	    ((= (img item) +sword+)
	     (get-equip-item p drop-weapon))
	    ((= (img item) +armour+)
	     (get-equip-item p drop-armor))
	    ((= (img item) +hammer+)
	     (get-hammer p))
	    ((= (img item) +arrow+)
	     (get-arrow p))
	    ((= (img item) +orb+)
	     (incf (orb p))
	     (go-ending)
	     (sound-play *get-orb*)
	     (change-bgm *ed-bgm*)))
	  (setf items (remove item items :test #'equal)))))))



;;ダメージ計算
(defmethod damage-calc ((atker player) defender)
  (with-slots (weapon) atker
    (let* ((a1 (+ (str atker) (damage weapon))))
      (max 1 (floor (* (- a1 (/ (def defender) 2)) (/ (+ 99 (random 55)) 256)))))))

;;ダメージ計算
(defmethod damage-calc ((atker enemy) defender)
  (with-slots (armor) defender
    (let* ((a1 (str atker)))
      (if (> (blk armor) (random 100))
	  0
	  (max 1 (floor (* (- a1 (/ (def defender) 2)) (/ (+ 99 (random 55)) 256))))))))

;;ダメージ計算
(defmethod damage-calc ((atker arrow) (defender player))
  (with-slots (armor) defender
    (let* ((a1 (level atker)))
      (if (> (blk armor) (random 100))
          0
          (max 1 (floor (* (- a1 (/ (def defender) 2)) (/ (+ 99 (random 55)) 256))))))))
;; プレイヤー→敵
(defmethod damage-calc ((atker arrow) (defender enemy))
  (let* ((a1 (level atker)))
    (max 1 (floor (* (- a1 (/ (def defender) 2)) (/ (+ 99 (random 55)) 256))))))

;;ダメージ計算
(defmethod damage-calc ((atker fire) (defender player))
  (with-slots (armor) defender
    (let* ((a1 (level atker)))
      (max 1 (floor (* (- a1 (/ (def defender) 2)) (/ (+ 99 (random 55)) 256)))))))
;;プレイヤー→敵
(defmethod damage-calc ((atker fire) (defender enemy))
  (let* ((a1 (level atker)))
    (max 1 (floor (* (- a1 (/ (def defender) 2)) (/ (+ 99 (random 55)) 256))))))






;;レヴェルアップ時ステータス上昇
(defun status-up (atker)
  (incf (maxhp atker) (+ (random 3) 2))
  (setf (hp atker) (maxhp atker))
  (incf (str atker) (random 3))
  (incf (def atker) (random 3)))

;;経験値取得
(defun player-get-exp (atker defender)
  (when (eq 'player (type-of atker))
    (incf (expe atker) (expe defender))
    (when (>= (expe atker) (lvup-exp atker))
      (sound-play *lvup-wav*)
      (loop while (>= (expe atker) (lvup-exp atker))
	    do
	       (status-up atker)
	       (incf (level atker))
	       (setf (expe atker) (- (expe atker) (lvup-exp atker)))
	       (incf (lvup-exp atker) 20)))))

;;ダメージ表示用
(defun create-dmg-font (dmg-num defender color)
  (with-slots (x y posx posy obj-type hp atk-spd) defender
    (let* ((x-dir (if (eq (random 2) 0) +left+ +right+))
	   (dmg-x (if (eq x-dir +left+) (+ posx (+ 4 (random 16))) (+ posx (+ 13 (random 18)))))
	   (dmg-y (+ posy (+ 8 (random 18))))
	   
	   (dmg (make-instance 'dmg-font :posx dmg-x :posy dmg-y :pos (list dmg-x dmg-y)
					 :dmg-num  dmg-num :vx 1 :vy (1+ (random 1))
					 :y-dir +up+ :x-dir x-dir :color color
					 :maxy dmg-y :miny (- dmg-y (+ 12 (random 15))))))
      dmg)))

;;ダメージ計算して表示する位置とか設定
(defun set-damage (atker defender color)
  (with-slots (dmg) *game*
    (with-slots (x y posx posy obj-type hp atk-spd) defender
      (let* ((dmg-num (damage-calc atker defender)))
	(if (> dmg-num 0)
	    (sound-play *damage-wav*)
	    (sound-play *guard*))
	(push (create-dmg-font dmg-num defender color) ;;ダメージを表示するためのもの
	      dmg)
	(decf (hp defender) dmg-num) ;;hpを減らす
	(when (>= 0 (hp defender)) ;; hpが0以下になったら死亡
	  (setf (dead defender) t)
	  (player-get-exp atker defender))))))





;;ダメージフォントの位置更新
(defun update-damage-fonts ()
  (with-slots (dmg) *game*
    (loop :for d :in dmg
	  :do (with-slots (vx vy x-dir y-dir posx posy maxy miny interval) d
		(incf interval)
		(when (>= interval 1)
		  (setf interval 0)
		  (cond
		    ((eq +up+ y-dir)
		     (if (eq x-dir +right+)
			 (incf posx vx)
			 (decf posx vx))
		     (decf posy vy)
		     (when (<= posy miny)
		       (setf y-dir +down+)))
		    ((eq +down+ y-dir)
		     (if (eq x-dir +right+)
			 (incf posx vx)
			 (decf posx vx))
		     (incf posy vy)
		     (when (>= posy maxy)
		       (setf dmg (remove d dmg :test #'equal))))))))))

;;ダメージフォントの位置更新
;; (defun update-damage-fonts (atk def)
;;   (when (zerop (mod (atk-c atk) 3))
;;     (update-damage-font def)))


(defun delete-damage-font (def)
  (setf (dmg def) nil))


;;攻撃した時の移動アニメーション計算
(defun update-atk-img-pos (p atk-pos)
  (cond
    ((and (null (atk-pos-f p))
	  (<= *atk-pos-max* atk-pos))
     (setf (atk-pos-f p) t)
     (- atk-pos *atk-width*))
    ((and (atk-pos-f p)
	  (<= atk-pos 0))
     (setf (atk-pos-f p) nil
	   (atk-now p) nil)
     0)
    ((and (null (atk-pos-f p))
	  (> *atk-pos-max* atk-pos))
     (+ atk-pos *atk-width*))
    ((and (atk-pos-f p)
	  (> atk-pos 0))
     (- atk-pos *atk-width*))))

;;攻撃移動更新
(defun update-atk-img (p)
  (with-slots (img atk-c w anime-max) p
    (when (zerop (mod atk-c 6))
      ;;(print img)
      (when (= img  (* w anime-max))
	(setf img 0))
      (incf img w))
    (when (zerop (mod atk-c(atk-spd p)))
      ;;(setf atk-c 0)
      ;;(incf img)
      ;;(print img)
      (cond
	((= (dir p) +right+) (setf (atk-pos-x p) (update-atk-img-pos p (atk-pos-x p))))
	((= (dir p) +left+)  (setf (atk-pos-x p) (- (update-atk-img-pos p (- (atk-pos-x p))))))
	((= (dir p) +down+)  (setf (atk-pos-y p) (update-atk-img-pos p (atk-pos-y p))))
	((= (dir p) +up+)    (setf (atk-pos-y p) (- (update-atk-img-pos p (- (atk-pos-y p))))))))))

;;普通のモンスター
(defmethod update-monster-atk-img ((e enemy))
  (with-slots (img atk-c w anime-max dir atk-pos-x atk-pos-y) e
    (when (= img  (* w anime-max))
      (setf img 0))
    (incf img w)
    (cond
      ((= dir +right+) (setf atk-pos-x (update-atk-img-pos e atk-pos-x)))
      ((= dir +left+)  (setf atk-pos-x (- (update-atk-img-pos e (- atk-pos-x)))))
      ((= dir +down+)  (setf atk-pos-y (update-atk-img-pos e atk-pos-y)))
      ((= dir +up+)    (setf atk-pos-y (- (update-atk-img-pos e (- atk-pos-y))))))))

;;ヒドラの攻撃更新 ヒドラの周りを一周させる
(defmethod update-monster-atk-img ((head hydra-head))
  (with-slots (donjon atk-chara) *game*
    (with-slots (enemies) donjon
      (with-slots (posx posy atk-now) head
	(let* ((radian  (/ (* (deg head) pi) 180))
	       (addx (floor (* (cos radian) 30)))
	       (addy (floor (* (sin radian) 30))))
	  (setf (posx head) (- (+ (centerx head) addx) (w/2 head))
	   	(posy head) (- (+ (centery head) addy) (h/2 head)))
	  (incf (deg head) 10))
	;;(format t "posx ~d posy ~d~%" posx posy)
	(when (< 360 (deg head))
	  (setf enemies
		(remove head enemies :test #'equal)
		atk-now nil))))))


;;攻撃アニメ終わるまでループ
(defun update-atk-anime (atk)
  (with-slots (atk-now atk-c dir actioned-p img-h) atk
    (incf atk-c)
    (update-atk-img atk)
    (unless atk-now
      (setf actioned-p t
	    atk-c 0
	    img 0))
    (setf img-h dir)))





;;隣に敵がいるか調べる
(defun check-neighbour (p e)
  (let ((dir (mapcar #'- (pos p) (pos e))))
    (cond
      ((equal dir '(1 0))
       +left+)
      ((equal dir '(-1 0))
       +right+)
      ((equal dir '(0 1))
       +up+)
      ((equal dir '(0 -1))
       +down+))))


(defun create-fire (p)
  (make-instance 'fire :img-h +dragon-fire+ :posx (posx p) :dir (dir p) :img 0
		      :x (x p) :y (y p) :level (str p) :belong :enemy
		      :posy (posy p) :img-src *anime-monsters-img*
		      :pos (copy-list (pos p))))

(defun shot-fire (p)
  (with-slots (donjon long-atk-chara) *game*
    (let ((fire (create-fire p)))
      ;;(setf (arrow donjon) fire)
      (push fire long-atk-chara)
      )))

;;矢
(defun create-arrow (p)
  (with-slots (dir posx posy w h pos level) p
    (let ((new-posx (cond ((eq (type-of p) 'player)
			   (- posx 4))
			  (t posx))))
      (make-instance 'arrow :img (dir p) :posx new-posx :dir (dir p)
			    :x (x p) :y (y p) :level (+ level (str p))
			    :belong (if (eq (type-of p) 'player) :player :enemy)
			    :posy posy :img-src *arrow-img*
			    :pos (copy-tree pos)))))


;;矢生成
(defun shot-arrow (p)
  (with-slots (long-atk-chara) *game*
    (let ((arrow (create-arrow p)))
      ;;(setf (arrow donjon) arrow))
      (push arrow long-atk-chara)
      )))

;;矢の位置更新
(defun set-next-arrow-pos (arrow)
  (let ((arrow-spd 6))
    (cond
      ((eq (dir arrow) +left+)
       (decf (posx arrow) arrow-spd)
       (setf (pos arrow) (list (1+ (floor (posx arrow) *obj-w*))
			       (floor (posy arrow) *obj-h*))))
      ((eq (dir arrow) +right+)
       (incf (posx arrow) arrow-spd)
       (setf (pos arrow) (list (floor (posx arrow) *obj-w*)
			       (floor (posy arrow) *obj-h*))))
      ((eq (dir arrow) +up+)
       (decf (posy arrow) arrow-spd)
       (setf (pos arrow) (list (floor (posx arrow) *obj-w*)
			       (1+ (floor (posy arrow) *obj-h*)))))
      ((eq (dir arrow) +down+)
       (incf (posy arrow) arrow-spd)
       (setf (pos arrow) (list (floor (posx arrow) *obj-w*)
			       (floor (posy arrow) *obj-h*)))))))

;;弓矢攻撃アニメ
;; (defun update-arrow-loop (p hwnd)
;;   (with-slots (donjon) *game*
;;     (with-slots (arrow) donjon
;;       (loop
;; 	:do
;; 	   (let ((tempos (copy-list (pos arrow))))
;; 	     (set-next-arrow-pos arrow)
;; 	     (when (hit-hantei (pos arrow) :walls t :blocks t)
;; 	       (push (make-instance 'obj :img +arrow+ :pos tempos)
;; 		     (items donjon))
;; 	       (setf arrow nil)
;; 	       (return))
;; 	     (let ((e (hit-hantei (pos arrow) :enemies t)))
;; 	       (when e
;; 		 (set-damage p e (encode-rgb 255 147 122))
;; 		 (sound-play *hit-arrow*)
;; 		 (setf arrow nil)
;; 		 (loop :while (dmg e)
;; 		       :do (incf (atk-c p))
;; 			   (update-damage-fonts)
;; 			   (invalidate-rect hwnd nil nil)
;; 			   (update-window hwnd))
;; 		 (setf (atk-c p) 0)
;; 		 (return)))
;; 	     (invalidate-rect hwnd nil nil)
;; 	     (update-window hwnd))))))


;;位置を戻す
(defun return-position (p tempos)
  (with-slots (pos x y posx posy) p
    (setf pos tempos
	  x (car pos)
	  y (cadr pos)
	  posx (* x *obj-w*)
	  posy (* y *obj-h*))))

;;キー入力をもとに方向を変える
(defun set-dir-key (p)
  (with-slots (up down left right) *keystate*
    (with-slots (dir) p
      (cond
	(up (setf dir +up+))
	(down (setf dir +down+))
	(left (setf dir +left+))
	(right (setf dir +right+))))))
  
;;プレイヤーの移動先当たり判定
(defun player-hit-hantei (p nextpos)
  (with-slots (donjon) *game*
    (with-slots (pos hammer atk-now x y posx posy) p
      (let ((e (hit-hantei nextpos :enemies t))
	    (blo (hit-hantei nextpos :blocks t)))
	(cond
	  (e (set-dir-key p)
	     t)
	  (blo (set-dir-key p)
	       t)
	  ((hit-hantei nextpos :walls t :blocks t)
	   t)
	  (t nil))))))


;;ドアとの当たり判定
(defun player-hit-door (p)
  (with-slots (donjon) *game*
    (with-slots (stage enemy-rate) donjon
      (when (hit-hantei (pos p) :door t)
	(sound-play *door-wav*)
	(incf stage)
	(adjust-enemy-rate)
	(adjust-item-rate)
	(create-maze)))))


;;描画ポジション更新
(defun update-move-pos (p spd)
  (with-slots (dir posx posy move-spd) p
    (cond
      ((= dir +right+) (incf posx spd))
      ((= dir +left+)  (decf posx spd))
      ((= dir +down+)  (incf posy spd))
      ((= dir +up+)    (decf posy spd)))))

(defun update-move-anime (p spd)
  (with-slots (move-chara) *game*
    (with-slots (posx posy move-c move-p nextposx nextposy img anime-now w anime-max dmg) p
      (incf move-c)
      (when (= move-c 8)
	(incf anime-now)
	(setf move-c 0))
      (when (> anime-now anime-max)
	(setf anime-now 0))
      (setf img (* anime-now w))
      
      (update-move-pos p spd)
      (when (and (= posx nextposx)
		 (= posy nextposy))
	(setf move-chara (remove p move-chara :test #'equal))))))
	;; (setf move-p nil))))
    ;; 	    (turn *game*) +enemy+))))
     ;;(invalidate-rect hwnd nil nil)
     ;;(update-window hwnd))))

;;モンスター移動描画
(defun monster-move (e hwnd)
  (with-slots (nextposx nextposy move-c posx posy actioned-p move-p) e
    ;;(incf move-c)
    ;;(when (= move-c 1)
      (setf move-c 0)
      (update-move-pos e)
      (when (and (= posx nextposx)
		 (= posy nextposy))
	(setf move-p nil
	      actioned-p t))
      (invalidate-rect hwnd nil nil)
    (update-window hwnd)))

;;移動するキャラだけアニメ
(defun update-chara-move (lst)
  (loop :for c :in lst
	:do (update-move-anime c 2)))


;;向いてる方向のnextpos
(defun get-dir-nextpos (p)
  (with-slots ((px x) (py y) dir) p
    (let ((nx (cond ((eq dir +left+)
		     (- px 1))
		    ((eq dir +right+)
		     (+ px 1))
		    (t px)))
	  (ny (cond ((eq dir +up+)
		     (- py 1))
		    ((eq dir +down+)
		     (+ py 1))
		    (t py))))
      (list nx ny))))

;;向いてる方向のブロック取得
(defun get-dir-block (p)
  (let ((nextpos (get-dir-nextpos p)))
    (hit-hantei nextpos :blocks t)))

;;向いてる方向にいる敵を取得
(defun get-dir-enemy (p)
  (let ((nextpos (get-dir-nextpos p)))
    (hit-hantei nextpos :enemies t)))

;;新しい床セット
(defun set-new-yuka (pos)
  (with-slots (donjon) *game*
    (with-slots (yuka) donjon
      (push pos yuka))))

;;キー入力処理
(defun update-input-key-action (p)
  (with-slots (move-chara long-atk-chara) *game*
  (with-slots (left right down up z c (keyx x) w shift enter) *keystate*
    (with-slots (x y posx posy pos arrow-num potion state hp maxhp dir poison-cnt move-p action-p
		 nextposx nextposy atk-now img-h atk-c img hammer) p
      (let ((nextpos nil)
	    (nextx 0)
	    (nexty 0))
	;; (when (or keyx)
	;;   (setf (turn *game*) +enemy+))
	(cond
	  (z
	   (let ((e (get-dir-enemy p))
		 (b (get-dir-block p)))
	     (cond
	       (e (set-damage p e (encode-rgb 255 255 255)))
	       ((and b (> hammer 0))
                (decf hammer)
		(set-new-yuka (copy-list b))
		(sound-play *atk-block-wav*)
		(remove-block b)))
	     (setf atk-now t
		   img 0
		   img-h (+ dir 4))
	     (setf action-p t)))
	  (enter
	   (player-hit-door p))
	  ((and c
		(> potion 0))
	   ;;(print 1)
	   (sound-play *use-potion*)
	   (decf potion)
	   (setf hp maxhp
		 poison-cnt 0
		 action-p t))
	  ((and keyx
		(> arrow-num 0))
	   (sound-play *atk-arrow*)
	   (decf arrow-num)
	   (shot-arrow p)
	   ;;(update-arrow-loop p hwnd)
	   (setf action-p t))
	  (left
	   (setf dir +left+
		 nextx (1- x)
		 nexty y))
	  (right
	   (setf dir +right+
		 nextx (1+ x)
		 nexty y))
	  (up
	   (setf dir +up+
		 nexty (1- y)
		 nextx x))
	  (down
	   (setf dir +down+
		 nexty (1+ y)
		 nextx x)))
	(setf img-h dir)
	(when (or left right up down)
	  (setf nextpos (list nextx nexty))
	  (unless (player-hit-hantei p nextpos)
	    (update-poison p)
	    (setf pos nextpos
		  x nextx
		  y nexty
		  nextposx (+ (* x *obj-w*) 4)
		  nextposy (* y *obj-h*)
		  action-p t)
	    (push p move-chara))))))))
	    ;;(update-move-anime p (+ (* x *obj-w*) 4) (* y *obj-h*) hwnd)))))))

;;階層ごとにアイテムのドロップ率変更
(defun adjust-item-rate ()
  (with-slots (donjon) *game*
    (rate-decf 30 (drop-weapon donjon))
    (rate-decf 30 (drop-armor donjon))))




;;ゲットしたアイテムのテキスト動かす
(defun update-get-item-text ()
  (with-slots (p) *game*
  (with-slots (getitem) p
    (when getitem
      (decf (posy getitem) 1)
      (when (> (maxy getitem) (posy getitem))
	(setf getitem nil))))))

;;poison-cntが0より大きい時毒ダメージ
(defun update-poison (p)
  (with-slots (dmg) *game*
    (with-slots (poison-cnt hp atk-c dead dash-p) p
      (when (> poison-cnt 0)
        (decf poison-cnt)
        (let ((poison-dmg (max 1 (random (max 1 (floor hp 10))))))
          (push (create-dmg-font poison-dmg p (encode-rgb 192 48 192)) dmg)
          (decf hp poison-dmg)
          (sound-play *poison*)
          (when (>= 0 hp)
            (setf dead t)))))))
  
;; アニメ
(defun update-anime (obj)
  (with-slots (anime-now anime-max anime-interval anime-time img w pos) obj
    (incf anime-time)
    (when (= anime-time anime-interval)
      (incf anime-now)
      (setf anime-time 0)
      (when (> anime-now anime-max)
	(setf anime-now 0))
      (setf img (* anime-now w)))))



;;プレイヤーの色々更新
(defun update-player (p)
  (update-input-key-action p)
  (player-hit-item p))






;;移動可能場所生成
(defun where-to-go-next (e)
  (loop :for nextpos :in '((0 1) (0 -1) (1 0) (-1 0) (0 0))
     :unless (hit-hantei (mapcar #'+ (pos e) nextpos) :walls t :blocks t :enemies t)
     :collect nextpos))


(defun get-dir (lst)
  (cond
    ((equal lst '(0 1))
     +down+)
    ((equal lst '(0 -1))
     +up+)
    ((equal lst '(1 0))
     +right+)
    ((equal lst '(-1 0))
     +left+)))


;;ランダムで移動可能場所選ぶ
(defun update-enemy-pos-random (e)
  (with-slots (move-chara) *game*
  (with-slots (pos posx posy x y dir move-p nextposx nextposy actioned-p) e
    (let* ((next-list (where-to-go-next e)))
      (if next-list
	  (let ((next (nth (random (length next-list)) next-list)))
	    (setf pos (mapcar #'+ pos next)
		  x    (car pos)
		  nextposx (* x *obj-w*)
		  y    (cadr pos)
		  dir (get-dir next)
		  nextposy (* y *obj-h*)
		  move-p t)
	    (push e move-chara))
	  (setf actioned-p t))))))

;;nextにいけるならセット
(defun set-enemy-next-pos (e next diff)
  (with-slots (pos x nextposx y nextposy dir move-p) e
    (setf pos next
          x        (car (pos e))
          nextposx (* (x e) *obj-w*)
          y        (cadr (pos e))
          nextposy (* (y e) *obj-h*)
          dir (get-dir diff)
          move-p t)))

;;nextにいけるかチェック
(defun can-move-next-pos? (e next diff)
  (with-slots (move-chara) *game*
    (with-slots (actioned-p) e
      (if (null (hit-hantei next :walls t :blocks t :enemies t))
          (progn
            (set-enemy-next-pos e next diff)
            (push e move-chara))
          (setf actioned-p t)))))

;;敵がプレイヤーを追いかけてくる
(defun update-tracking-pos (e)
  (with-slots (p donjon move-chara) *game*
    (with-slots (enemies) donjon
    (with-slots (pos x y posx posy nextposx nextposy move-p dir actioned-p) e
      (let* ((enemies-pos (loop :for enemy :in enemies
                                :collect (pos enemy)))
             (next (astar (pos e) (pos p) (field donjon) *move-cost* (append (walls donjon) (blocks donjon) enemies-pos))))
        (if (listp next)
            (let ((diff (mapcar #'- next pos)))
              (can-move-next-pos? e next diff))
            (progn
              (setf next (astar (pos e) (pos p) (field donjon) *move-cost* (append (walls donjon) (blocks donjon))))
              (when (listp next)
                (let ((diff (mapcar #'- next pos)))
                  (can-move-next-pos? e next diff)))))
          )))))


;;となりにプレイヤー居たら殴る
(defun enemy-attack-neighbour (e p)
  (with-slots (atk-chara) *game*
    ;;(set-damage e p)
    (setf (atk-now e) t
	  (dash-p p) nil)
    (push e atk-chara)))


;;スライムの行動
(defmethod update ((e slime) p)
  (setf (dir e) (check-neighbour e p))
  (cond
    ((dir e)
     (enemy-attack-neighbour e p))
    (t
     (update-enemy-pos-random e))))

;;スライムの行動
(defmethod update ((e yote1) p)
  (setf (dir e) (check-neighbour e p))
  (cond
    ((dir e)
     (enemy-attack-neighbour e p))
    (t
     (update-enemy-pos-random e))))



;;オークの行動
(defmethod update ((e orc) p)
  (setf (dir e) (check-neighbour e p))
  (cond
    ((dir e)
     (enemy-attack-neighbour e p))
    ((>= (sight e) (manhatan (pos e) (pos p)))
     (update-tracking-pos e))
    (t
     (update-enemy-pos-random e))))


(defun check-hydra-around-player (e p)
  (loop :for x :in *around*
     :for npos = (mapcar #'+ (pos e) x)
     :when (equal (pos p) npos)
     :return t))

;;ヒドラの攻撃用頭を追加する
(defun set-hydra-head (e)
  (with-slots (atk-chara donjon) *game*
    (with-slots (enemies) donjon
      (let ((head (make-instance 'hydra-head :img 0 :obj-type :hydra-atk :img-h +hydra-atk+
					:posx (+ (posx e) 32) :posy (posy e) :str (str e)
					     :moto-w 32 :moto-h 32 :dir (dir e) :atk-now t
					     :atk-interval 1
					:w 32 :h 32 :w/2 16 :h/2 16)))
	(setf (centerx head) (* (+ (posx e) (w/2 e)) 1)
	      (centery head) (* (+ (posy e) (h/2 e)) 1))
	(push head enemies)
	(push head atk-chara)))))

(defun set-hydra-attack (e)
  (with-slots (atk-chara) *game*
    (push e atk-chara)))

;;ヒドラの行動
(defmethod update ((e hydra) p)
  (with-slots (dash-p) p
    (cond
      ((check-hydra-around-player e p)
       (set-hydra-attack e))
      ;;((>= (sight e) (manhatan (pos e) (pos p)))
      ;; (update-tracking-pos e))
      (t
       (update-enemy-pos-random e)))))

  

;;遠距離攻撃のアニメ更新
(defun update-long-atk ()
  (with-slots (long-atk-chara p donjon) *game*
    (with-slots (enemies blocks walls items) donjon
      (loop :for arrow :in long-atk-chara
	    :do (with-slots (belong pos) arrow
		  (let ((tempos (copy-tree pos)))
		    (set-next-arrow-pos arrow)
		    (let ((hitted nil)
			  (color nil))
		      (if (eq  belong :player)
			  (setf hitted (find (pos arrow)  enemies :key #'pos :test #'equal)
				color (encode-rgb 255 255 255))
			  (setf hitted (if (equal (pos arrow) (pos p)) p nil)
				color (encode-rgb 255 147 122)))
		      (cond
			(hitted
			 (set-damage arrow hitted color)
			 (if (eq (type-of arrow) 'arrow)
			     (sound-play *hit-arrow*)
			     (sound-play *hit-fire*))
			 (setf long-atk-chara (remove arrow long-atk-chara :test #'equal)))
			(t
			 (let ((hitted-b (find (pos arrow) blocks :test #'equal))
			       (hitted-w (find (pos arrow) walls :test #'equal)))
			   (when (or hitted-b hitted-w)
			     (push (make-instance 'obj :img +arrow+ :pos tempos)
 				   items)
			     (setf long-atk-chara (remove arrow long-atk-chara :test #'equal)))))))))
	    ))))


;;直線状にpがいるか
(defun check-straight-iru (e p x)
  (loop
     :repeat (range e)
     :for pos = (copy-list (pos e)) :then pos
     :do (setf pos (mapcar #'+ pos x))
     :when (hit-hantei pos :walls t :blocks t :enemies t)
     :return nil
     :when (equal (pos p) pos)
     :return t))


;;遠距離攻撃がぷれいやーにあたるか
(defun check-can-long-atk (e p)
  (let ((px (x p)) (py (y p))
	(ex (x e)) (ey (y e)))
    (cond
      ;;pが下にいる
      ((and (= px ex)
	    (> py ey)
	    (check-straight-iru e p '(0 1)))
       +down+)
      ((and (= px ex)
	    (< py ey)
	    (check-straight-iru e p '(0 -1)))
       +up+)
      ((and (= py ey)
	    (> px ex)
	    (check-straight-iru e p '(1 0)))
       +right+)
      ((and (= py ey)
	    (< px ex)
	    (check-straight-iru e p '(-1 0)))
      +left+)
      )))

(defmethod enemy-long-attack ((e brigand) p nageru)
  (with-slots (long-atk-chara) *game*
    (setf (dir e) nageru
	  (dash-p p) nil
	  (move-p p) nil
	  (actioned-p e) t)
    (shot-arrow e)
    (sound-play *atk-arrow*)
    ))
;;(update-long-atk-loop e p hwnd))

;;ブリガンドの行動
(defmethod update ((e brigand) p)
  (let ((nageru (check-can-long-atk e p)))
    (setf (dir e) (check-neighbour e p))
    (cond
      ((dir e)
       (enemy-attack-neighbour e p))
      (nageru
       (enemy-long-attack e p nageru))
      (t
       (update-enemy-pos-random e)))))






(defmethod enemy-long-attack ((e dragon) p nageru)
  (setf (dir e) nageru
	(dash-p p) nil)
  (shot-fire e)
  (sound-play *atk-fire*)
  )


;;ドラゴンの行動
(defmethod update ((e dragon) p)
  (let ((nageru (check-can-long-atk e p)))
    (setf (dir e) (check-neighbour e p))
    (cond
      ((dir e)
       (enemy-attack-neighbour e p))
      (nageru
       (enemy-long-attack e p nageru))
      ((>= (sight e) (manhatan (pos e) (pos p)))
       (update-tracking-pos e))
      (t
       (update-enemy-pos-random e)))))


(defmethod update ((e chest) p)
  )

;;スケルトンの行動
(defmethod update ((e skeleton) p)
  (setf (dir e) (check-neighbour e p))
  (cond
    ((dir e)
     (enemy-attack-neighbour e p))
    ((>= (sight e) (manhatan (pos e) (pos p)))
     (update-tracking-pos e))
    (t
     (update-enemy-pos-random e))))

;;バブルの行動
(defmethod update ((e bubble) p)
  (setf (dir e) (check-neighbour e p))
  (cond
    ((dir e)
     (enemy-attack-neighbour e p)
     (when (= 0 (random 10)) ;;プレイヤーを毒状態にする
       (setf (poison-cnt p) 5)))
    ((>= (sight e) (manhatan (pos e) (pos p)))
     (update-tracking-pos e))
    (t
     (update-enemy-pos-random e))))


;;スライムダッシュ
(defmethod update-dash ((e slime) p)
  (cond
    ((check-neighbour e p)
     (setf (dash-p p) nil))
    (t
     (update-enemy-pos-random e))))

;;yote1
(defmethod update-dash ((e yote1) p)
  (cond
    ((check-neighbour e p)
     (setf (dash-p p) nil))
    (t
     (update-enemy-pos-random e))))

;;オークダッシュ
(defmethod update-dash ((e orc) p)
  (cond
    ((check-neighbour e p)
     (setf (dash-p p) nil))
    ((>= (sight e) (manhatan (pos e) (pos p)))
     (update-tracking-pos e))
    (t
     (update-enemy-pos-random e))))

;;ヒドラダッシュ
(defmethod update-dash ((e hydra) p)
  (cond
    ((check-hydra-around-player e p)
     (setf (dash-p p) nil))
    (t
     (update-enemy-pos-random e))))

;;ブリガンドダッシュ
(defmethod update-dash ((e brigand) p)
  (cond
    ((or (check-can-long-atk e p)
         (check-neighbour e p))
     (setf (dash-p p) nil))
    (t
     (update-enemy-pos-random e))))

;;ドラゴンダッシュ
(defmethod update-dash ((e dragon) p)
  (cond
    ((or (check-can-long-atk e p)
         (check-neighbour e p))
     (setf (dash-p p) nil))
    ((>= (sight e) (manhatan (pos e) (pos p)))
     (update-tracking-pos e))
    (t
     (update-enemy-pos-random e))))

;;バブルダッシュ
(defmethod update-dash ((e bubble) p)
  (cond
    ((check-neighbour e p)
     (setf (dash-p p) nil))
    (t 
     (update-enemy-pos-random e))))

;;スケルトンダッシュ
(defmethod update-dash ((e skeleton) p)
  (cond
    ((check-neighbour e p)
     (setf (dash-p p) nil))
    ((>= (sight e) (manhatan (pos e) (pos p)))
     (update-tracking-pos e))
    (t
     (update-enemy-pos-random e))))
    

;;敵の待機アニメ
(defun update-enemy-wait-anime ()
  (with-slots (donjon) *game*
    (with-slots (enemies) donjon
      (loop :for e :in enemies
	    :do (update-anime e)))))

;;敵の位置更新
(defun update-enemies ()
  (with-slots (p donjon turn) *game*
    (with-slots (enemies) donjon
      (loop :for e :in enemies
	    :do (with-slots (move-p dead actioned-p) e
		  (unless dead
		    (update e p)))))))

;; (defun update-enemies (hwnd)
;;   (with-slots (p donjon turn) *game*
;;     (with-slots (enemies) donjon
;;       (let ((actioned-num 0))
;; 	(loop :for e :in enemies
;; 	      :do (unless (eq (type-of e) 'chest)
;; 		    (with-slots (move-p dead actioned-p) e
;; 		      (unless dead
;; 			(update-anime e)
;; 			;;(update-damage-font e)
;; 			(cond
;; 			  ((and move-p (null actioned-p))
;; 			   (monster-move e hwnd))
;; 			  ((and (null move-p) (null actioned-p))
;; 			   (update e p hwnd))
;; 			  (actioned-p
;; 			   (incf actioned-num)))))))
;; 	(when (= actioned-num (length enemies))
;; 	  (setf turn +player+)
;; 	  (loop :for e :in enemies
;; 		:do (setf (actioned-p e) nil)))))))
;;モンスター攻撃モーション
(defun update-chara-atk (atk-lst)
  (with-slots (atk-chara) *game*
    (loop :for e :in atk-lst
	  :do (with-slots (atk-now atk-c atk-interval) e
		(incf atk-c)
		(when (>= atk-c atk-interval)
		  (update-monster-atk-img e)
		  (setf atk-c 0))
		(unless atk-now
		  (setf atk-chara (remove e atk-chara :test #'equal)))))))

;;敵ダッシュ TODO
(Defun update-dash-enemies (p)
  (with-slots (donjon atk-chara move-chara long-atk-chara) *game*
    (with-slots (enemies) donjon
      (loop :for e :in enemies
	    :do (with-slots (x y posx posy w h actioned-p move-p) e
		  (update-dash e p)
		  (setf posx (* x w)
			posy (* y h)
			move-p nil
			actioned-p nil)))
      (setf move-chara nil))))

;;プレイヤーダッシュ
(defun update-dash-player (p)
  (with-slots (up down right left) *keystate*
    (with-slots (x y posx posy pos w h dir dash-p img-h move-p) p
      (let* ((next (cond
		     (up  (setf dir +up+
				img-h +up+)
			  '(0 -1))
		     (down (setf dir +down+
				 img-h +down+)
			   '(0 1))
		     (left (setf dir +left+
				 img-h +left+)
			   '(-1 0))
		     (right  (setf dir +right+
				   img-h +right+)
			     '(1 0))))
	    (nextpos (mapcar #'+ pos next)))
	(cond
	  ((hit-hantei nextpos :walls t :blocks t :enemies t)
	   (setf dash-p nil
		 move-p nil))
	  (t (setf pos nextpos
		   x (car pos)
		   y (cadr pos)
		   posx (+ (* x *obj-w*) 4)
		   posy (* y *obj-h*))
	     ))))))

;;ダッシュ
(defun update-dash-move (p)
  (with-slots (dash-p dir) p
    (with-slots (up down right left) *keystate*
      (setf dash-p t)
      (loop :while dash-p
	    :do 
	       (update-dash-player p)
	       (player-hit-item p)
	    :when (null dash-p)
	      :return nil
	    :do
	       (update-dash-enemies p)))))


;;その場で向きだけ変える
(defun update-player-dir (p)
  (with-slots (dir img-h) p
    (with-slots (up down right left) *keystate*
      (cond
	(up (setf dir +up+
		  img-h +up+))
	(down (setf dir +down+
		    img-h +down+))
	(right (setf dir +right+
		     img-h +right+))
	(left (setf dir +left+
		    img-h +left+))))))
	
    


(defun moge-paint (hwnd)
  (with-double-buffering-2 (hdc hwnd)
    (with-compatible-dc (hmemdc hdc)
      (set-bk-mode hdc :transparent)
      (render-game hdc hmemdc hwnd))))

;;宝箱
(defmethod drop-item ((e chest))
  (let* ((n (random 12)))
    (cond
      ((>= 1 n 0) +arrow+)
      ((>= 4 n 2)  +potion+)
      ((>= 7 n 5) +armour+)
      ((>= 10  n 8) +sword+)
      ((>=  n 11) +hammer+))))

;;スライムのドロップ品
(defmethod drop-item ((e slime))
  (when (= 0 (random 12))
    +potion+))
;;バブルのドロップ品
(defmethod drop-item ((e bubble))
  (when (= 0 (random 10))
    +potion+))
;;ブリガンドのドロップ品
(defmethod drop-item ((e brigand))
  (when (> (random 10) 4)
    +arrow+))

(defmethod drop-item ((e orc))
  (let ((n (random 10)))
    (case n
      (0 +armour+)
      (1 +sword+)
      (2 +hammer+))))

(defmethod drop-item ((e hydra))
  (let ((n (random 10)))
    (case n
      (0 +armour+)
      (1 +potion+))))

(defmethod drop-item ((e dragon))
  (let ((n (random 10)))
    (case n
      ((0 1) +armour+)
      ((2 3) +sword+)
      (4 +potion+)
      )))

(defmethod drop-item ((e skeleton))
  (let ((n (random 12)))
    (case n
      (0 +armour+)
      (1 +sword+)
      (2 +potion+)
      )))

;;通常のドロップアイテム
(defmethod drop-item ((e yote1))
  (let* ((n (random 100)))
    (cond
      ((>= 9 n 0) +arrow+)
      ((>= 20 n 10)  +potion+)
      ((>= 36 n 26) +hammer+)
      ((>= 51  n 46) +sword+)
      ((>= 57  n 52) +armour+)
      (t nil))))

;;アイテム落ちそうな場所
(defun can-drop-pos (pos)
  (loop :for n :in *droppos*
     :for new = (mapcar #'+ pos n)
     :unless (hit-hantei new :blocks t :walls t :player t :items t)
     :collect new))

;;アイテムの靴を落とす
(defun enemy-drop-item (e)
  (with-slots (donjon) *game*
  (let* ((droppos (can-drop-pos (pos e))))
    (when droppos
      (loop
	 :repeat (max 1 (random (length droppos)))
	 :for pos :in droppos
	 :do
	   (let ((item (drop-item e)))
	     (when item
	       (let ((drop-item (make-instance 'obj :img item :level (level e)
					       :pos pos :posx (posx e) :posy (posy e)
					       :x (x e) :y (y e) :w 32 :h 32 :w/2 16 :h/2 16
					       :moto-w 32 :moto-h 32 :obj-type item)))
		 (push drop-item (items donjon))))
	     (when (eq (obj-type e) :chest)
	       (return))))))))
       ;;(setf (donjon-drop-item *map*) (cdr (donjon-drop-item *map*)))))))
;; (when (>= (random 5) 3)
	;;   (let ((drop-item (make-instance 'obj :img +potion+
	;; 				:x (x e) :y (y e) :w 32 :h 32 :w/2 16 :h/2 16
	;; 				:moto-w 32 :moto-h 32 :obj-type :potion)))
	;;     (push drop-item (donjon-objects *map*)))))))



;;死んだ敵の情報を消す
(defun delete-enemies ()
  (with-slots (donjon) *game*
  (loop for e in (enemies donjon)
     do (when (and (null (dmg e))
		   (dead e))
	  ;;(when (eq (obj-type e) :boss)
	  ;;  (go-ending))
	  (enemy-drop-item e)
	  (setf (enemies donjon)
		(remove e (enemies donjon) :test #'equal))))))



(defun start-name ()
  (setf (state *game*) :name
	(cursor *game*) 0))


;;ゲームを開始する
(defun start-game ()
  ;;(init-game)
  (create-maze)
  ;; (dotimes (i 290)
  ;;   (get-equip-item (p *game*) (drop-weapon (donjon *game*))))
  (setf (state *game*) :playing
	(cursor *game*) 0
	*start-time* (get-internal-real-time))
  (when (null (bgm-p *game*))
    (bgm-stop (bgm-alias *game*))))

(defun continue-game ()
  (init-game)
  (start-game))


(defun start-choose-difficulty ()
  (setf (state *game*) :choose-difficulty
	(cursor *game*) 0))

(defun restart-game ()
  (bgm-stop (bgm-alias *game*))
  (init-game)
  (start-choose-difficulty))

;;タイトル画面での操作
(defun update-title-and-ending-gamen (hwnd)
  (with-slots (up down enter z) *keystate*
    (with-slots (cursor state) *game*
      (cond
	(up
	 (cond
	   ((= cursor 1)
	    (setf cursor 0))
	   ((= cursor 0)
	    (setf cursor 1))))
	(down
	 (cond
	   ((= cursor 0)
	    (setf cursor 1))
	   ((= cursor 1)
	    (setf cursor 0))))
	((or enter z)
	 (if (= cursor 0)
	     (case state
	       (:ending (restart-game))
	       (:gameover (restart-game))
	       (otherwise
		(start-name)))
	     (send-message hwnd (const +wm-close+) nil nil)))))))

;;ゲームオーバー判定
(defun game-over? ()
  (with-slots (p state) *game*
    (when (dead p)
      (setf state :gameover))))

;;名前決め
(defun update-name-cursor ()
  (with-slots (cursor p state) *game*
    (with-slots (up down right left z x enter) *keystate*
      (cond
	(enter
	 (start-choose-difficulty))
	(x
	 (when (> (length (name p)) 0)
           (setf (name p)
                 (subseq (name p) 0 (1- (length (name p)))))))
	(z
	 (cond
	   ((= cursor 77)
	    (start-choose-difficulty))
	   ((= cursor 76)
	    (when (> (length (name p)) 0)
              (setf (name p)
                    (subseq (name p) 0 (1- (length (name p)))))))
	   ((> 6 (length (name p)))
	    (setf (name p)
		  (concatenate 'string (name p) (format nil "~a" (aref *aiueo* cursor)))))))
	(up
	 (cond
	   ((> 5 cursor)
	    (setf cursor (+ cursor 45)))
	   ((>= 52 cursor 50)
	    (setf cursor (+ cursor 25)))
	   ((>= 54 cursor 53)
	    (setf cursor (+ cursor 20)))
	   (t
	    (decf cursor 5))))
	(down
	 (cond
	   ((>= 49 cursor 45)
	    (setf cursor (- cursor 45)))
	   ((>= 77 cursor 75)
	    (setf cursor (- cursor 25)))
	   ((>= 74 cursor 73)
	    (setf cursor (- cursor 20)))
	   (t
	    (incf cursor 5))))
	(right
	 (cond
	   ((and  (> 30 cursor)
		  (= (mod cursor 5) 4))
	    (setf cursor (+ cursor 46)))
	   ((and  (> 50 cursor 30)
		  (= (mod cursor 5) 4))
	    (setf cursor (- cursor 4)))
	   ((and  (>  75 cursor 50)
		  (= (mod cursor 5) 4))
	    (setf cursor (- cursor 54)))
	   ((= 77 cursor)
	    (setf cursor 25))
	   (t
	    (incf cursor))))
	(left
	 (cond
	   ((and  (> 25 cursor)
		  (= (mod cursor 5) 0))
	    (setf cursor (+ cursor 54)))
	   ((and  (> 50 cursor 29)
		  (= (mod cursor 5) 0))
	    (setf cursor (+ cursor 4)))
	   ((and  (>=  74 cursor 50)
		  (= (mod cursor 5) 0))
	    (setf cursor (- cursor 46)))
	   ((= 75 cursor)
	    (setf cursor 29))
	   ((= 25 cursor)
	    (setf cursor 77))
	   (t
	    (decf cursor))))))))


(defmethod equip-select-item (p (item weapondesc))
  (with-slots (weapon) p
    (when (new item)
      (setf (new item) nil))
    (setf (equiped weapon) nil
	  weapon item
	  (equiped weapon) t)))

(defmethod equip-select-item (p (item armordesc))
  (with-slots (armor) p
    (when (new item)
      (setf (new item) nil))
    (setf (equiped armor) nil
	  armor item
	  (equiped armor) t)))

;;アイテム交換画面
(defun update-weaponchange ()
  (with-slots (left right down up z x esc w a) *keystate*
    (with-slots (cursor p state) *game*
      (with-slots (item item-page weapon) p
	(let* ((item-max (1- (length item)))
	       (now-page-min (* item-page *item-show-max*))
	       (now-page-max (+ now-page-min (1- *item-show-max*)))
	       (item-page-max (floor item-max *item-show-max*)))
	  (cond
	    ((or esc a)
	     (mapc #'(lambda (x) (setf (new x) nil)) item)
	     (setf state :playing
		   cursor 0
		   item-page 0))
	    (up
	     (cond
	       ((and (= item-page item-page-max)
		     (= cursor now-page-min))
		(setf cursor item-max))
	       ((= cursor now-page-min)
		(setf cursor now-page-max))
	       (t
		(decf cursor))))
	    (down
	     (cond
	       ((and (= item-page item-page-max)
		     (= cursor item-max))
		(setf cursor now-page-min))
	       ((= cursor now-page-max) 
		(setf cursor now-page-min))
	       (t
		(incf cursor))))
	    (right
	     (if (= item-page item-page-max)
		 (setf cursor (- cursor (* item-page *item-show-max*))
		       item-page 0)
		 (progn (incf item-page)
			(incf cursor *item-show-max*)
			(when (> cursor item-max)
			  (setf cursor item-max)))))
	    (left
	     (if (= item-page 0)
		 (progn (setf item-page item-page-max
                              cursor (+ cursor (* item-page *item-show-max*)))
			(when (> cursor item-max)
			  (setf cursor item-max)))
		 (progn (decf item-page)
			(decf cursor *item-show-max*))))
	    (x
	     (let ((select-item (nth cursor item)))
	       (when (null (equiped select-item))
		 (setf item (remove select-item item :test #'equal))
                 (let ((new-item-num (length item)))
                   (when (> *item-show-max* new-item-num)
                     (setf cursor (1- new-item-num)))))))
	    (z
	     (let ((select-item (nth cursor item)))
	       (equip-select-item p select-item)))
	    ))))))

;;難易度選択
(defun update-difficulty ()
  (with-slots (up down enter z) *keystate*
    (with-slots (donjon-name cursor state donjon diffculty) *game*
      (cond
	(up
	 (cond
	   ((= cursor 0)
	    (setf cursor 3))
	   (t (decf cursor))))
	(down
	 (cond
	   ((= cursor 3)
	    (setf cursor 0))
	   (t (incf cursor))))
	((or enter z)
	 (case cursor
	   (0
	    (setf (lastfloor donjon) 10
		  diffculty 0
                  donjon-name "モゲゾウ平原")
	    (change-bgm *d1-bgm*))
	   (1 (setf (lastfloor donjon) 30
		    diffculty 1
                    donjon-name "もげぞうの洞窟")
	    (change-bgm *d2-bgm*))
	   (2 (setf (lastfloor donjon) 50
		    diffculty 2
                    donjon-name "もげぞうの森")
	    (change-bgm *d3-bgm*))
	   (3 (setf (lastfloor donjon) 100
		    diffculty 3
                    donjon-name "もげぞうの迷宮")
	    (change-bgm *d4-bgm*)))
	 (start-game))))))

;;
(defun update-game ()
  (with-slots (state p donjon turn move-chara atk-chara long-atk-chara set-dmg-flag monster-atk-interval) *game*
    (with-slots  (action-p move-p dmg atk-now) p
      (with-slots (left right down up z x c shift w enter esc a ctrl) *keystate*
	(cond
	  ((not (or move-chara atk-now atk-chara long-atk-chara))
	   (cond
	     (enter
	      ;;(print 4)
	      (player-hit-door p))
	     ((and ctrl
		   (or  up down left right))
	      ;;(print 1)
	      (update-player-dir p))
	     (a
	      (setf state :weaponchange))
	     ((and shift
		   (or up down left right)
		   (= (poison-cnt p) 0))
	      (update-dash-move p))
	     ((or  up down left right z x c)
	      (update-player p)
	      (when action-p 
		(update-enemies)))))
	  (atk-now
	   (update-atk-anime p))
	  (move-chara
	   (update-chara-move move-chara)
	   (when long-atk-chara
	     (update-long-atk)))
	  ((or atk-chara long-atk-chara)
	   (when atk-chara
	     (unless set-dmg-flag
	       (loop :for e :in atk-chara
		     :do (set-damage e p (encode-rgb 255 123 89))
			 (when (eq (type-of e) 'hydra)
			   (set-hydra-head e)
			   (setf atk-chara (remove e atk-chara :test #'equal))))
	       (setf set-dmg-flag t))
	     (update-chara-atk atk-chara)
	     (unless atk-chara
	       (setf set-dmg-flag nil)))
	   (when long-atk-chara
	     (update-long-atk))))
	(delete-enemies)
        (update-damage-fonts)
	(game-over?)
	(update-anime p)
	(update-enemy-wait-anime)
	(update-get-item-text)
        (when action-p
          (setf action-p nil))))))

;;bgm on off切り替え
(defun bgm-onoff ()
  (with-slots (bgm-p bgm-alias) *game*
    (when (b *keystate*)
      (if bgm-p
	  (progn (bgm-stop bgm-alias)
		 (setf bgm-p nil))
	  (progn (bgm-play bgm-alias)
		 (setf bgm-p t))))))

(defun bgm-repeat ()
  (with-slots (bgm-p bgm-alias) *game*
    (when bgm-p
      (let ((st (bgm-status bgm-alias)))
	(when (equal st "stopped")
	  (bgm-play bgm-alias))))))

;;ゲームループ
(defun main-game-loop (hwnd)
  (with-slots (state) *game*
    (cond
      ((eq state :title)
       (update-title-and-ending-gamen hwnd))
      ((eq state :name)
       (update-name-cursor))
      ((eq state :weaponchange)
       (update-weaponchange))
      ((eq state :choose-difficulty)
       (update-difficulty))
      ((eq state :playing)
       (update-game))
      ((eq state :gameover)
       (update-title-and-ending-gamen hwnd))
      ((eq state :ending)
       (update-title-and-ending-gamen hwnd)))
    (bgm-onoff)
    (bgm-repeat)
    (init-keystate )
    (invalidate-rect hwnd nil nil)
    ;;(update-window hwnd)
    ))


(defun moge-timer (hwnd)
  (main-game-loop hwnd))

;;ウィンドウサイズ変更時に画像拡大縮小する
(defun change-screen-size (lp)
  (let* ((change-w (loword lp))
	 (change-h (hiword lp)))
    (setf *change-screen-w* change-w
	  *change-screen-h* change-h)))


;;クライアント領域を*client-w* *client-h*に設定
;; (defun set-client-size (hwnd)
;;   (let* ((rc (get-client-rect hwnd))
;;          (rw (get-window-rect hwnd))
;;          (new-w (+ *client-w* (- (- (rect-right rw) (rect-left rw))
;;                                (- (rect-right rc) (rect-left rc)))))
;;          (new-h (+ *client-h* (- (- (rect-bottom rw) (rect-top rw))
;;                                (- (rect-bottom rc) (rect-top rc))))))
;;     (set-window-pos hwnd nil 0 0 new-w new-h '(:no-move :no-zorder))))




(defun moge-create (hwnd)
  (set-timer :hwnd hwnd :elapse 20 :replace-timer 1)
  (setf *name* nil)
  (set-brush)
  (set-font)
  (init-bgm)
  (load-images)
  (init-game)
  (load-save-clear-time)
  ;;(set-client-size hwnd)
  (bgm-play (bgm-alias *game*))
  (setf *c-rect* (get-client-rect hwnd)))


;;proc
(defwndproc moge-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (moge-create hwnd))
    ((const +wm-paint+)
     (moge-paint hwnd))
    ((const +wm-size+)
     (change-screen-size lparam))
    ((const +wm-close+)
     (destroy-window hwnd))
    ((const +wm-timer+)
     (moge-timer hwnd))
    ((const +wm-keydown+)
     (moge-keydown hwnd wparam))
    ((const +wm-keyup+)
     (moge-keyup wparam))
    ((const +wm-destroy+)
     (delete-brush)
     (delete-images)
     (delete-font)
     (close-bgms)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


;;メイン
(defun moge ()
  (setf *random-state* (make-random-state t))
  (register-class "MOGE" (callback moge-wndproc)
		  :styles (logior-consts +cs-hredraw+ +cs-vredraw+)
                  :cursor (load-cursor :arrow)
                  :background (create-solid-brush (encode-rgb 0 0 0)))
  (let ((hwnd (create-window "MOGE"
                             :window-name "もげぞうの迷宮"
                             :ex-styles  (logior-consts +ws-ex-composited+) ;;透明
                             :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                             :x 400 :y 100 :width *screen-w* :height *screen-h*))
        (msg (make-msg)))
    ;;(init-game)
    (unwind-protect
	 (progn
	   (show-window hwnd)
	   (update-window hwnd)
	   (do ((done nil))
               (done)
	     (let ((r (peek-message msg :remove-msg :remove :error-p nil)))
               (cond
		 (r
		  (cond 
		    ((= (msg-message msg) (const +wm-quit+))
		     (setf done t))
		    (t
		     (translate-message msg)
		     (dispatch-message msg))))
		 (t
		  (main-game-loop hwnd)
		  (sleep 0.01)))))
	   (msg-lparam msg))
      (unregister-class "MOGE"))))
