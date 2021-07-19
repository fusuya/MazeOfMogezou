(in-package :mazeofmogezou)
;;TODO 敵の攻撃
;;ブラシ生成
(defun set-brush ()
  (setf *brush* (make-array 7 :initial-contents
                              (list
                                (create-solid-brush (encode-rgb 128 0 160))
                                (create-solid-brush (encode-rgb 255 0 0))
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
  (with-slots (left right down up z x c enter shift w esc a b) *keystate*
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
					  :hp 30 :maxhp 30 :name *name* :atk-spd 10 :orb 0
					  :moto-w *p-w* :moto-h *p-h* :atk-now nil :ido-spd 2 :level 1
					  :w/2 (floor *obj-w* 2) :h/2 (floor *obj-h* 2) :hammer 3
					  :dir +down+ :item nil :armor nil :weapon  nil
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
  (with-slots (left right down up z x c enter shift w esc a b) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
	(:keya (setf a t))
	(:keyb (setf b t))
        (:left (setf left t))
	(:shift (setf shift t))
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
  (with-slots (left right down up z x c enter shift w esc a b) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
	(:keya (setf a nil))
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
      (equal (pos (door donjon)) pos))))

;;ボスを倒したら
(defun go-ending ()
  (setf (state *game*) :ending
	(endtime *game*) (get-internal-real-time)))

;;同じ武器が3つあったらレベルアップ
(defun auto-levelup-item (new-item p)
  (if (>= (count-if #'(lambda (x) (and (equal (name x) (name new-item))
					 (= (level x) (level new-item)))) (item p)) 2)
      (progn (setf (item p) (remove-if #'(lambda (x) (and (equal (name x) (name new-item))
							  (= (level x) (level new-item)))) (item p)))
	     (incf (level new-item))
	     (auto-levelup-item new-item p))
      (if (eq (type-of new-item) 'weapondesc)
	  (incf (damage new-item) (level new-item))
	  (incf (def new-item) (level new-item)))))

;;取得したアイテム名を表示するためのもの
(defun get-item (p drop-item-list)
  (let* ((drop (weightpick drop-item-list))
	 (new-item (shallow-copy-object (aref *weapondescs* drop))))
    (setf (new new-item) t
	  (getitem p) (make-instance 'itemtext :name (name new-item)
                                               :posx (max 0
							  (- (posx p)
							     (* (length (name new-item)) 10)))
				               :posy (posy p)
                                               :maxy (- (posy p) 20)))
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

;;プレイヤーとフロアにあるアイテムの当たり判定
(defun player-hit-item (p)
  (with-slots (donjon) *game*
    (with-slots (items stage drop-weapon drop-armor) donjon
      (let ((item (find (pos p) items :key #'pos :test #'equal)))
	(when item
	  (sound-play *get-item-wav*)
	  (cond
	    ((= (img item) +potion+)
	     (incf (potion p)))
	    ((= (img item) +sword+)
	     (get-item p drop-weapon))
	    ((= (img item) +armour+)
	     (get-item p drop-armor))
	    ((= (img item) +hammer+)
	     (incf (hammer p)))
	    ((= (img item) +arrow+)
	     (incf (arrow-num p)))
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

;;レヴェルアップ時ステータス上昇
(defun status-up (atker)
  (incf (maxhp atker) (1+ (random 3)))
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
(defun create-dmg-font (dmg-num dir defender)
  (with-slots (x y posx posy obj-type hp atk-spd) defender
    (let* ((dmg-x (+ posx 10))
	   (dmg-y (+ posy 15))
	   (x-dir (if (eq dir +left+) +left+ +right+))
	   (dmg (make-instance 'dmg-font :posx dmg-x :posy dmg-y :pos (list dmg-x dmg-y)
					 :dmg-num  dmg-num
					 :y-dir +up+ :x-dir x-dir
					 :maxy dmg-y :miny (- dmg-y 15))))
      dmg)))

;;ダメージ計算して表示する位置とか設定
(defun set-damage (atker defender)
  (with-slots (x y posx posy obj-type hp atk-spd) defender
    (let* ((dmg-num (damage-calc atker defender)))
      (if (> dmg-num 0)
	  (sound-play *damage-wav*)
	  (sound-play *guard*))
      (setf (dmg defender) (create-dmg-font dmg-num (dir atker) defender)) ;;ダメージを表示するためのもの
      (decf (hp defender) dmg-num) ;;hpを減らす
      (when (>= 0 (hp defender)) ;; hpが0以下になったら死亡
	(setf (dead defender) t)
	(player-get-exp atker defender))))) 





;;ダメージフォントの位置更新
(defun update-damage-font (e)
  (with-slots (dmg) e
    (when dmg
      (cond
	((eq +up+ (y-dir dmg))
	 (if (eq (x-dir dmg) +right+)
	     (incf (posx dmg))
	     (decf (posx dmg)))
	 (decf (posy dmg))
	 (when (= (posy dmg) (miny dmg))
	   (setf (y-dir dmg) +down+)))
	((eq +down+ (y-dir dmg))
	 (if (eq (x-dir dmg) +right+)
	     (incf (posx dmg))
	     (decf (posx dmg)))
	 (incf (posy dmg))
	 (when (= (posy dmg) (maxy dmg))
	   (setf dmg nil)))))))

;;ダメージフォントの位置更新
(defun update-damage-fonts (atk def)
  (when (zerop (mod (atk-c atk) 3))
    (update-damage-font def)))


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
  (when (zerop (mod (atk-c p) (atk-spd p)))
    (setf (atk-c p) 0)
    (cond
      ((= (dir p) +right+) (setf (atk-pos-x p) (update-atk-img-pos p (atk-pos-x p))))
      ((= (dir p) +left+)  (setf (atk-pos-x p) (- (update-atk-img-pos p (- (atk-pos-x p))))))
      ((= (dir p) +down+)  (setf (atk-pos-y p) (update-atk-img-pos p (atk-pos-y p))))
      ((= (dir p) +up+)    (setf (atk-pos-y p) (- (update-atk-img-pos p (- (atk-pos-y p)))))))))


;;攻撃アニメ終わるまでループ
(defun update-atk-anime (atk hwnd &key (def nil))
  (loop :while (atk-now atk)
     :do (incf (atk-c atk))
       (update-atk-img atk)
       (when def
	 (update-damage-fonts atk def))
       (invalidate-rect hwnd nil nil)
       (update-window hwnd))
  (when def
    (delete-damage-font def))
  (invalidate-rect hwnd nil nil)
  (update-window hwnd))





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
  (make-instance 'obj :img-h +dragon-fire+ :posx (posx p) :dir (dir p) :img 0
		      :x (x p) :y (y p)
		      :posy (posy p) :img-src *anime-monsters-img*
		      :pos (copy-list (pos p))))

(defun shot-fire (p)
  (with-slots (donjon) *game*
    (let ((fire (create-fire p)))
      (setf (arrow donjon) fire))))

;;矢
(defun create-arrow (p)
  (make-instance 'obj :img (dir p) :posx (posx p) :dir (dir p)
		      :x (x p) :y (y p)
		      :posy (posy p) :img-src *arrow-img*
		      :pos (copy-list (pos p))))


;;矢生成
(defun shot-arrow (p)
  (with-slots (donjon) *game*
  (let ((arrow (create-arrow p)))
    (setf (arrow donjon) arrow))
    ))

;;矢の位置更新
(defun set-next-arrow-pos ()
  (with-slots (donjon) *game*
  (with-slots (arrow) donjon
    (cond
      ((eq (dir arrow) +left+)
       (decf (posx arrow) 2)
       (setf (pos arrow) (list (1+ (floor (posx arrow) *obj-w*))
			       (floor (posy arrow) *obj-h*))))
      ((eq (dir arrow) +right+)
       (incf (posx arrow) 2)
       (setf (pos arrow) (list (floor (posx arrow) *obj-w*)
			       (floor (posy arrow) *obj-h*))))
      ((eq (dir arrow) +up+)
       (decf (posy arrow) 2)
       (setf (pos arrow) (list (floor (posx arrow) *obj-w*)
			       (1+ (floor (posy arrow) *obj-h*)))))
      ((eq (dir arrow) +down+)
       (incf (posy arrow) 2)
       (setf (pos arrow) (list (floor (posx arrow) *obj-w*)
			       (floor (posy arrow) *obj-h*))))))))

;;弓矢攻撃アニメ
(defun update-arrow-loop (p hwnd)
  (with-slots (donjon) *game*
  (with-slots (arrow) donjon
    (loop
       :do
	 (let ((tempos (copy-list (pos arrow))))
	   (set-next-arrow-pos)
	   (when (hit-hantei (pos arrow) :walls t :blocks t)
	     (push (make-instance 'obj :img +arrow+ :pos tempos)
		   (items donjon))
	     (setf arrow nil)
	     (return))
	   (let ((e (hit-hantei (pos arrow) :enemies t)))
	     (when e
	       (set-damage p e)
	       (sound-play *hit-arrow*)
	       (setf arrow nil)
	       (loop :while (dmg e)
		  :do (incf (atk-c p))
		    (update-damage-fonts p e)
		    (invalidate-rect hwnd nil nil)
		    (update-window hwnd))
	       (setf (atk-c p) 0)
	       (return)))
	   (invalidate-rect hwnd nil nil)
	   (update-window hwnd))))))


;;位置を戻す
(defun return-position (p tempos)
  (with-slots (pos x y posx posy) p
    (setf pos tempos
	  x (car pos)
	  y (cadr pos)
	  posx (* x *obj-w*)
	  posy (* y *obj-h*))))
  
;;プレイヤーの移動先当たり判定
(defun player-hit-hantei (p tempos hwnd)
  (with-slots (donjon) *game*
  (with-slots (pos hammer atk-now x y posx posy) p
    (let ((e (hit-hantei pos :enemies t))
	  (blo (hit-hantei pos :blocks t)))
      (cond
	((hit-hantei pos :enemies t)
	 (set-damage p e)
	 (setf atk-now t)
	 (return-position p tempos)
	 (sound-play  *atk-enemy-wav* )
	 (update-atk-anime p hwnd :def e))
	((and (hit-hantei pos :blocks t)
	      (> (hammer p) 0))
	 (decf (hammer p))
	 (setf atk-now t)
	 (return-position p tempos)
	 (sound-play  *atk-block-wav*)
	 (push (copy-list blo) (yuka donjon))
	 (setf (blocks donjon)
	       (remove blo (blocks donjon) :test #'equal))
	 (update-atk-anime p hwnd))
	((hit-hantei pos :walls t :blocks t)
	 (return-position p tempos)))))))


;;キー入力処理
(defun update-input-key (p hwnd)
  (with-slots (left right down up z c (keyx x) w shift) *keystate*
    (with-slots (x y posx posy pos arrow-num potion state hp maxhp dir poison-cnt) p
    (let ((tempos (copy-list (pos p))))
      (cond
	(c)
	(w
	 (format t "hogehoge~%")
	 (setf state :weaponchange))
	((and keyx
	      (> potion 0))
	 (sound-play *use-potion*)
	 (decf potion)
	 (setf hp maxhp
	       poison-cnt 0))
	((and z
	      (> arrow-num 0))
	 (sound-play *atk-arrow*)
	 (decf arrow-num)
	 (shot-arrow p)
	 (update-arrow-loop p hwnd))
	(left
	 (setf dir +left+)
	 (decf x))
	(right
	 (setf dir +right+)
	 (incf x))
	(up
	 (setf dir +up+)
	 (decf y))
	(down
	 (setf dir +down+)
	 (incf y)))
      (setf pos (list x y)
	    posx (* x *obj-w*)
	    posy (* y *obj-h*))
      (player-hit-hantei p tempos hwnd)))))

;;階層ごとにアイテムのドロップ率変更
(defun adjust-item-rate ()
  (with-slots (donjon) *game*
    (rate-decf 30 (drop-weapon donjon))
    (rate-decf 30 (drop-armor donjon))))

;;ドアとの当たり判定
(defun player-hit-door (p)
  (with-slots (donjon) *game*
  (with-slots (stage enemy-rate) donjon
    (if (hit-hantei (pos p) :door t)
	(progn
	  (setf (door p) t)
	  (when (enter *keystate*)
	    (setf (door p) nil)
	    (sound-play *door-wav*)
	    (incf stage)
	    (adjust-enemy-rate)
	    (adjust-item-rate)
	    (create-maze)))
	(setf (door p) nil)))))


;;ゲットしたアイテムのテキスト動かす
(defun update-get-item-text ()
  (with-slots (p) *game*
  (with-slots (getitem) p
    (when getitem
      (decf (posy getitem) 1)
      (when (> (maxy getitem) (posy getitem))
	(setf getitem nil))))))

;;poison-cntが0より大きい時毒ダメージ
(defun update-poison (p hwnd)
  (with-slots (poison-cnt hp dmg atk-c dead dash-p) p
    (when (> poison-cnt 0)
      (decf poison-cnt)
      (let ((poison-dmg (max 1 (random (max 1 (floor hp 10))))))
	(setf dmg (create-dmg-font poison-dmg +left+ p))
	(decf hp poison-dmg)
	(sound-play *poison*)
	(unless dash-p 
	  (loop :while dmg
		:do (incf atk-c)
		    (update-damage-fonts p p)
		    (invalidate-rect hwnd nil nil)
		    (update-window hwnd)))
	(setf atk-c 0)
	(when (>= 0 hp)
	  (setf dead t))))))

;;プレイヤーの色々更新
(defun update-player (p hwnd)
  (update-input-key p hwnd)
  (update-poison p hwnd)
  (player-hit-door p)
  (player-hit-item p))






;;移動可能場所生成
(defun where-to-go-next (e)
  (loop :for nextpos :in '((0 1) (0 -1) (1 0) (-1 0) (0 0))
     :unless (hit-hantei (mapcar #'+ (pos e) nextpos) :walls t :blocks t :enemies t)
     :collect nextpos))


;;ランダムで移動可能場所選ぶ
(defun update-enemy-pos-random (e)
  (let* ((next-list (where-to-go-next e)))
    (when next-list
      (let ((next (nth (random (length next-list)) next-list)))
	(setf (pos e) (mapcar #'+ (pos e) next)
	      (x e)    (car (pos e))
	      (posx e) (* (x e) *obj-w*)
	      (y e)    (cadr (pos e))
	      (posy e) (* (y e) *obj-h*))))))


;;敵がプレイヤーを追いかけてくる
(defun update-tracking-pos (e)
  (with-slots (p donjon) *game*
  (let ((next (astar (pos e) (pos p) (field donjon) *move-cost* (append (walls donjon) (blocks donjon)))))
    (let ((hit (hit-hantei next :walls t :blocks t :enemies t)))
      (when (or (null hit)
		(eq (obj-type hit) :chest))
	(setf (pos e) next
	      (x e)    (car (pos e))
	      (posx e) (* (x e) *obj-w*)
	      (y e)    (cadr (pos e))
	      (posy e) (* (y e) *obj-h*)))))))


;;となりにプレイヤー居たら殴る
(defun enemy-attack-neighbour (e p)
  (set-damage e p)
  (setf (atk-now e) t
	(dash-p p) nil))


;;スライムの行動
(defmethod update ((e slime) p hwnd)
  (setf (dir e) (check-neighbour e p))
  (cond
    ((dir e)
     (enemy-attack-neighbour e p)
     (update-atk-anime e hwnd :def p))
    ;;(print "moge"))
    (t
     (update-enemy-pos-random e))))

;;スライムの行動
(defmethod update ((e yote1) p hwnd)
  (setf (dir e) (check-neighbour e p))
  (cond
    ((dir e)
     (enemy-attack-neighbour e p)
     (update-atk-anime e hwnd :def p))
    (t
     (update-enemy-pos-random e))))



;;オークの行動
(defmethod update ((e orc) p hwnd)
  (setf (dir e) (check-neighbour e p))
  (cond
    ((dir e)
     (enemy-attack-neighbour e p)
     (update-atk-anime e hwnd :def p))
    ((>= (sight e) (manhatan (pos e) (pos p)))
     (update-tracking-pos e))
    (t
     (update-enemy-pos-random e))))


(defun check-hydra-around-player (e p)
  (loop :for x :in *around*
     :for npos = (mapcar #'+ (pos e) x)
     :when (equal (pos p) npos)
     :return t))


;;ヒドラの攻撃更新 ヒドラの周りを一周させる
(defun update-hydra-atk-anime (e p hwnd)
  (with-slots (donjon) *game*
  (let ((head (make-instance 'hydra :img 0 :obj-type :hydra-atk :img-h +hydra-atk+
			    :posx (- (posx e) 32) :posy (posy e) :str (str e)
			    :moto-w 32 :moto-h 32 :dir (dir e)

			    :w 32 :h 32 :w/2 16 :h/2 16)))
    (setf (centerx head) (* (+ (posx e) (w/2 e)) 1)
	  (centery head) (* (+ (posy e) (h/2 e)) 1))
    (push head (enemies donjon))
    ;; (invalidate-rect hwnd nil nil)
    ;; (update-window hwnd)
    (loop :while (> 360 (deg head))
       :do
	 (incf (atk-c head))
	 (when (zerop (mod (atk-c head) 30))
	   (let* ((radian  (/ (* (deg head) pi) 180))
	   	  (addx (floor (* (cos radian) 30)))
	   	  (addy (floor (* (sin radian) 30))))
	     (setf (posx head) (- (+ (centerx head) addx) (w/2 e))
	   	   (posy head) (- (+ (centery head) addy) (h/2 e)))
	     (incf (deg head) 5)
	     (when (zerop (mod (atk-c head) 60))
	       (update-damage-font p))
	   (invalidate-rect hwnd nil nil)
	   (update-window hwnd))))
    (setf (enemies donjon)
	  (remove head (enemies donjon) :test #'equal)))))


;;ヒドラの行動
(defmethod update ((e hydra) p hwnd)
  (cond
    ((check-hydra-around-player e p)
     (enemy-attack-neighbour e p)
     (update-hydra-atk-anime e p hwnd))
    ((>= (sight e) (manhatan (pos e) (pos p)))
     (update-tracking-pos e))
    (t
     (update-enemy-pos-random e))))



;;ブリガンドの弓矢攻撃
(defun update-long-atk-loop (e p hwnd)
  (with-slots (donjon) *game*
  (with-slots (arrow) donjon
    (loop
       :do
	  (set-next-arrow-pos)
	  (invalidate-rect hwnd nil nil)
	  (update-window hwnd)
       :when (equal (pos arrow) (pos p))
	 :do
	   (set-damage e p)
	   (if (eq (obj-type e) :brigand)
	       (sound-play *hit-arrow*)
	       (sound-play *hit-fire*))
	   (setf arrow nil)
	   (loop :while (dmg p)
	      :do (incf (atk-c e))
		(update-damage-fonts  e p)
		(invalidate-rect hwnd nil nil)
		(update-window hwnd))
	   (setf (atk-c e) 0)
	   (return))
	 )))


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

(defmethod enemy-long-attack ((e brigand) p nageru hwnd)
  (setf (dir e) nageru
	(dash-p p) nil)
  (shot-arrow e)
  (sound-play *atk-arrow*)
  (update-long-atk-loop e p hwnd))

;;ブリガンドの行動
(defmethod update ((e brigand) p hwnd)
  (let ((nageru (check-can-long-atk e p)))
    (setf (dir e) (check-neighbour e p))
    (cond
      ((dir e)
       (enemy-attack-neighbour e p)
       (update-atk-anime e hwnd :def p))
      (nageru
       (enemy-long-attack e p nageru hwnd))
      (t
       (update-enemy-pos-random e)))))






(defmethod enemy-long-attack ((e dragon) p nageru hwnd)
  (setf (dir e) nageru
	(dash-p p) nil)
  (shot-fire e)
  (sound-play *atk-fire*)
  (update-long-atk-loop e p hwnd))


;;ドラゴンの行動
(defmethod update ((e dragon) p hwnd)
  (let ((nageru (check-can-long-atk e p)))
    (setf (dir e) (check-neighbour e p))
    (cond
      ((dir e)
       (enemy-attack-neighbour e p)
       (update-atk-anime e hwnd :def p))
      (nageru
       (enemy-long-attack e p nageru hwnd))
      ((>= (sight e) (manhatan (pos e) (pos p)))
       (update-tracking-pos e))
      (t
       (update-enemy-pos-random e)))))


(defmethod update ((e chest) p hwnd)
  )

;;スケルトンの行動
(defmethod update ((e skeleton) p hwnd)
  (setf (dir e) (check-neighbour e p))
  (cond
    ((dir e)
     (enemy-attack-neighbour e p)
     (update-atk-anime e hwnd :def p))
    ((>= (sight e) (manhatan (pos e) (pos p)))
     (update-tracking-pos e))
    (t
     (update-enemy-pos-random e))))

;;バブルの行動
(defmethod update ((e bubble) p hwnd)
  (setf (dir e) (check-neighbour e p))
  (cond
    ((dir e)
     (enemy-attack-neighbour e p)
     (update-atk-anime e hwnd :def p)
     (when (= 0 (random 10)) ;;プレイヤーを毒状態にする
       (setf (poison-cnt p) 5)))
    ((>= (sight e) (manhatan (pos e) (pos p)))
     (update-tracking-pos e))
    (t
     (update-enemy-pos-random e))))

;;敵の位置更新
(defun update-enemies (hwnd)
  (with-slots (p donjon) *game*
  (loop for e in (enemies donjon)
     do (when (null (dead e))
	  (update-damage-font e)
	  (update e p hwnd)))))


(defun update-dash-player (p)
  (with-slots (up down right left) *keystate*
    (with-slots (x y posx posy pos w h dir dash-p) p
      (let* ((next (cond
		     (up  (setf dir +up+)
			  '(0 -1))
		     (down (setf dir +down+)
			   '(0 1))
		     (left (setf dir +left+)
			   '(-1 0))
		     (right  (setf dir +right+)
			     '(1 0))))
	    (nextpos (mapcar #'+ pos next)))
	(cond
	  ((hit-hantei nextpos :walls t :blocks t :enemies t)
	   (setf dash-p nil))
	  (t (setf pos nextpos
		   x (car pos)
		   y (cadr pos)
		   posx (* x *obj-w*)
		   posy (* y *obj-h*))
	     ))))))

;;ダッシュ
(defun update-dash-move (p hwnd)
  (with-slots (dash-p) p
    (with-slots (up down right left) *keystate*
      (setf dash-p t)
      (loop :while dash-p
	    :do 
	       (update-dash-player p)
	    :when (null dash-p)
	      :return nil
	    :do
	       (update-enemies hwnd)))))
	     


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
	 (when (> (length *name*) 0)
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
	   (setf state :playing))
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
		(setf item (remove select-item item :test #'equal)))))
	   (z
	    (let ((select-item (nth cursor item)))
	      (equip-select-item p select-item)))
	   ))))))

;;難易度選択
(defun update-difficulty ()
  (with-slots (up down enter z) *keystate*
    (with-slots (cursor state donjon) *game*
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
	    (setf (lastfloor donjon) 10)
	    (change-bgm *d1-bgm*))
	   (1 (setf (lastfloor donjon) 30)
	    (change-bgm *d2-bgm*))
	   (2 (setf (lastfloor donjon) 50)
	    (change-bgm *d3-bgm*))
	   (3 (setf (lastfloor donjon) 100)
	    (change-bgm *d4-bgm*)))
	 (start-game))))))

;;
(defun update-game (hwnd)
  (with-slots (state p donjon) *game*
    (with-slots (left right down up z x c shift w enter esc a) *keystate*
      (cond
	(a
	 (setf state :weaponchange))
	((and shift
	      (or up down left right)
	      (= (poison-cnt p) 0))
	 (update-dash-move p hwnd))
	((or left right down up z x c enter esc)
	 (update-player p hwnd)
	 (update-enemies hwnd)
	 (delete-enemies)
	 (game-over?)))
      (update-get-item-text))))

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
       (update-game hwnd))
      ((eq state :gameover)
       (update-title-and-ending-gamen hwnd))
      ((eq state :ending)
       (update-title-and-ending-gamen hwnd)))
    (bgm-onoff)
    (bgm-repeat)
    (init-keystate )
    (invalidate-rect hwnd nil nil)))


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
