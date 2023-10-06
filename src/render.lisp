(in-package :mazeofmogezou)

;;transparent-blt
(defun trans-blt (x y w-src h-src w-dest h-dest)
  (transparent-blt *hmemdc* x y *hogememdc* 0 0 :width-source w-src
		   :height-source h-src
		   :width-dest w-dest :height-dest h-dest
		   :transparent-color (encode-rgb 0 255 0)))

(defun new-trans-blt (x y x-src y-src w-src h-src w-dest h-dest hdc hmemdc)
  (transparent-blt hdc x y hmemdc x-src y-src :width-source w-src
		   :height-source h-src
		   :width-dest w-dest :height-dest h-dest
		   :transparent-color (encode-rgb 0 255 0)))




;;アニメ表示
(defun render-enemy (e hdc hmemdc)
  (when (null (dead e)) ;;死んでなかったら表示
    (select-object hmemdc *anime-monsters-img*)
    (let ((x (+ (posx e) (atk-pos-x e)))
	  (y (+ (posy e) (atk-pos-y e))))
      (new-trans-blt x y  (img e) (* *obj-h* (img-h e))
		     (moto-w e) (moto-h e) (moto-w e) (moto-h e)  hdc hmemdc))))
      ;; (bit-blt hdc x y hmemdc (* (moto-w e) (img e)) (* *obj-h* (img-h e))
      ;; 		     :width (moto-w e) :height (moto-h e) :raster-op :srccopy))))

;;敵表示
(defun render-enemies (hdc hmemdc)
  (with-slots (donjon) *game*
  (loop for e in (enemies donjon)
     do (Render-enemy e hdc hmemdc))))


(defun render-get-item (hdc)
  (with-slots (p) *game*
  (with-slots (getitem) p
    (when getitem
      (with-slots (posx posy name) getitem
	(select-object hdc *font30*)
	(set-text-color hdc (encode-rgb 0 0 0))
	(text-out hdc (format nil "~a" name) (- posx 2) posy)
	(text-out hdc (format nil "~a" name) (+ posx 2) posy)
	(text-out hdc (format nil "~a" name) posx (- posy 2))
	(text-out hdc (format nil "~a" name) posx (+ posy 2))
	;;
	(set-text-color hdc (encode-rgb 255 255 255))
	(text-out hdc (format nil "~a" name) posx posy))))))


;;プレイヤー表示
(defun render-player (hdc hmemdc)
  (with-slots (p) *game*
    (with-slots (posx posy atk-pos-x atk-pos-y img img-h w h moto-w moto-h atk-now dir) p
      (select-object hmemdc *p-img*)
      (let ((x (+ posx atk-pos-x ))
	    (y (+ posy atk-pos-y )))
	(new-trans-blt x y  img (* *p-h* img-h)
		       w h moto-w moto-h hdc hmemdc)
	(when atk-now
	  (select-object hmemdc *buki-img*)
	  (cond
	    ((= dir +right+) (incf x 16))
	    ((= dir +left+) (decf x 16))
	    ((= dir +up+)   (decf y 16))
	    ((= dir +down+) (incf y 16)))
	  (new-trans-blt x y  (* (floor img 24) 28) (* *p-h* dir)
		       28 h 28 moto-h hdc hmemdc))))))
    ;; (bit-blt hdc x y hmemdc (* *p-w* (img *p*)) (* *p-h* (p-dir-num))
    ;; 		   :width (moto-w *p*) :height (moto-h *p*) :raster-op :srccopy)))


(defun render-arrow (hdc hmemdc)
  (with-slots (long-atk-chara) *game*
    (when long-atk-chara
      (loop :for arrow :in long-atk-chara
	    :do (select-object hmemdc (img-src arrow))
		(new-trans-blt (posx arrow) (posy arrow) (* *obj-w* (img arrow)) (* *obj-h* (img-h arrow))
			       *obj-w* *obj-h* *obj-w* *obj-h* hdc hmemdc)))))

;;*objs-img*の描画
(defun render-objs-img (pos img hdc hmemdc)
  (select-object hmemdc *objs-img*)
  (let ((x (* (car pos) *obj-w*))
        (y (* (cadr pos) *obj-h*)))
    (new-trans-blt x y (* *obj-w* img) 0
                   *obj-w* *obj-h*  *obj-w* *obj-h* hdc hmemdc)))
;; (bit-blt hdc x y hmemdc (* *obj-w* img) 0
;; 		   :width *obj-w* :height *obj-h* :raster-op :srccopy)))


(defun render-field (hdc hmemdc)
  (with-slots (donjon) *game*
  (with-slots (blocks walls yuka door) donjon
    (loop :for y :in yuka
       :do (render-objs-img y +yuka+ hdc hmemdc))
    (loop :for b :in blocks
       :do (render-objs-img b +soft-block+ hdc hmemdc))
    (loop :for w :in walls
	  :do (render-objs-img w +hard-block+ hdc hmemdc))
    (when door
      (render-objs-img (pos door) (img door) hdc hmemdc)))))


;;鍵とか描画
(defun render-item (hdc hmemdc)
  (with-slots (donjon) *game*
    (loop for obj in (items donjon)
          do (render-objs-img (pos obj) (img obj) hdc hmemdc))))




(defun render-bar (hdc left top bot hp-right decreased-hp-right &key (hp-color +green+)
								    (dec-hp-color +red+))
  ;;残りHP
  (select-object hdc (aref *brush* hp-color))
  (rectangle hdc left top hp-right bot)
  ;;減ったHP
  (select-object hdc (aref *brush* dec-hp-color))
  (rectangle hdc hp-right top decreased-hp-right bot))


;;経験値表示
(defun render-exp-bar (exp lvup-exp num hdc)
  (let* ((len (floor (* (/ exp lvup-exp) *bukiexpbar-max*)))
	 (left (+ *map-w* 10))
	 (bottom num)
	 (top (- bottom 15))
	 (hp-right (+ left len))
	 (dec-hp-right (+ hp-right (- *bukiexpbar-max* len))))
    (render-bar hdc left top bottom hp-right dec-hp-right)))

;;HPバー表示
(defun render-hp-bar (e hdc)
  (let* ((hp (floor (* (/ (hp e) (maxhp e)) *hpbar-max*)))
	 (left (* (car (pos e)) *obj-w*))
	 (bot (* (cadr (pos e)) *obj-h*))
	 (top (- bot 8))
	 (hp-right (+ left hp))
	 (decreased-hp (- *hpbar-max* hp))
	 (decreased-hp-right (+ hp-right decreased-hp)))
    (render-bar hdc left top bot hp-right decreased-hp-right)))


;;プレイヤーのステータス表示
(defun render-p-status (hdc hmemdc)
  (with-slots (p donjon bgm-p donjon-name) *game*
    (with-slots (name level hp maxhp str def hammer weapon armor potion arrow-num orb expe
		 poison-cnt lvup-exp) p
      (let* ((num 10)
	     (bgm (if bgm-p "playing" "stop"))
	     (hp-bar-color (if (> poison-cnt 0) +purple+ +green+))
	     (left (+ *map-w* 10))
	     (hp-bar-max 100)
	     (nokori-hp (floor (* (/ hp maxhp) hp-bar-max)))
	     (nokori-hp-right (+ nokori-hp left))
	     (decreased-hp (- hp-bar-max nokori-hp))
	     (dec-hp-right (+ decreased-hp nokori-hp-right))
	     (time-now (get-internal-real-time))
	     (time1 (- time-now *start-time*)))
	(multiple-value-bind (h m s ms) (get-hms time1)
	  (render-bar hdc left  85 100 nokori-hp-right dec-hp-right :hp-color hp-bar-color)
	  (macrolet ((hoge25 (n)
		       `(incf ,n 25))
		     (hoge40 (n)
		       `(incf ,n 40))
		     (hoge30 (n)
		       `(incf ,n 30)))
	    (select-object hdc *font20*)
	    (set-text-color hdc (encode-rgb 255 255 255))
	    ;;(set-bk-mode hdc :transparent)
	    (text-out hdc (format nil "~a" name) left num)
	    (text-out hdc (format nil "Lv:~2d" level) left (hoge25 num))
	    (text-out hdc (format nil "HP:~2d/~2d" hp maxhp) left (hoge25 num))
	    
	    (text-out hdc (format nil "攻:~2d" str) left (hoge40 num))
	    (text-out hdc (format nil "防:~2d" def) left (hoge25 num))
	    (text-out hdc (format nil "exp") left (hoge25 num))
	    (render-exp-bar expe lvup-exp (+ (hoge25 num) 15) hdc)
	    ;;hammer
	    (hoge25 num)
	    (select-object hmemdc *objs-img*)
	    (new-trans-blt (+ *map-w* 10) num (* *obj-w* +hammer+) 0
			   32 32 32 32 hdc hmemdc)
	    (text-out hdc (format nil "x ~d" hammer)  (+ *map-w* 50) (- (hoge30 num) 25))
	    ;;武器
	    ;;(hoge25 num)
	    (select-object hmemdc *objs-img*)
	    (new-trans-blt left num (* *obj-w* +sword+) 0
			   32 32 32 32 hdc hmemdc)
	    (text-out hdc (format nil "~a" (name weapon))  (+ *map-w* 50) (- (hoge30 num) 20))
	    ;;防具
	    (select-object hmemdc *objs-img*)
	    (new-trans-blt left num (* *obj-w* +armour+) 0
			   32 32 32 32 hdc hmemdc)
	    (text-out hdc (format nil "~a" (name armor))  (+ *map-w* 50) (- (hoge30 num) 20))
	    ;;potion
	    (new-trans-blt left num (* *obj-w* +potion+) 0
			   32 32 32 32 hdc hmemdc)
	    (text-out hdc (format nil "x ~d  (c)" potion)  (+ *map-w* 50) (- (hoge25 num) 20))
	    ;;arrow
	    (new-trans-blt left (+ num 10) (* *obj-w* +arrow+) 0
			   32 32 32 32 hdc hmemdc)
	    (text-out hdc (format nil "x ~d  (x)" arrow-num)  (+ *map-w* 50) (- (hoge25 num) 10))
	    ;;orb
	    (new-trans-blt left (+ num 20) (* *obj-w* +orb+) 0
			   32 32 32 32 hdc hmemdc)
	    (text-out hdc (format nil "x ~d" orb)  (+ *map-w* 50) (hoge25 num))
	    ;;(text-out hdc (format nil "w:~2d" *change-screen-w*) (+ *map-w* 10) 250)
	    ;;(text-out hdc (format nil "h:~2d" *change-screen-h*) (+ *map-w* 10) 290)
	    (select-object hdc *font30*)
	    (set-text-color hdc (encode-rgb 15 185 125))
            
	    (text-out hdc (format nil "~a ~2,'0d階" donjon-name (stage donjon)) 10 (+ *map-h* 5))
            (set-text-color hdc (encode-rgb 255 255 255))
	    (text-out hdc (format nil "~2,'0d:~2,'0d:~2,'0d:~2,'0d" h m s ms) 200 (+ *map-h* 5))
            (set-text-color hdc (encode-rgb 125 125 255))
	    (text-out hdc (format nil "※shift+方向キーでダッシュ ctrl+方向キーで向き変更")
		      10 (+ *map-h* 30))
            (set-text-color hdc (encode-rgb 255 255 0))
	    (text-out hdc (format nil "※壁に攻撃(z)すると壊せる(要:ハンマー)") 10 (+ *map-h* 50))
            (set-text-color hdc (encode-rgb 255 0 255))
	    (text-out hdc (format nil "※zで攻撃") 10 (+ *map-h* 70))
            (set-text-color hdc (encode-rgb 0 255 255))
	    (text-out hdc (format nil "※aで装備変更画面") 10 (+ *map-h* 90))
            (set-text-color hdc (encode-rgb 255 255 255))
        (select-object hdc *font20*)
	    (text-out hdc (format nil "※BGMのON/OFF(b)") 610 (+ *map-h* 5))
	    (text-out hdc (format nil "   now:~a" bgm) 610 (+ *map-h* 30))
	    (text-out hdc (format nil "※ゲーム終了(esc)") 610 (+ *map-h* 60))
	    ;;(when (door p)
	      (set-text-color hdc (encode-rgb 0 255 255))
	      (text-out hdc (format nil "ドアの上でEnterで次の階層へ") 350 (+ *map-h* 5))
	    ))))))


;;バックグラウンド
(defun render-background (hdc)
  (select-object hdc (get-stock-object :black-brush))
  (rectangle hdc 0 0 *change-screen-w* *change-screen-h*))

;;マップを表示
(defun render-donjon (hdc hmemdc)
  (render-background hdc)
  (render-field hdc hmemdc)
  ;; (render-yuka)
  ;; (render-block)
  (render-item hdc hmemdc))



    

;;ダメージ表示
(defun render-damage (d hdc)
  (with-slots (color dmg-num posx posy) d
    (select-object hdc *font20*)
    ;;縁取り
    (set-text-color hdc (encode-rgb 0 0 0))
    (text-out hdc (format nil "~d" dmg-num) (- posx 2) posy)
    (text-out hdc (format nil "~d" dmg-num) (+ posx 2) posy)
    (text-out hdc (format nil "~d" dmg-num) posx (- posy 2))
    (text-out hdc (format nil "~d" dmg-num) posx (+ posy 2))
    ;;
    (set-text-color hdc color)
    (text-out hdc (format nil "~d" dmg-num) posx posy)
    ))

;;全てのダメージ表示
(defun render-all-damage (hdc)
  (with-slots (dmg) *game*
    (loop for d in dmg
	  do (with-slots (color) d
	       (render-damage d hdc)))))

(Defun render-monster-hp-bar (hdc)
  (with-slots (donjon) *game*
    (with-slots (enemies) donjon
      (loop :for e :in enemies
	    :do (with-slots (maxhp hp dead) e
		  (when (and (/= (maxhp e) (hp e))
			     (null (dead e)))
		    (render-hp-bar e hdc)))))))


;;test
(defun render-test ()
  (select-object *hogememdc*  *anime-monsters-img*)
  (transparent-blt *hmemdc* 0 0 *hogememdc* 0 32 :width-source 32
		   :height-source 32
		   :width-dest 32 :height-dest 32
		   :transparent-color (encode-rgb 0 255 0)))

;;タイトル画面
(defun render-title-gamen (mes1 mes2 hdc hmemdc hwnd)
  (with-slots (p state donjon cursor) *game*
  (select-object hdc *font140*)
  
  (set-text-color hdc (encode-rgb 0 155 255))
  (text-out hdc (format nil "~a" mes1) 60 50)
  (when (eq state :gameover)
    (text-out hdc (format nil "~3d階で力尽きた" (stage donjon)) 10 200))
  (select-object hmemdc *objs-img*)
  (if (= cursor 0)
      (bit-blt hdc 280 360 hmemdc (* 32 +cursor+) 0 :width 32 :height 32 :raster-op :srccopy)
      (bit-blt hdc 280 410 hmemdc (* 32 +cursor+) 0 :width 32 :height 32 :raster-op :srccopy))
  (select-object hdc *font40*)
  (set-text-color hdc (encode-rgb 255 255 255))
  ;;(let ((r (get-client-rect hwnd)))
  ;;  (text-out hdc (format nil "w:~d h:~d" *change-screen-w* *change-screen-h*) 330 260)
  ;;  (text-out hdc (format nil "cw:~d ch:~d" (rect-right r) (rect-bottom r)) 330 310))
  (text-out hdc (format nil "~a" mes2) 330 360)
  (text-out hdc (format nil "おわる") 330 410)))



;;エンディング画面
(defun render-ending-gamen (hdc hmemdc)
  (with-slots (p endtime cursor donjon) *game*
    (let ((time1 (- endtime *start-time*))
	  (donjonname (case (lastfloor donjon)
			(10 "モゲゾウ平原")
			(30 "もげぞうの洞窟")
			(50 "もげぞうの森")
			(100 "もげぞうの迷宮"))))
    (multiple-value-bind (h m s ms) (get-hms time1)
      (render-background hdc)
      (select-object hdc *font70*)
      (set-text-color hdc (encode-rgb 0 155 255))
      (text-out hdc (format nil "~a は" (name p)) 10 10)
      (text-out hdc (format nil "~aを制覇した！" donjonname) 100 100)
      (select-object hdc *font70*)
      (set-text-color hdc (encode-rgb 255 255 255))
      (text-out hdc (format nil "クリアタイム") 270 200)
      (text-out hdc (format nil  "~2,'0d 時間 ~2,'0d 分 ~2,'0d 秒 ~2,'0d" h m s ms) 100 280)
      (select-object hmemdc *objs-img*)
      (if (= cursor 0)
	  (new-trans-blt 280 400 (* 32 +cursor+) 0 32 32 32 32 hdc hmemdc)
	  (new-trans-blt 280 450 (* 32 +cursor+) 0 32 32 32 32 hdc hmemdc))
      (select-object hdc *font40*)
      (set-text-color hdc (encode-rgb 255 255 255))
      (text-out hdc (format nil "もう一度やる") 330 400)
      (text-out hdc (format nil "おわる") 330 450)))))

;;名前入力画面
(defun set-name-gamen (hdc)
  (with-slots (cursor p) *game*
  (render-background hdc)
  (select-object hdc *font40*)
  (set-bk-mode hdc :transparent)
  (set-text-color hdc (encode-rgb 0 155 255))
  (text-out hdc "※名前を入力してください （6文字まで）" 100 10)
  (text-out hdc "zキーで入力！" 300 350)
  (text-out hdc "消 or xキーで一文字削除！" 300 400)
  (text-out hdc "決 or Enterキーでゲームスタート！" 300 450)
  (let* ((hpen (create-pen :solid (encode-rgb 255 255 255) 5))
	 (holdpen (select-object hdc hpen)))
    (select-object hdc (get-stock-object :null-brush ))
    (rectangle hdc 290 350 780 492)
    (rectangle hdc 520 60 780 200)
    (select-object  hdc holdpen)
    (delete-object hpen))
  (let ((x 0) (y 0) (xx 50))
    (loop :for i :across *aiueo*
	 :for cursor-temp :from 0
       :with m = 0
       :do
	 (when (equal i #\が)
	   (setf xx 300
		 y 0
		 m 0))
	 (cond
	   ((zerop (mod m 5))
	    (setf x xx
		  y (+ y 50)
		  m 0))

	   (t (setf x (+ x 40))))
	 (incf m)
	 (when (= cursor-temp cursor)
	   (select-object hdc (get-stock-object :white-brush))
	   (rectangle hdc x y (+ x 40) (+ y 40)))
	 (select-object hdc *font40*)
	 (text-out hdc (format nil "~a" i) x y))
    (text-out hdc (format nil "名前") 530 60)
    (text-out hdc (format nil "~a" (name p)) 530 100))))



(defun render-item-name (x1 x2 y1 y2 buki strx stry num hdc &key (font *font40*))
  (with-slots (cursor) *game*
  (select-object hdc font)
  (if (= cursor num) 
      (progn (select-object hdc (get-stock-object :white-brush))
	     (rectangle hdc x1 y1 x2 y2)
	     (set-text-color hdc (encode-rgb 0 0 0)))
      (progn ;;(select-object hmemdc *waku-img*)
	     ;;(new-trans-blt x1 y1 0 0 128 128 (- x2 x1) (- y2 y1) hdc hmemdc)
	(set-text-color hdc (encode-rgb 255 255 255))))
    (if (= (level buki) 0)
	(text-out hdc (format nil "~a" (name buki)) strx stry)
	(text-out hdc (format nil "~a+~d" (name buki) (level buki)) strx stry))))


;; (defun create-render-button-no-waku (x1 x2 y1 y2 str strx stry &key (font *font40*))
;;   (select-object *hmemdc* font)
;;   ;;(set-bk-mode *hmemdc* :transparent)
;;   (if (and (>= x2 (x *mouse*) x1)
;; 	   (>= y2 (y *mouse*) y1))
;;       (progn (select-object *hmemdc* (get-stock-object :white-brush))
;; 	     (rectangle *hmemdc* x1 y1 x2 y2)
;; 	     (set-text-color *hmemdc* (encode-rgb 0 0 0)))
;;       (progn 
;; 	     (set-text-color *hmemdc* (encode-rgb 255 255 255))))
;;   (text-out *hmemdc* (format nil "~a" str) strx stry))


;;装備変更画面の説明文
(defun render-description-item-gamen (hdc)
  (with-slots (cursor p) *game*
    (with-slots (item item-page) p
      (let* ((item-max (1- (length item)))
	     (item-page-max (floor item-max *item-show-max*)))
    (select-object hdc *font20*)
    (set-text-color hdc (encode-rgb 255 255 255))
    (text-out hdc "所持品リスト" 60 5)
    (text-out hdc "現在の武器" 270 5)
    (text-out hdc "現在の防具" 570 5)
    (text-out hdc "選択中のアイテム" 270 195)
    (text-out hdc "※捨てる:アイテムにカーソルを合わせてxキー" 270 430)
    (text-out hdc "装備中のアイテムは捨てることができません" 280 455)
    (text-out hdc (format nil "次→")  200 415)
    (text-out hdc (format nil "←前")  30 415)
    (text-out hdc (format nil "ページ ~d/~d" (1+ item-page) (1+ item-page-max)) 90 415)
    (text-out hdc (format nil "戻る( a )")  40 470)))))


;;持っている武器を表示 TODO 
(Defun render-weapon-list (hdc hmemdc)
  (with-slots (p) *game*
  (select-object hmemdc *waku-img*)
  (new-trans-blt 30 35 0 0 128 128 200 380 hdc hmemdc)
  (loop ;;:for buki :in (item *p*)
     :for x = 50
     :for y :from 45 :by 30
     :for b :from 0 :below *item-show-max*
     ;;:repeat 10
     :do (let* ((num (+ b (* (item-page p) *item-show-max*))))
	   (if (>= num (length (item p)))
	       (return)
	       (let ((buki (nth num (item p))))
		 (cond
		   ((new buki)
		    (select-object hdc *font2*)
		    (set-text-color hdc (encode-rgb 255 255 255))
		    (text-out hdc "N" 35 y)
		    (text-out hdc "e" 40 (+ y 8))
		    (text-out hdc "w" 45 (+ y 16)))
		   ((equiped buki)
		    (select-object hdc *font20*)
		    (set-text-color hdc (encode-rgb 255 255 255))
		    (text-out hdc "E" 34 y)))
		 (render-item-name x (+ x 170) y (+ y 25)
				   buki (+ x 4) y num hdc :font *font30*)))))))


;;カーソル位置にあるアイテムデータ表示
(defun render-selecting-item (hdc)
  (with-slots (p cursor ) *game*
    (with-slots (weapon armor) p
      (let* ((item (nth cursor  (item p)))
	     (y 200))
	(macrolet ((hoge (n)
		     `(incf ,n 30)))
	  (cond
	    ((and item
		  (eq (categoly item) :armor))
	     (text-out hdc (format nil "名前 : ~a" (name item)) 270 (hoge y))
	     (let ((defdiff (- (def item) (def armor))))
	       (cond
		 ((> defdiff 0)
		  (set-text-color hdc (encode-rgb 0 255 0))
		  (text-out hdc (format nil "防御力 : ~a  ↑ + ~d" (def item) defdiff) 270 (hoge y)))
		 ((> 0 defdiff)
		  (set-text-color hdc (encode-rgb 255 0 0))
		  (text-out hdc (format nil "防御力 : ~a  ↓ ~d" (def item) defdiff) 270 (hoge y)))
		 (t
		  (text-out hdc (format nil "防御力 : ~a" (def item)) 270 (hoge y)))))
	     (set-text-color hdc (encode-rgb 255 255 255))
	     (text-out hdc (format nil "ブロック率 : ~a%" (blk item)) 270 (hoge y)))
	    (item
	     (text-out hdc (format nil "名前 : ~a" (name item)) 270 (hoge y))
	     (let ((dmgdiff (- (damage item) (damage weapon))))
	       (cond
		 ((> dmgdiff 0)
		  (set-text-color hdc (encode-rgb 0 255 0))
		  (text-out hdc (format nil "攻撃力 : ~a  ↑ + ~d" (damage item) dmgdiff) 270 (hoge y)))
		 ((> 0 dmgdiff)
		  (set-text-color hdc (encode-rgb 255 0 0))
		  (text-out hdc (format nil "攻撃力 : ~a  ↓ ~d" (damage item) dmgdiff) 270 (hoge y)))
		 (t
		  (text-out hdc (format nil "攻撃力 : ~a" (damage item)) 270 (hoge y)))))
	     (set-text-color hdc (encode-rgb 255 255 255))
	     (text-out hdc (format nil "命中 : ~a" (hit item)) 270 (hoge y))
	     (text-out hdc (format nil "射程 : ~a～~a" (rangemin item)
				   (rangemax item)) 270 (hoge y))
	     (text-out hdc (format nil "会心 : ~a%" (critical item)) 270 (hoge y)))))))))

;;現在装備中のアイテム
(defun render-now-equip (hdc)
  (with-slots (p) *game*
  (with-slots (weapon armor) p
    (text-out hdc (format nil "名前    : ~a" (name weapon)) 270 40)
    (text-out hdc (format nil "攻撃力 : ~a" (damage weapon)) 270 70)
    (text-out hdc (format nil "命中    : ~a" (hit weapon)) 270 100)
    (text-out hdc (format nil "射程    : ~a～~a" (rangemin weapon)
			  (rangemax weapon)) 270 130)
    (text-out hdc (format nil "会心    : ~a%" (critical weapon)) 270 160)
    (text-out hdc (format nil "名前 : ~a" (name armor)) 570 40)
    (text-out hdc (format nil "防御力 : ~a" (def armor)) 570 70)
    (text-out hdc (format nil "ブロック率 : ~a%" (blk armor)) 570 100))))


;;武器変更画面
(defun render-weapon-change-gamen (hdc hmemdc)
  ;; (render-background )
  ;; (render-bgmonoff-button)
  (render-description-item-gamen hdc)
  (render-weapon-list hdc hmemdc)
  ;;(with-slots (selected) *mouse*
  (select-object hdc *font30*)
  (set-text-color hdc (encode-rgb 255 255 255))
  (render-now-equip hdc)
  (render-selecting-item hdc)
  (select-object hmemdc *waku-img*)
  (new-trans-blt 260 35 0 0 128 128 250 160 hdc hmemdc)
  (new-trans-blt 530 35 0 0 128 128 250 150 hdc hmemdc)
  (new-trans-blt 260 220 0 0 128 128 250 210 hdc hmemdc)
  )


(defun get-time-string (n)
  (multiple-value-bind (h m s ms) (get-hms n)
    (format nil "~2,'0d時間~2,'0d分~2,'0d秒~2,'0d" h m s ms)))

(defun render-choose-difficulty (hdc hmemdc)
  (with-slots (cursor) *game*
    (let ((don1 (nth 0 *donjon-clear-time-list*))
	  (don2 (nth 1 *donjon-clear-time-list* ))
	  (don3 (nth 2 *donjon-clear-time-list* ))
	  (don4 (nth 3 *donjon-clear-time-list* ))
	  (time-x 470)
	  (nashi-x 580))
      (select-object hdc *font40*)
      (set-text-color hdc (encode-rgb 255 255 255))
      (text-out hdc "難易度を選択してください" 50 10)
      (text-out hdc "最速クリアタイム" 500 40)
      (text-out hdc "モゲゾウ平原     (～10階)"  60 100)
      (if (> don1 0)
	  (text-out hdc (Get-time-string don1) time-x 100)
	  (text-out hdc "なし" nashi-x 100))
      (set-text-color hdc (encode-rgb 255 255 0))
      (text-out hdc "もげぞうの洞窟  (～30階)"  60 150)
      (if (> don2 0)
	  (text-out hdc (Get-time-string don2) time-x 150)
	  (text-out hdc "なし" nashi-x 150))
      (set-text-color hdc (encode-rgb 255 0 0))
      (text-out hdc "もげぞうの森      (～50階)"  60 200)
      (if (> don3 0)
	  (text-out hdc (Get-time-string don3) time-x 200)
	  (text-out hdc "なし" nashi-x 200))
      (set-text-color hdc (encode-rgb 255 0 255))
      (text-out hdc "もげぞうの迷宮 (～100階)" 60 250)
      (if (> don4 0)
	  (text-out hdc (Get-time-string don1) time-x 250)
	  (text-out hdc "なし" nashi-x 250))
      (select-object hmemdc *objs-img*)
      (let ((y (case cursor
                 (0 100)
                 (1 150)
                 (2 200)
                 (3 250))))
        (new-trans-blt 20 y (* 32 +cursor+) 0 32 32 32 32 hdc hmemdc))
      (select-object hmemdc *objs-img*)
      (new-trans-blt 380 400 (* *obj-w* +orb+) 0
                     32 32 32 32 hdc hmemdc)
      (set-text-color hdc (encode-rgb 0 254 0))
      (text-out hdc "最上階でオーブ     を手に入れよう！" 150 400)
      )))

;;ゲーム全体描画
(defun render-game (hdc hmemdc hwnd)
  (with-slots (state) *game*
  (cond
    ((eq state :title) ;;タイトル画面
     (render-title-gamen "もげぞうの迷宮" "はじめる" hdc hmemdc hwnd))
    ((eq state :name)
     (set-name-gamen hdc))
    ((eq state :choose-difficulty)
     (render-choose-difficulty hdc hmemdc))
    ((eq state :weaponchange)
     (render-weapon-change-gamen hdc hmemdc))
    ((eq state :playing) ;;ゲーム
     (render-donjon hdc hmemdc)
     (render-enemies hdc hmemdc)
     (render-player hdc hmemdc)
     (render-arrow hdc hmemdc)
     (render-all-damage hdc)
     (render-monster-hp-bar hdc)
     (render-p-status hdc hmemdc)
     (render-get-item hdc)
     )
     ;;(render-test)
    ((eq state :gameover)
     (render-title-gamen "ゲームオーバー" "もう一度やる" hdc hmemdc hwnd))
    ((eq state :ending) ;;エンディング画面
     (render-ending-gamen hdc hmemdc)
     ))))
