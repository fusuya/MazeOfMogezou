(in-package :mazeofmogezou)


(defparameter *data-root* (asdf:system-source-directory 'mazeofmogezou))
(defparameter *img-root* (merge-pathnames "img/" *data-root*))
(defparameter *sound-root* (merge-pathnames "sound/" *data-root*))

(defmacro with-double-buffering-2 ((var hwnd) &body body)
  "Evaluate body in a WITH-PAINT context where VAR is bound to an in-memory HDC
which is blitted onto the hwnd's DC as the final step. This prevents flickering 
when drawing lots of small items on the screen."
  (alexandria:with-gensyms (gbm gold gwidth gheight ghdc gps)
    `(with-paint (,hwnd ,ghdc ,gps)
       (let ((,gwidth (rect-right *c-rect*))
		       ;;(paintstruct-paint ,gps)))
	     (,gheight (rect-bottom *c-rect*)))
			;;(paintstruct-paint ,gps))))
	 (with-compatible-dc (,var ,ghdc)
	   (let* ((,gbm (create-compatible-bitmap ,ghdc ,gwidth ,gheight))
		  (,gold (select-object ,var ,gbm)))
	     (unwind-protect (progn ,@body)
	       (stretch-blt ,ghdc 0 0 ,var 0 0
			:width-dest *change-screen-w*
			:height-dest *change-screen-h*
			:width-source (rect-right *c-rect*)
			:height-source (rect-bottom *c-rect*)
			:raster-op :srccopy)
	       ;; (transparent-blt ,ghdc 0 0 ,var 0 0 
	       ;; 			:width-dest *change-screen-w*
	       ;; 			:height-dest *change-screen-h*
	       ;; 			:width-source (rect-right *c-rect*)
	       ;; 			:height-source (rect-bottom *c-rect*)
	       ;; 			:transparent-color (encode-rgb 0 255 0))
	       
	       (select-object ,var ,gold)
	       (delete-object ,gbm))))))))


;;時間変換
(defun get-hms (n)
  (multiple-value-bind (h m1) (floor n 3600000)
    (multiple-value-bind (m s1) (floor m1 60000)
      (multiple-value-bind (s ms1) (floor s1 1000)
	(multiple-value-bind (ms) (floor ms1 10)
	  (values h m s ms))))))


;;class copy
(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))


(defmacro my-enum (&rest names)
  `(progn
     ,@(loop for i from 0
             for name in names
        collect `(defparameter ,name ,i))))


;;".\\images\\*.*" ロードした画像の配列を作る
(defun make-imgs-array (img-path)
  (let* ((img-list (mapcar #'namestring (directory img-path)))
         (imgs (make-array (length img-list))))
    (loop for str in img-list
          for i from 0
          do (setf (aref imgs i)
                   (load-image str :type :bitmap
                               :flags '(:load-from-file :create-dib-section))))
    imgs))

(defparameter *atk-width* 8)
(defparameter *atk-pos-max* (* *atk-width* 3)) 

(defparameter *p-img* nil)
(defparameter *p-atk-img* nil)
(defparameter *buki-img* nil)
(defparameter *hammer-img* nil)
(defparameter *monster-anime* nil)
(defparameter *objs-img* nil)
(defparameter *arrow-img* nil)

;;プレイヤー画像切り替えよう
(defconstant +down+ 0)
(defconstant +left+ 2)
(defconstant +right+ 3)
(defconstant +up+ 1)

;;敵画像切り替えよう
;; (defconstant +brigand-anime+ 0)
;; (defconstant +dragon-anime+ 1)
;; (defconstant +hydra-anime+ 2)
;; (defconstant +yote-anime+ 3)
;; (defconstant +orc-anime+ 4)
;; (defconstant +slime-anime+ 5)
;; (defconstant +boss-anime+ 11)
;; (defconstant +hydra-atk+ 6)
;; (defconstant +brigand-ball+ 7)
;; (defconstant +dragon-fire+ 8)
;; (defconstant +orc-atk+ 9)
;; (defconstant +chest+ 10)

(my-enum  +brigand-anime+ +dragon-anime+ +hydra-anime+ +yote-anime+ +orc-anime+
	  +slime-anime+ +bubble-anime+ +skeleton-anime+  +hydra-atk+ +brigand-ball+ +dragon-fire+ +orc-atk+ +chest+)

(defparameter *enemy-appear-rate* '((:slime . 250) (:orc . 180) (:skeleton . 150) (:bubble . 120)
				    (:brigand . 80) (:hydra . 40) (:dragon . 10) (:yote1 . 1)))


;;敵の攻撃演出時間
(defparameter *orc-atk-effect-time* 30)
(defparameter *hydra-atk-effect-time* 30)

;;透過用
(defcfun (%set-layered-window-attributes "SetLayeredWindowAttributes" :convention :stdcall)
         :boolean
  (hwnd :pointer)
  (crkey :int32)
  (balpha :uint8)
  (dwflags :uint32))

(defun set-layered-window-attributes (hwnd crkey balpha dwflags)
  (%set-layered-window-attributes hwnd crkey balpha dwflags))



;;(defparameter *tate* 11) ;;マップサイズ
;;(defparameter *yoko* 11)
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)

(defparameter *map* nil)
(defparameter *ido?* nil)
(defparameter *p* nil)
(defparameter *pt* nil)

(defparameter *battle?* nil)
(defparameter *monster-num* 10)
(defparameter *monster-level* 1) ;;階数によるモンスターのレベル
(defparameter *boss?* 0)
(defparameter *end* 0)
(defparameter *lv-exp* 100)
(defparameter *images* nil)
(defparameter *anime-monsters-img* nil)

(defvar *atk-block-wav*  (namestring (merge-pathnames "atk-block.wav"   *sound-root*)))
(defvar *atk-enemy-wav*  (namestring (merge-pathnames "atk-enemy.wav"   *sound-root*)))
(defvar *damage-wav*     (namestring (merge-pathnames "damage.wav"      *sound-root*)))
(defvar *door-wav*       (namestring (merge-pathnames "door.wav"        *sound-root*)))
(defvar *get-item-wav*   (namestring (merge-pathnames "get-item.wav"    *sound-root*)))
(defvar *get-potion-wav* (namestring (merge-pathnames "get-potion.wav"  *sound-root*)))
(defvar *lvup-wav*       (namestring (merge-pathnames "lvup.wav"        *sound-root*)))
(defvar *get-orb*        (namestring (merge-pathnames "get-orb.wav"     *sound-root*)))
(defvar *use-potion*     (namestring (merge-pathnames "use-potion.wav"  *sound-root*)))
(defvar *atk-arrow*      (namestring (merge-pathnames "atk-arrow.wav"   *sound-root*)))
(defvar *hit-arrow*      (namestring (merge-pathnames "hit-arrow.wav"   *sound-root*)))
(defvar *atk-fire*       (namestring (merge-pathnames "atk-fire.wav"    *sound-root*)))
(defvar *hit-fire*       (namestring (merge-pathnames "hit-fire.wav"    *sound-root*)))
(defvar *poison*         (namestring (merge-pathnames "poison.wav"      *sound-root*)))
(defvar *guard*          (namestring (merge-pathnames "guard.wav"       *sound-root*)))

(defvar *OP-bgm-path*    (namestring (merge-pathnames "MOM-OP.wav"      *sound-root*)))
(defvar *ED-bgm-path*    (namestring (merge-pathnames "MOM-ED.wav"      *sound-root*)))
(defvar *d1-bgm-path*    (namestring (merge-pathnames "MOM-d1.wav"      *sound-root*)))
(defvar *d2-bgm-path*    (namestring (merge-pathnames "MOM-d2.wav"      *sound-root*)))
(defvar *d3-bgm-path*    (namestring (merge-pathnames "MOM-d3.wav"      *sound-root*)))
(defvar *d4-bgm-path*    (namestring (merge-pathnames "MOM-d4.wav"      *sound-root*)))

;;bgm alias
(defvar *op-bgm* "opbgm")
(defvar *ed-bgm* "edbgm")
(defvar *d1-bgm* "d1bgm")
(defvar *d2-bgm* "d2bgm")
(defvar *d3-bgm* "d3bgm")
(defvar *d4-bgm* "d4bgm")


;;拡大
(Defparameter *mag-w* 1)
(Defparameter *mag-h* 1)

;;基本サイズ 元の画像サイズ
(defparameter *obj-w* 32)
(defparameter *obj-h* 32)

(defparameter *w/2* (floor *obj-w* 2))
(defparameter *h/2* (floor *obj-h* 2))

;;元のブロック画像のサイズ
(defparameter *blo-w* 32)
(defparameter *blo-h* 32)

;;炎サイズ
(defparameter *fire-w* 32)
(defparameter *fire-h* 32)
(defparameter *fire-w/2* (floor *fire-w* 2))
(defparameter *fire-h/2* (floor *fire-h* 2))

;;プレイヤーのサイズ
(defparameter *p-w* 24)
(defparameter *p-h* 32)
(defparameter *p-w/2* (floor *p-w* 2))
(defparameter *p-h/2* (floor *p-h* 2))





;;オブジェクト画像表示サイズ

;;
(defparameter *tate* 20)
(defparameter *yoko* 30)

(defparameter *yoko-block-num* 21)
(defparameter *tate-block-num* 13)

;;ゲームマップ領域
(defparameter *map-w* (* *yoko-block-num* *obj-w*))
(defparameter *map-h* (* *tate-block-num* *obj-h*))
;;プレイヤーのステータス表示用領域サイズ
(defparameter *status-w* 180)
(defparameter *status-h* 160)


(defparameter *screen-w* (+ *map-w* *status-w*))
(defparameter *screen-h* (+ *map-h* *status-h*))

(defparameter *change-screen-w* *screen-w*)
(defparameter *change-screen-h* *screen-h*)

(defparameter *waku-size* 10) ;;ゲームフィールドの周りの枠太さ
(defparameter *c-rect* nil) ;;クライアント領域

(defparameter *start-time* 0)
(defparameter *name* nil)
(defparameter *donjon* nil)

;;画面領域
(defparameter *client-w* (+ *map-w* 150))
;;(defparameter *client-h* (* *blo-h46* *tate*))

(defparameter *screen-center-x* nil)

(defparameter *brush* nil)
(defparameter *start* nil)
(defparameter *hmemDC* nil)
(defparameter *hbitmap* nil)


(defparameter *hogememDC* nil)
(defparameter *hogebitmap* nil)

(defparameter *kabe-break* nil)
(defparameter *HPbar-max* 40)
(defparameter *bukiexpbar-max* 100)

(defparameter *droppos* '((0 0) (0 1) (0 -1) (1 0) (-1 0) (1 1) (-1 -1) (-1 1) (1 -1)))
(defparameter *around* '((0 1) (0 -1) (1 0) (-1 0) (1 1) (-1 -1) (-1 1) (1 -1)))
(defparameter *tonari* '((0 1) (0 -1) (1 0) (-1 0)))
(defparameter *tonari-dir* '(:right :left :down :up))

(defparameter *waku-img* nil)
(defparameter *waku-ao* nil)
(defparameter *waku-aka* nil)

(defparameter *item-show-max* 12)

(defparameter *game* nil)

(defun gonyu (n)
  (multiple-value-bind (a b)
      (floor n 1)
    (if (>= b 0.5)
	(incf a)
	a)))

(defun load-images ()
  (setf *objs-img*           (load-image (namestring (merge-pathnames "objs-img4.bmp" *img-root*))
		 			 :type :bitmap
					 :flags '(:load-from-file :create-dib-section))
	*waku-img*           (load-image (namestring (merge-pathnames "waku.bmp" *img-root*))
					 :type :bitmap
					 :flags '(:load-from-file :create-dib-section))
	*arrow-img*          (load-image (namestring (merge-pathnames "arrow.bmp" *img-root*))
					 :type :bitmap
					 :flags '(:load-from-file :create-dib-section))
	*p-img*              (load-image  (namestring (merge-pathnames "p-ido-anime.bmp" *img-root*))
					  :type :bitmap
					  :flags '(:load-from-file :create-dib-section))
	*anime-monsters-img* (load-image (namestring (merge-pathnames "monsters.bmp" *img-root*))
					 :type :bitmap
					 :flags '(:load-from-file :create-dib-section))
	))

(my-enum +boots+ +door+ +hammer+ +hard-block+ +key+ +potion+ +soft-block+ +yuka+ +sword+ +cursor+ +armour+ +arrow+ +orb+)

(my-enum +purple+ +red+ +green+ +blue+ +yellow+ +cyan+ +pink+ )

(my-enum +arrow-right+ +arrow-left+ +arrow-down+ +arrow-up+)

(defclass game ()
  ((state     :accessor state     :initform nil :initarg :state)
   (bgm-p     :accessor bgm-p     :initform t   :initarg :bgm-p)
   (bgm-alias :accessor bgm-alias :initform nil :initarg :bgm-alias)
   (p         :accessor p         :initform nil :initarg :p)
   (donjon    :accessor donjon    :initform nil :initarg :donjon)
   (endtime   :accessor endtime   :initform nil :initarg :endtime)
   (cursor    :accessor cursor    :initform 0   :initarg :cursor)))
   
(defclass keystate ()
  ((right :accessor right :initform nil :initarg :right)
   (left  :accessor left  :initform nil :initarg :left)
   (down  :accessor down  :initform nil :initarg :down)
   (up    :accessor up    :initform nil :initarg :up)
   (enter :accessor enter :initform nil :initarg :enter)
   (shift :accessor shift :initform nil :initarg :shift)
   (esc   :accessor esc :initform nil :initarg :esc)
   (w     :accessor w     :initform nil :initarg :w)
   (b     :accessor b     :initform nil :initarg :b)
   (a     :accessor a     :initform nil :initarg :a)
   (z     :accessor z     :initform nil :initarg :z)
   (x     :accessor x     :initform nil :initarg :x)
   (c     :accessor c     :initform nil :initarg :c)))

(defparameter *keystate* (make-instance 'keystate))


;;ドロップアイテムリスト
(defparameter *drop-item*
  '(:boots :atkup :defup))

(defclass donjon ()
  ((field :accessor field :initform nil :initarg :field)  ;;マップ
   (tate :accessor tate :initform *tate* :initarg :tate)  ;;縦幅
   (yoko :accessor yoko :initform *yoko* :initarg :yoko)  ;;横幅
   (enemies :accessor enemies :initform nil :initarg :enemies)
   (path :accessor path :initform nil :initarg :path)
   (arrow :accessor arrow :initform nil :initarg :arrow)
   (lastfloor :accessor lastfloor :initform 0 :initarg :lastfloor)
   (name      :accessor name       :initform nil  :initarg :name)
   (yuka      :accessor yuka       :initform nil  :initarg :yuka) ;;床
   (walls     :accessor walls      :initform nil  :initarg :walls)
   (blocks    :accessor blocks     :initform nil  :initarg :blocks) ;;ブロック
   (items     :accessor items    :initform nil  :initarg :items) ;;アイテム
   (door      :accessor door       :initform nil   :initarg :door) 
   (stage     :accessor stage       :initform 1   :initarg :stage) 
   (stop-list :accessor stop-list  :initform nil  :initarg :stop-list)
   (enemy-rate :accessor enemy-rate  :initform nil  :initarg :enemy-rate) ;;敵出現率
   (drop-weapon :accessor drop-weapon  :initform nil :initarg :drop-weapon)
   (drop-armor :accessor drop-armor  :initform nil :initarg :drop-armor))) ;;行き止まりリスト


;;ブロックとか
(defclass obj ()
  ((x        :accessor x        :initform 0      :initarg :x)
   (y        :accessor y        :initform 0      :initarg :y)
   (x2       :accessor x2       :initform 0      :initarg :x2)
   (y2       :accessor y2       :initform 0      :initarg :y2)
   (w        :accessor w        :initform 0      :initarg :w)
   (h        :accessor h        :initform 0      :initarg :h)
   (posx     :accessor posx        :initform 0      :initarg :posx) ;;描画用x座標
   (posy     :accessor posy        :initform 0      :initarg :posy) ;;y
   (moto-w   :accessor moto-w   :initform 0      :initarg :moto-w)
   (moto-h   :accessor moto-h   :initform 0      :initarg :moto-h)
   (pos      :accessor pos      :initform 0      :initarg :pos)
   (w/2      :accessor w/2      :initform 0      :initarg :w/2)
   (h/2      :accessor h/2      :initform 0      :initarg :h/2)
   (obj-type :accessor obj-type :initform 0      :initarg :obj-type)
   (img-h    :accessor img-h    :initform 0      :initarg :img-h)
   (img-src  :accessor img-src    :initform 0      :initarg :img-src)
   (dir       :accessor dir       :initform :down :initarg :dir)     ;;現在の方向
   (level    :accessor level     :initform 1     :initarg :level)
   (img      :accessor img      :initform nil    :initarg :img)))

(defclass dmg-font (obj)
  ((dmg-num  :accessor dmg-num   :initform 0     :initarg :dmg-num)
   (miny     :accessor miny      :initform 0     :initarg :miny)
   (maxy     :accessor maxy      :initform 0     :initarg :maxy)
   (color    :accessor color     :initform 0     :initarg :color)
   (y-dir    :accessor y-dir     :initform :up   :initarg :y-dir)
   (x-dir    :accessor x-dir     :initform :left :initarg :x-dir)
   ))

(defclass buki (obj)
  ((atk  :accessor atk       :initform 0   :initarg :atk)
   (name :accessor name      :initform nil :initarg :name)))

;;プレイヤーと敵で共通で使うやつ
(defclass common (obj)
  ((hp        :accessor hp        :initform 30    :initarg :hp)
   (maxhp     :accessor maxhp     :initform 30    :initarg :maxhp)
   (agi       :accessor agi       :initform 30    :initarg :agi)
   (def       :accessor def       :initform 30    :initarg :def)
   (str       :accessor str       :initform 30    :initarg :str)
   (dead      :accessor dead      :initform nil   :initarg :dead)    ;;死亡判定
   (ido-spd   :accessor ido-spd   :initform 2     :initarg :ido-spd) ;;移動速度
   (atk-pos-x   :accessor atk-pos-x   :initform 0     :initarg :atk-pos-x)
   (atk-pos-y   :accessor atk-pos-y   :initform 0     :initarg :atk-pos-y)
   (atk-pos-f :accessor atk-pos-f :initform nil   :initarg :atk-pos-f)
   (dmg       :accessor dmg       :initform nil   :initarg :dmg)     ;;ダメージ表示用
   (dmg-c     :accessor dmg-c     :initform 0     :initarg :dmg-c)   ;;ダメージを受ける間隔
   (race      :accessor race      :initform nil   :initarg :race)    ;;種族  0:プレイヤー 1:オーク 2:スライム 3:ヒドラ 4:ブリガンド 5 メテルヨテイチ
   (walk-c    :accessor walk-c    :initform 0     :initarg :walk-c)  ;;歩行アニメカウンター
   (walk-func :accessor walk-func :initform #'+   :initarg :walk-func)
   
   (dir-c     :accessor dir-c     :initform 0     :initarg :dir-c)   ;;方向転換用カウンター
   (atk-now   :accessor atk-now   :initform nil   :initarg :atk-now) ;;攻撃中か
   (atk-c     :accessor atk-c     :initform 0     :initarg :atk-c)   ;;攻撃モーション更新用
   (atk-img   :accessor atk-img   :initform 0     :initarg :atk-img) ;;攻撃画像番号 ０～２
   (atk-spd   :accessor atk-spd   :initform 10    :initarg :atk-spd) ;;攻撃速度
   (expe      :accessor expe      :initform 0     :initarg :expe) ;;もらえる経験値orプレイヤーの所持経験値
   ))

(defclass chest (common)
  ())

;;適用
(defclass enemy (common)
  ((drop         :accessor drop       :initform nil :initarg :drop)    ;;ドロップするアイテム
   (vx           :accessor vx         :initform 2   :initarg :vx)
   (vy           :accessor vy         :initform 2   :initarg :vy)
   (sight        :accessor sight      :initform 0   :initarg :sight)
   )
  (:default-initargs :w *obj-w* :h *obj-h* :moto-w *obj-w* :moto-h *obj-h*
   :w/2 *w/2* :h/2 *h/2* :atk-spd 10)) 

(defclass slime (enemy)
  ())

(defclass orc (enemy)
  ())

(defclass bubble (enemy)
  ())

(defclass skeleton (enemy)
  ())

(defclass brigand (enemy)
  ((range :accessor range :initform 0 :initarg :range)))

(defclass hydra (enemy)
  ((centerx      :accessor centerx    :initform 30  :initarg :centerx)
   (centery      :accessor centery    :initform 30  :initarg :centery)
   (deg          :accessor deg        :initform 10  :initarg :deg)))

(defclass dragon (brigand)
  ())

(defclass yote1 (enemy)
  ())

;;プレイヤー用
(defclass player (common)
  ((key?       :accessor key?        :initform nil :initarg :key?)     ;;鍵所持
   (lvup-exp   :accessor lvup-exp    :initform 100 :initarg :lvup-exp) ;;次のレベルアップに必要な経験値
   (name       :accessor name        :initform nil :initarg :name)     ;;名前
   (hammer     :accessor hammer      :initform 0   :initarg :hammer)   ;;所持ハンマー
   (potion     :accessor potion      :initform 1   :initarg :potion)   ;;所持ハンマー
   (weapon     :accessor weapon      :initform nil :initarg :weapon)     ;;装備武器
   (getitem    :accessor getitem     :initform nil :initarg :getitem) ;;拾ったアイテム描画用
   (arrow-num  :accessor arrow-num   :initform 1   :initarg :arrow-num) 
   (armor      :accessor armor       :initform nil :initarg :armor)
   (dash-p     :accessor dash-p      :initform nil :initarg :dash-p)
   (poison-cnt :accessor poison-cnt  :initform 0   :initarg :poison-cnt) ;;毒カウンター
   (item       :accessor item        :initform nil :initarg :item)     ;;所持アイテム
   (item-page  :accessor item-page   :initform 0   :initarg :item-page) 
   (stage      :accessor stage       :initform 1   :initarg :stage)    ;;プレイヤーのいる階層
   (state      :accessor state       :initform :title :initarg :state)
   (cursor     :accessor cursor      :initform 0   :initarg :cursor)
   (orb        :accessor orb         :initform 0   :initarg :orb)      ;;クリアアイテム
   (endtime    :accessor endtime     :initform 0   :initarg :endtime)  ;;クリア時間
   (door       :accessor door        :initform nil :initarg :door)     ;;ドア当たり判定用
   ))

;;拾ったアイテム描画用
(defclass itemtext (dmg-font)
  ((name :accessor name      :initform nil :initarg :name)
   (timer :accessor timer      :initform 0     :initarg :timer)))

(defparameter *move-cost* (make-array 50 :initial-element 0))
(setf (aref *move-cost* 40) 10
      (aref *move-cost* 30) 10
      (aref *move-cost* 0)   0)


(defparameter *aiueo*
  "あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやいゆえよらりるれろわをん■■がぎぐげござじずぜぞだぢづでどばびぶべぼぱぴぷぺぽー消決")


(defclass itemdesc ()
  ((name       :accessor name      :initform nil :initarg :name)
   (price      :accessor price     :initform 0   :initarg :price)
   (new        :accessor new       :initform nil :initarg :new)
   (level      :accessor level     :initform 0   :initarg :level)
   (categoly   :accessor categoly  :initform nil :initarg :categoly)
   (equiped    :accessor equiped   :initform nil :initarg :equiped)
   (damage     :accessor damage    :initform 0   :initarg :damage)
   (hit        :accessor hit       :initform 0   :initarg :hit)
   (atktype    :accessor atktype   :initform :atk :initarg :atktype)
   (tokkou     :accessor tokkou    :initform 0   :initarg :tokkou)
   (critical   :accessor critical  :initform 0   :initarg :critical)
   (rangemin   :accessor rangemin  :initform 0   :initarg :rangemin)
   (rangemax   :accessor rangemax  :initform 0   :initarg :rangemax)
   (blk        :accessor blk      :initform nil :initarg :blk)
   (def        :accessor def       :initform 0   :initarg :def)
   (itemnum        :accessor itemnum      :initform nil :initarg :itemnum)))

(defclass weapondesc (itemdesc)
  ())


(defclass armordesc (itemdesc)
  ())


;;font----------------------------------------------------------
(defparameter *font140* nil)
(defparameter *font90* nil)
(defparameter *font70* nil)
(defparameter *font40* nil)
(defparameter *font30* nil)
(defparameter *font20* nil)
(defparameter *font2* nil)

(defun set-font ()
  (setf *font140* (create-font "MSゴシック" :height 140)
        *font90* (create-font "MSゴシック" :height 90)
        *font70* (create-font "MSゴシック" :height 70)
        *font40* (create-font "MSゴシック" :height 40)
        *font30* (create-font "MSゴシック" :height 30)
        *font20* (create-font "MSゴシック" :height 25)
	*font2* (create-font "MSゴシック" :height 15)));; :width 12 :weight (const +fw-bold+))))
