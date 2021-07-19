(in-package :mazeofmogezou)


(cffi:defctype mcierror :int)
(cffi:defcfun "mciSendStringA" mcierror 
  (msg :string) (ret :string) 
  (a :int) (b :int))



; Helper function that calls mcisendstringa
(defun mci-send-string (command) 
  (mcisendstringa command "" 0 0))

(defun bgm-play (alias &optional (from 0))
  (mci-send-string
   (format nil "play ~a from ~d" alias from)))


(defun bgm-open (path alias)
  (mci-send-string
   (format nil "open ~a alias ~a type mpegvideo" path alias)))
  


(defun bgm-set-volume (vol alias)
  (mci-send-string
   (format nil "setaudio ~a volume to ~d" alias vol)))

(defun mci-send-status (alias)
  (with-foreign-strings ((str (make-string 255)))
    (mcisendstringa
     (concatenate 'string "status " alias " mode") str 255 0)
    (foreign-string-to-lisp str)))
;; 
;;(defun set-song (song) 
;;  (setf *song* song))

(defun bgm-status (alias)
  (multiple-value-bind (str num)
      (mci-send-status alias)
    (declare (ignore num))
    str))

; play-song is a helper function. 
;; (defun bgm-play (path alias vol) 
;;   (let ((status 
;; 	 (mci-send-open path alias)))
;;     (if (zerop status) 
;; 	1
;; 	(progn (mci-send-volume vol alias)
;; 	       (mci-send-play alias)))))

; Plays the current song. 
; It will keep trying to play it if there is an error, 
; because frequently it will fail for some reason :/ 
; This failure appears to be nondeterministic... 
;; (defun play-bgm (path vol) 
;;   (if *song* 
;;       (if (zerop (play-song path vol))
;; 	  0 
;; 	  (play path vol)) 
;;       "Error: You must first specify a song with SET-SONG."))

; Pauses the current song. 
(defun bgm-pause (alias) 
  (mci-send-string
   (format nil "pause ~a" alias)))

; Stops the current song. 
(defun bgm-stop (alias) 
  (mci-send-string
   (format nil "stop ~a" alias)))

; Please remember to call this when you're done with a song! 
(defun bgm-close (alias) 
  (mci-send-string
   (format nil "close ~a" alias)))



(defun init-bgm ()
  (loop :for path :in (list *OP-bgm-path* *ED-bgm-path* *d1-bgm-path*
			    *d2-bgm-path* *d3-bgm-path*
			    *d4-bgm-path*)
	:for alias :in (list *op-bgm* *ed-bgm* *d1-bgm* *d2-bgm*
			     *d3-bgm* *d4-bgm*)
     :do (bgm-open path alias)
       (bgm-set-volume 300 alias)))

(defun close-bgms ()
  (loop  :for alias :in (list *op-bgm* *ed-bgm* *d1-bgm* *d2-bgm*
			     *d3-bgm* *d4-bgm*)
     :do (bgm-close  alias)))

