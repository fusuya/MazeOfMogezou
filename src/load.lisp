(ql:quickload :ftw)

(defpackage mazeofmogezou
  (:use :cl :ftw :cffi))


(loop :for file :in '("define.lisp" "item.lisp" "mci.lisp"
                      "maze-test.lisp" "render.lisp"
                      "astar.lisp" "meimoge.lisp")
      :do (load file :external-format :utf-8))



#|
(sb-ext:save-lisp-and-die "mogerpg"
        :toplevel #'main
        :save-runtime-options t
        :executable t)
|#
