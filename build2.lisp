(ql:quickload :mazeofmogezou)

(sb-ext:save-lisp-and-die "MazeOfMogezou.exe" :toplevel #'mazeofmogezou:moge
					      ;;:application-type :gui
					      :executable t :save-runtime-options t)
