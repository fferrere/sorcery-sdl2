(in-package #:sorcery-sdl2)

(defparameter *frame-rate* 4)

(defvar *game*)

(defun start ()
  (let ((game (make-instance 'game :loop 'game-loop :width 800 :height 700)))
    
    (s2e:add-asset game :board *board-file* 800 700)
    (s2e:add-asset game :intro *intro-file* 640 288)
    (s2e:add-asset game :bot *bot-file* 128 80 :mutablep t)
    (s2e:add-asset game :sprites *elements-file* 48 48)
    (s2e:add-asset game :sprites-stencils *stencils-elements-file* 48 48 :type :stencil)
    (s2e:add-asset game :tiles *tiles-file* 40 48)
    (s2e:add-asset game :tiles-stencils *stencils-tiles-file* 40 48 :type :stencil)

    (s2e:add-keyboard-mapping game "up" :up 'player-move-up)
    (s2e:add-keyboard-mapping game "left" :left 'player-move-left)
    (s2e:add-keyboard-mapping game "right" :right 'player-move-right)
    (s2e:add-keyboard-mapping game "space" :space 'player-fire)
    (s2e:add-keyboard-mapping game "start" :s 'game-start)
    (s2e:add-keyboard-mapping game "lang" :l 'game-switch-lang)
    (s2e:add-keyboard-mapping game "pause" :p 'game-pause)
    (s2e:disable-keyboard-controls game)
    
    (dolist (lang '(:french :english))
      (s2e:with-text-asset (game asset lang *font-file* 16)
        (dolist (dict (getf *translate* lang))
          (s2e:add-text asset (car dict) (cdr dict) #xF7 #XF7 0 0))
        (s2e:add-text-alphabet asset #xF7 #XF7 0 0)
        (s2e:add-text-digits asset #xF7 #XF7 0 0)))
   
    (game-init game)

    (setf *game* game)
    
    (s2e:launch game "Amstrad - Sorcery")))
