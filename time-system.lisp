(in-package :sorcery-sdl2)

;; Book of time : 128x80 pixels
;; Displayed at : x=496, y=300
;;
;; An intermediate texture is used to draw points in order to clean up the image
;;

(defclass time-system (system)
  ((speed :accessor ts-speed :initarg :speed)
   (last-tick :accessor ts-lt :initarg :last-tick))
  (:default-initargs :speed 300
                     :last-tick 0))

(defmethod update ((system time-system) entity-manager game)
  
  (s2e:with-ticks ((ts-lt system) (ts-speed system))
    (dolist (entity (find-entities entity-manager 'btime))
      (let ((costume (entity-component entity-manager entity 'costume))
            (nb-pixels-to-clean 4)
            (btime (entity-component entity-manager entity 'btime)))
        
        (multiple-value-bind (divisor remainder)
            (truncate (- (btime-start btime) (btime-count btime))
                      (/ (costume-w costume) nb-pixels-to-clean))
          
          (if (> (btime-count btime) 0)
              (let ((x (* remainder nb-pixels-to-clean)) (y divisor))
                (s2e:draw-line game x y (+ x nb-pixels-to-clean) y :asset-name :bot)
                (decf (btime-count btime)))
              (player-die game (game-player-entity game))))))))
