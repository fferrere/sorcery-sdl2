(in-package :sorcery-sdl2)

(defclass render-system (system) ())

(defmethod update ((system render-system) entity-manager engine)
  (s2e:draw-sprite engine :board 0 0 0)
    
  (let ((foregrounds '()))
    (dolist (entity (find-entities entity-manager 'costume))
      (dolist (costume (entity-components entity-manager entity 'costume))
        (if (eql (costume-depth costume) :foreground)
            (push costume foregrounds)
            (render costume engine))))
    (dolist (costume foregrounds)
      (render costume engine))))
