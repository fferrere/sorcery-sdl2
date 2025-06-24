(in-package :sorcery-sdl2)

;; Display inventory

(defclass bag-system (system) ())

(defmethod update ((system bag-system) entity-manager engine)
  (dolist (entity (find-entities entity-manager 'costume))
    (dolist (costume (entity-components entity-manager entity 'costume))
      (s2e:draw-sprite engine
                              (costume-asset costume)
                              (costume-index costume)
                              120 440
                              :scale-w 0.9
                              :scale-h 0.9))))
