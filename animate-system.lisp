(in-package :sorcery-sdl2)

(defclass animate-system (system) ())

(defmethod update ((system animate-system) entity-manager game)
  (dolist (entity (find-entities entity-manager 'animation))
    (let ((animation (entity-component entity-manager entity 'animation))
          (costume (entity-component entity-manager entity 'costume)))
      (s2e:with-ticks ((animation-lt animation) (animation-speed animation))
        (animate animation)
        (update-costume-index animation costume)
        (when (animation-destroy-flag animation)
          (pushnew entity (game-trash game))
          (let ((replace (animation-replace animation)))
            (when replace
              (add-element (game-current-area game)
                           replace
                           (costume-x costume)
                           (costume-y costume)))))))))

