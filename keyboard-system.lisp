(in-package :sorcery-sdl2)

(defclass keyboard-system (system) ())

(defmethod update ((system keyboard-system) entity-manager game)
  ""
  (let* ((player (game-player-entity game))
         (player-kb (entity-component entity-manager player 'keyboard)))
    (when (kb-stack-action-p player-kb 'fire)
      (let* ((player-data (entity-component entity-manager player 'player-data))
             (item (bag-item player-data))
             (costume (entity-component entity-manager player 'costume)))

        (when item
          (let ((action (item-action item)))
            (when action
              (funcall action game (costume-x costume) (costume-y costume))
              (remove-from-bag player-data))))))))

