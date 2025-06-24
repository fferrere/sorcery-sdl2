(in-package :sorcery-sdl2)

(defclass clear-round-system (system) ())

(defun add-apprentice-to-sancuary (game)
  (let* ((sanctuary (game-area game 'm6))
         (player (game-player-entity game))
         (player-data (entity-component (area-em (game-current-area game)) player 'player-data))
         (coords (sanctuary-apprentice-pos (player-apprentices player-data))))
    (add-element sanctuary 'apprentice-sanctuary (aref coords 0) (aref coords 1))))

(defun remove-player (game entity-manager player)
  (del-components entity-manager player 'animation)
  (del-components entity-manager player 'costume)
  (del-components entity-manager player 'stencil)
  (del-components entity-manager player 'move)
  (del-components entity-manager player 'contact)
  (game-switch-state game (make-instance 'game-state-end-game
                                         :game game
                                         :next-state 'game-state-game-over)))

(defmethod update ((system clear-round-system) entity-manager game)
  ""
  (let ((player (game-player-entity game)))

    ;; Player Energy Out ?
    (when (energy-emptyp (entity-component entity-manager player 'player-data))
      (player-die game player))

    ;; Some apprentice released ?
    (dolist (entity (find-entities entity-manager 'apprentice))
      (let ((apprentice (entity-component entity-manager entity 'apprentice)))
        (when (and apprentice (apprentice-releasedp apprentice))
          (add-apprentice-to-sancuary game)
          (remove-entity entity-manager entity))))

    ;; Entities to remove ?
    (dolist (entity (game-trash game))
      (if (= entity player)
          (remove-player game entity-manager player) ;; remove some components only           
          (remove-entity entity-manager entity)))
    (game-trash-clear game)))

