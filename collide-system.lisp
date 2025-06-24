(in-package :sorcery-sdl2)

;; Contact predicates

(defun contact-overlapsp (game entity1 entity2)
  (let* ((entity-manager (area-em (game-current-area game)))
         (costume1 (entity-component entity-manager entity1 'costume))
         (costume2 (entity-component entity-manager entity2 'costume)))
    (with-costume-geometry (costume1 x1 y1 w1 h1)
        (with-costume-geometry (costume2 x2 y2 w2 h2)
            (overlapsp game entity-manager entity1 x1 y1 w1 h1 entity2 x2 y2 w2 h2)))))

(defun door-contactp (game player door direction)
  (let* ((entity-manager (area-em (game-current-area game)))
         (keyboard (entity-component entity-manager player 'keyboard))
         (costume1 (entity-component entity-manager player 'costume))
         (costume2 (entity-component entity-manager door 'costume)))
    (and keyboard
         (kb-stack-action-p keyboard direction)
         (<= (abs (- (costume-y costume1) (costume-y costume2))) 2)
         (<= (abs (- (costume-x costume1) (costume-x costume2))) +element-width+))))

(defun door-left-contactp (game player door)
  (door-contactp game player door 'move-left))

(defun door-right-contactp (game player door)
  (door-contactp game player door 'move-right))

(defun door-green-contactp (game player door)
  (let* ((entity-manager (area-em (game-current-area game)))
         (costume1 (entity-component entity-manager player 'costume))
         (costume2 (entity-component entity-manager door 'costume)))
    (and
     (<= (abs (- (costume-y costume1) (costume-y costume2))) 2)
     (<= (abs (- (costume-x costume1) (costume-x costume2))) 32))))

(defun door-trap-contactp (game player door)
  (let* ((entity-manager (area-em (game-current-area game)))
         (costume1 (entity-component entity-manager player 'costume))
         (costume2 (entity-component entity-manager door 'costume)))
    (and
     (<= (abs (- (costume-y costume1) (costume-y costume2))) 16)
     (<= (abs (- (costume-x costume1) (costume-x costume2))) 2))))

(defun cauldron-contactp (game player cauldron)
  (let* ((entity-manager (area-em (game-current-area game)))
         (costume1 (entity-component entity-manager player 'costume))
         (costume2 (entity-component entity-manager cauldron 'costume)))
    (and
     (<= (abs (- (costume-y costume1) (costume-y costume2))) 30)
     (<= (abs (- (costume-x costume1) (costume-x costume2))) 12))))

(defun sanctuary-pedestal-contactp (game player pedestal)
  (declare (ignore pedestal game))
 
  (let* ((entity-manager (area-em (game-current-area game)))
         (costume (entity-component entity-manager player 'costume)))
    (and (= (costume-x costume) 288)
         (= (costume-y costume) 52))))

;; Contact actions

(defun sanctuary-pedestal-end-game (game player entity)
  (declare (ignore entity))
  (let* ((entity-manager (area-em (game-current-area game)))
         (player-data (entity-component entity-manager player 'player-data)))
    (when (all-apprentices-releasedp player-data)
      (let ((animation (entity-component entity-manager player 'animation)))
        (update-direction animation :front)
        (game-switch-state game (make-instance 'game-state-end-game
                                              :next-state 'game-state-victory
                                              :game game))))))

(defun putdown (game player entity)
  (let* ((entity-manager (area-em (game-current-area game)))
         (player-data (entity-component entity-manager player 'player-data))
         (costume (entity-component entity-manager entity 'costume)))
    (unless (bag-emptyp player-data)
      (add-element (game-current-area game)
                   (remove-from-bag player-data)
                   (costume-x costume) (costume-y costume)))))

(defun pickup (game player entity)
  (let* ((entity-manager (area-em (game-current-area game)))
         (player-data (entity-component entity-manager player 'player-data))
         (keyboard (entity-component entity-manager player 'keyboard))
         (item (entity-component entity-manager entity 'item)))

    (when (kb-stack-action-p keyboard 'fire)
      (putdown game player entity)
      (store-to-bag player-data (item-name item))
      (pushnew entity (game-trash game))
      (kb-stack-remove-action keyboard 'fire))))

(defun damage-player (game player entity)
  (let* ((entity-manager (area-em (game-current-area game)))
         (player-data (entity-component entity-manager player 'player-data)))

    (unless (or (game-trash-entity-memberp game entity) (player-cheat player-data))
      (decrease-energy player-data))))

(defun heal-player (game player entity)
  (let* ((entity-manager (area-em (game-current-area game)))
         (player-data (entity-component entity-manager player 'player-data)))
    (increase-energy player-data)))

(defun kill-monster (game entity player-or-missile)
  (let* ((area (game-current-area game))
         (entity-manager (area-em area))
         (player-data (entity-component entity-manager (game-player-entity game) 'player-data))
         (costume (entity-component entity-manager entity 'costume)))
    (unless (game-trash-entity-memberp game entity)
      (increase-score player-data)
      (add-element area 'blast (costume-x costume) (costume-y costume))
      (pushnew entity (game-trash game)))))

(defun remove-missile (game missile monster)
  (let ((entity-manager (area-em (game-current-area game))))
   (unless (game-trash-entity-memberp game monster)
     (pushnew missile (game-trash game)))))

(defun kill-monster-empty-bag (game entity player)
  (let* ((entity-manager (area-em (game-current-area game)))
         (player-data (entity-component entity-manager player 'player-data)))
    (kill-monster game entity player)
    (remove-from-bag player-data)))

(defun player-kill-monster (game entity player)
  ""
  (let* ((entity-manager (area-em (game-current-area game)))
         (item (bag-item (entity-component entity-manager player 'player-data)))
         (contact (entity-component entity-manager entity 'contact))
         (keyboard (entity-component entity-manager player 'keyboard)))

    (when (and (kb-stack-action-p keyboard 'fire)
               item
               (vulnerablep contact item))
      (kb-stack-remove-action keyboard 'fire)
      (kill-monster-empty-bag game entity player))))

(defun release-apprentice (game player entity)
  (let* ((area (game-current-area game))
         (entity-manager (area-em area))
         (player-data (entity-component entity-manager player 'player-data))
         (costume (entity-component entity-manager entity 'costume))
         (apprentice (entity-component entity-manager entity 'apprentice)))
    (increase-score player-data 100)
    (incf (player-apprentices player-data))
    (add-element area 'wheel-of-light (costume-x costume) (costume-y costume))
    (setf (apprentice-releasedp apprentice) t)))

(defun drown (game player entity)
  (declare (ignore entity))
  (let* ((entity-manager (area-em (game-current-area game)))
         (costume (entity-component entity-manager player 'costume)))
    (pushnew player (game-trash game))
    (add-element (game-current-area game) 'splash (costume-x costume) (costume-y costume))))

(defun player-die (game player)
  (let* ((area (game-current-area game))
         (entity-manager (area-em area))
        (costume (entity-component entity-manager player 'costume)))
    (when costume
      (pushnew entity (game-trash game))
      (add-element area 'blast
                   (costume-x costume) (costume-y costume)))))

(defun open-removable-door (game player entity) ;; ok
  (let* ((area (game-current-area game))
         (entity-manager (area-em area))
        (player-data (entity-component entity-manager player 'player-data))
        (door (entity-component entity-manager entity 'door)))
    (unlock door (bag-item player-data) :cheat (player-cheat player-data))
    (when (not (lockedp door))
      (remove-from-bag player-data)
      (qt:quadtree-remove (area-qt area) entity)
      (pushnew entity (game-trash game)))))

(defun open-door (game player entity) ;; ok
  (let* ((entity-manager (area-em (game-current-area game)))
         (player-data (entity-component entity-manager player 'player-data))
         (item (bag-item player-data))
         (door (entity-component entity-manager entity 'door)))
    (unlock door item :cheat (player-cheat player-data))
    (when (not (lockedp door))
      (game-door *game* entity))))

(defun apply-contact (game entity1 contact1 entity2 contact2)
  ""
  (dolist (action (contact-send-handler contact2))
    (when (contact-action-validp contact1 action)
      (funcall action game entity1 entity2))))

(defclass collide-system (system) ())

(defmethod update ((system collide-system) entity-manager game)
  ""
  (let ((contact-entities (find-entities entity-manager 'contact))
        (move-entities (find-entities entity-manager 'move)))
    (dolist (entity1 move-entities)
      (dolist (entity2 (remove entity1 contact-entities))
       (let ((contact1 (entity-component entity-manager entity1 'contact))
             (contact2 (entity-component entity-manager entity2 'contact)))

         (when (funcall (contact-predicate contact2) game entity1 entity2)
           
           (if (> (contact-priority contact1) (contact-priority contact2))
               (progn
                 (apply-contact game entity1 contact1 entity2 contact2)
                 (apply-contact game entity2 contact2 entity1 contact1))
               (progn
                 (apply-contact game entity2 contact2 entity1 contact1)
                 (apply-contact game entity1 contact1 entity2 contact2)))))))))

