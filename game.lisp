(in-package #:sorcery-sdl2)

(defun player-keyboard-action (game action key-status)
  (let* ((em (area-em (game-current-area game)))
         (entity (first (find-entities em 'keyboard)))
         (keyboard (entity-component em entity 'keyboard)))
    (when entity
      (case key-status
        (:down (kb-stack-add-action keyboard action))
        (:up (kb-stack-remove-action keyboard action))))))

(defun player-move-up (game key-status)
  (player-keyboard-action game 'move-up key-status))

(defun player-move-down (game key-status)
  (player-keyboard-action game 'move-down key-status))

(defun player-move-left (game key-status)
  (player-keyboard-action game 'move-left key-status))

(defun player-move-right (game key-status)
  (player-keyboard-action game 'move-right key-status))

(defun player-fire (game key-status)
  (player-keyboard-action game 'fire key-status))

(defun find-door (entity-manager id)
  "Return a door entity ID"
  (let ((doors (find-entities entity-manager 'door)))
    (find id doors :key (lambda (entity)
                          (door-id (entity-component entity-manager
                                                     entity 'door))))))

(defun locate-player-near-target-door (area door-id e-player)
  (let* ((entity-manager (area-em area))
         (e-door (find-door entity-manager door-id)))
    (when e-door
      (let ((costume (entity-component entity-manager e-door 'costume))
            (door (entity-component entity-manager e-door 'door))
            (costume-player (entity-component entity-manager e-player 'costume)))
        (setf (costume-x costume-player)
              (case (door-out door)
                (move-left (- (costume-x costume) (/ +element-width+ 2)))
                (move-right (+ (costume-x costume) (/ +element-width+ 2)))))
        (setf (costume-y costume-player) (costume-y costume))))))

(defun draw-keyboard-action-key (game kb-text kb-name x y)
  (s2e:draw-texts game (game-lang game) (list kb-text (s2e:keyboard-key-string game kb-name)) x y))

;; Game States
(defclass game-state ()
  ((game :accessor state-game :initarg :game)))

(defgeneric state-init (game-state))
(defgeneric state-loop (game-state))

;; Intro
(defclass game-state-intro (game-state) ())

(defmethod state-init ((gs game-state-intro))
  (let ((game (state-game gs)))
    (s2e:disable-keyboard-controls game)
    (s2e:enable-keyboard-key game :s)
    (s2e:enable-keyboard-key game :l)))

(defmethod state-loop ((gs game-state-intro))
  (let* ((game (state-game gs))
         (em (area-em (game-current-area game))))

    (s2e:draw-text game (game-lang game) 'start 400 420 :position :center)
    (s2e:draw-text game (game-lang game) 'keyboard 140 450)
    (draw-keyboard-action-key game 'kb-up "up" 160 470)
    (draw-keyboard-action-key game 'kb-left "left" 160 490)
    (draw-keyboard-action-key game 'kb-right "right" 160 510)
    (draw-keyboard-action-key game 'kb-fire "space" 160 530)
    (draw-keyboard-action-key game 'kb-pause "pause" 160 550)
    (draw-keyboard-action-key game 'kb-lang "lang" 160 570)
    (update (game-as game) em game)
    (update (game-rs game) em game)))

;; Play

(defclass game-state-base-play (game-state) ())

(defmethod state-loop ((gs game-state-base-play))
  (let* ((game (state-game gs))
         (area (game-current-area game))
         (em (area-em area))
         (e-player (game-player-entity game))
         (player-data (entity-component em e-player 'player-data)))
    (update (game-as game) em game)
    (update (game-rs game) em game)
    (update (game-rs game) (game-board game) game)
    (update (game-bs game) (player-bag player-data) game)
    (s2e:draw-text game (game-lang game) 'inventory 80 410)
    (s2e:draw-text game (game-lang game) 'score 280 410)
    (s2e:draw-number game (game-lang game) (player-score player-data) 280 450)
    (s2e:draw-text game (game-lang game) 'energy 400 410)
    (s2e:draw-number game (game-lang game) (player-energy player-data) 400 450)
    (s2e:draw-text game (game-lang game) 'released 80 500)
    (s2e:draw-texts game (game-lang game)
                    (list (prin1-to-string (player-apprentices player-data)) 'released-end)
                    104 530)    
    (s2e:draw-texts game (game-lang game) (list 'youare (area-loc area)) 80 580)))

(defclass game-state-play (game-state-base-play) ())

(defmethod state-init ((gs game-state-play))
  (let ((game (state-game gs)))
    (s2e:disable-keyboard-controls game)
    (s2e:enable-keyboard-key game :up)
    (s2e:enable-keyboard-key game :left)
    (s2e:enable-keyboard-key game :right)
    (s2e:enable-keyboard-key game :space)
    (s2e:enable-keyboard-key game :s)
    (s2e:enable-keyboard-key game :l)
    (s2e:enable-keyboard-key game :p)))

(defmethod state-loop ((gs game-state-play))
  (let* ((game (state-game gs))
         (em (area-em (game-current-area game))))
    (update (game-ms game) em game)
    (update (game-cs game) em game)
    (update (game-ks game) em game) ;; Kb system must be after collide (pickup / fire)
    (update (game-crs game) em game)
    (update (game-ts game) (game-board game) game)
    (call-next-method)))

;; Open Door
(defclass game-state-open-door (game-state-base-play)
  ((door :accessor gs-door :initarg :door)))

(defmethod state-init ((gs game-state-open-door))
  (let* ((game (state-game gs))
         (em (area-em (game-current-area game)))
         (animation (entity-component em (gs-door gs) 'animation)))
    (set-forward-state animation)))

(defmethod state-loop ((gs game-state-open-door))
  (let* ((game (state-game gs))
         (area (game-current-area game))
         (em (area-em area))
         (adoor (entity-component em (gs-door gs) 'animation)))

    (call-next-method)
    (when (= (animation-position adoor) (1- (length (current-indexes adoor))))
      (set-stop-state adoor)
      (game-switch-state
       game
       (make-instance 'game-state-cross-door
                      :game game
                      :action 'door-in
                      :door (gs-door gs))))))

;; Close Door
(defclass game-state-close-door (game-state-base-play)
  ((door :accessor gs-door :initarg :door)))

(defmethod state-init ((gs game-state-close-door))
  (let* ((game (state-game gs))
         (em (area-em (game-current-area game)))
         (animation (entity-component em (gs-door gs) 'animation)))
    (setf (animation-position animation) (1- (length (current-indexes animation))))
    (set-backward-state animation)))

(defmethod state-loop ((gs game-state-close-door))
  (let* ((game (state-game gs))
         (area (game-current-area game))
         (em (area-em area))
         (adoor (entity-component em (gs-door gs) 'animation)))

    (call-next-method)

    (when (= (animation-position adoor) 0)
      (set-stop-state adoor)

      (game-switch-state
       game
       (make-instance 'game-state-play
                      :game game)))))


;; Cross Door
(defclass game-state-cross-door (game-state-base-play)
  ((door :accessor gs-door :initarg :door)
   (action :accessor gs-cross-action
           :initarg :action
           :documentation "Must be 'door-in or 'door-out")
   (speed :accessor gs-speed :initform 20)
   (last-tick :accessor gs-lt :initform 0)
   (step :accessor gs-step :initform 23))
  (:default-initargs :action :in))

(defmethod cross-door ((gs game-state-cross-door) entity-manager game)
  (let ((costume-player (entity-component entity-manager (game-player-entity game) 'costume))
        (costume-door (entity-component entity-manager (gs-door gs) 'costume))
        (door (entity-component entity-manager (gs-door gs) 'door)))
    (setf (costume-y costume-player) (costume-y costume-door))
    (let ((direction (funcall (gs-cross-action gs) door)))
      (with-coords (x y) ((funcall direction (costume-x costume-player) (costume-y costume-player) 1))
        (setf (costume-x costume-player) x)))))

(defmethod state-init ((gs game-state-cross-door))
  (let* ((game (state-game gs))
         (em (area-em (game-current-area game)))
         (e-player (game-player-entity game)))
    (when (eql (gs-cross-action gs) 'door-out)
      (let ((costume (entity-component em (gs-door gs) 'costume))
            (animation (entity-component em (gs-door gs) 'animation))
            (door (entity-component em (gs-door gs) 'door))
            (animation-player (entity-component em e-player 'animation)))
        (case (door-out door)
          (move-left (update-direction animation-player :left))
          (move-right (update-direction animation-player :right)))
        (setf (animation-position animation)
              (1- (length (current-indexes animation))))
        (update-costume-index animation costume)))))

(defmethod state-loop ((gs game-state-cross-door))
  (let* ((game (state-game gs))
         (area (game-current-area game))
         (em (area-em area)))

    (call-next-method)
    (s2e:with-ticks ((gs-lt gs) (gs-speed gs))
      (cross-door gs em game)
      (if (zerop (gs-step gs))
          (let ((door (entity-component em (gs-door gs) 'door)))
            (case (gs-cross-action gs)
              (door-in (go-next-area game (door-target door) (door-id door)))
              (door-out (game-switch-state
                         game
                         (make-instance 'game-state-close-door
                                        :game game
                                        :door (gs-door gs))))))
          (decf (gs-step gs))))))


;; Pause
(defclass game-state-pause (game-state-base-play) ())
(defmethod state-init ((gs game-state-pause))
  (let ((game (state-game gs)))
    (s2e:disable-keyboard-controls game)
    (s2e:enable-keyboard-key game :l)
    (s2e:enable-keyboard-key game :p)))

(defmethod state-loop ((gs game-state-pause))
  (let ((game (state-game gs)))
    (call-next-method)
    (s2e:draw-text game (game-lang game) 'pause 350 200)))

;; Player End Game
(defclass game-state-end-game (game-state-base-play) 
   ((speed :accessor gs-speed :initform 100)
    (last-tick :accessor gs-lt :initform 0)
    (next-state :accessor gs-nt :initarg :next-state)
    (step :accessor gs-step :initform 20)))

(defmethod state-init ((gs game-state-end-game))
  (let ((game (state-game gs)))
    (s2e:disable-keyboard-controls game)))

(defmethod state-loop ((gs game-state-end-game))
  (let* ((game (state-game gs))
         (area (game-current-area game)))
    (update (game-crs game) (area-em area) game)
    (call-next-method)

    (s2e:with-ticks ((gs-lt gs) (gs-speed gs))
      (if (zerop (gs-step gs))
          (game-switch-state game (make-instance (gs-nt gs) :game game))
          (decf (gs-step gs))))))

;; Victory
(defclass game-state-victory (game-state-base-play)
  ((speed :accessor gs-speed :initform 800)
   (last-tick :accessor gs-lt :initform 0)
   (apprentices :accessor gs-apprentices)
   (apprentice-index :accessor gs-apprentice-index :initform 0)
   (step :accessor gs-step :initform 20)))

(defmethod state-init ((gs game-state-victory))
  (let ((game (state-game gs)))
    (setf (gs-apprentices gs) (find-entities (area-em (game-current-area game)) 'apprentice))
    (s2e:disable-keyboard-controls game)))

(defun modify-apprentice-costumes (entity-manager entity)
  (let ((animation (entity-component entity-manager entity 'animation)))
    (setf (animation-indexes animation) '(:front #(64 65 66 67)))))

(defmethod state-loop ((gs game-state-victory))
  (let ((game (state-game gs)))
    (call-next-method)
    (s2e:draw-text game (game-lang game) 'victory 330 110)
    (s2e:with-ticks ((gs-lt gs) (gs-speed gs))
      (if (zerop (gs-step gs))
          (game-init game)
          (let ((em (area-em (game-current-area game)))
                (index (gs-apprentice-index gs)))
            (unless (>= (gs-apprentice-index gs) 8)
              (let ((entity (nth index (gs-apprentices gs))))
                (when entity
                  (modify-apprentice-costumes em entity)
                  (incf (gs-apprentice-index gs)))))
            (decf (gs-step gs)))))))

;; Game Over
(defclass game-state-game-over (game-state-base-play)
  ((speed :accessor gs-speed :initform 100)
   (step :accessor gs-step :initform 30)
   (last-tick :accessor gs-lt :initform 0)))

(defmethod state-init ((gs game-state-game-over))
  (let ((game (state-game gs)))
    (dolist (entity (find-entities (game-board game) 'btime))
      (remove-entity (game-board game) entity))
    (s2e:reset-asset game :bot)
    (s2e:disable-keyboard-controls game)))

(defmethod state-loop ((gs game-state-game-over))
  (let ((game (state-game gs)))
    (update (game-rs game) (game-board game) game)
    (s2e:draw-text game (game-lang game) 'gameover 320 210)
    (s2e:with-ticks ((gs-lt gs) (gs-speed gs))
      (if (zerop (gs-step gs))
          (game-init game)
          (decf (gs-step gs))))))


;; Score
(defclass game-state-score (game-state) ())
(defmethod state-init ((gs game-state-score))
  (let ((game (state-game gs)))
    (s2e:disable-keyboard-controls game)
    (s2e:enable-keyboard-key game :space)))

(defmethod state-loop ((gs game-state-score)))


(defclass game (s2e:engine)
  ((state :accessor game-state :initarg :state)
   (world :accessor game-world :initform (make-hash-table))
   (board :accessor game-board :initarg :board)
   (render-system :reader game-rs :initform (make-instance 'render-system))
   (bag-system :reader game-bs :initform (make-instance 'bag-system))
   (time-system :reader game-ts :initform (make-instance 'time-system))
   (animation-system :reader game-as :initform (make-instance 'animate-system))
   (move-system :reader game-ms :initform (make-instance 'move-system))
   (keyboard-system :reader game-ks :initform (make-instance 'keyboard-system))
   (collide-system :reader game-cs :initform (make-instance 'collide-system))
   (clear-round-system :reader game-crs :initform (make-instance 'clear-round-system))
   (player-entity :accessor game-player-entity)
   (current-area :accessor game-current-area)
   (language :accessor game-lang :initarg :language)
   (trash :accessor game-trash :initform ()))
  (:default-initargs :language :french))

(defmethod game-init ((game game))
  (let ((state (make-instance 'game-state-intro :game game)))
    (build-world game)
    (setf (game-current-area game) (world-area (game-world game) 'intro))
    (setf (game-state game) state)
    (state-init state)))

(defmethod game-switch-state ((game game) state)
  (setf (game-state game) state)
  (state-init state))

(defmethod game-start ((game game) key-status)
  (when (eql key-status :down)
    (let ((state (make-instance 'game-state-play :game game)))
      (build-world game)
      (let* ((em (make-instance 'entity-manager))
             (entity (make-entity em)))
        (add-component em entity (make-instance 'btime))
        (add-component em entity (make-instance 'costume :asset :bot
                                                         :index 0
                                                         :x 504 :y 320 :w 128 :h 80))
        (setf (game-board game) em))
      (game-switch-state game state)
      (setf (game-current-area game) (world-area (game-world game) 'p6))
      (change-area game 'p6)
      (add-element (game-current-area game) 'sorcerer 200 50)
      (store-player-entity game (first (find-entities (area-em (game-current-area game)) 'player-data)))
      (state-init state))))

(defmethod game-pause ((game game) key-status)
  (when (eql key-status :down)
    (let* ((state (game-state game))
           (next-state (if (typep state 'game-state-pause)
                           (make-instance 'game-state-play :game game)
                           (make-instance 'game-state-pause :game game))))
      (state-init next-state)
      (game-switch-state game next-state))))

(defmethod game-switch-lang ((game game) key-status)
  (when (eql key-status :down)
    (let ((lang (game-lang game)))
      (setf (game-lang game)
            (case lang
              (:french :english)
              (:english :french))))))

(defmethod game-door ((game game) door)
  (let ((state (make-instance 'game-state-open-door
                              :game game
                              :door door)))
    (game-switch-state game state)))

(defmethod game-loop ((game game))
  (state-loop (game-state game)))

(defmethod game-trash-entity-memberp ((game game) entity)
  (member entity (game-trash game)))

(defmethod game-trash-clear ((game game))
  (setf (game-trash game) ()))

(defmethod game-area-entity-manager ((game game) area)
  (area-em (world-area (game-world game) area)))

(defmethod game-area ((game game) area)
  (world-area (game-world game) area))

(defmethod change-area ((game game) area)
  (setf (game-current-area game) (game-area game area)))

(defmethod store-player-entity ((game game) entity)
  (setf (game-player-entity game) entity))

(defmethod go-next-area ((game game) area-id door-id)
  (let* ((current-area (game-current-area game))
         (current-em (area-em current-area))
         (player (game-player-entity game))
         (player-components (find-entity-components current-em player)))
    (remove-entity current-em player)
    
    (setf (animation-position
           (entity-component current-em
                             (find-door current-em door-id)
                             'animation))
          0)
    (change-area game area-id) ;; Entity Manager change now
    
    (let* ((next-area (game-current-area game))
           (entity-manager (area-em next-area))
           (entity (make-entity entity-manager)))
      (apply #'add-components entity-manager (cons entity player-components))
      (setf (game-player-entity game) entity)
      (locate-player-near-target-door next-area door-id entity)
      (game-switch-state game
                         (make-instance 'game-state-cross-door
                                        :game game
                                        :door (find-door entity-manager door-id)
                                        :action 'door-out)))))
