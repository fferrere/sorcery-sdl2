(in-package :sorcery-sdl2)

(defclass costume (component)
  ((x :accessor costume-x
      :initarg :x)
   (y :accessor costume-y
      :initarg :y)
   (w :accessor costume-w
      :initarg :w)
   (h :accessor costume-h
      :initarg :h)
   (asset :accessor costume-asset
          :initarg :asset)
   (index :accessor costume-index
	  :initarg :index)
   (depth :accessor costume-depth
	  :initarg :depth
          :documentation ":foreground ou :background"))
  (:default-initargs :depth :background))

(defmethod update-index ((costume costume) index)
  (setf (costume-index costume) index))

(defmethod update-coords ((costume costume) x y)
  (setf (costume-x costume) x
        (costume-y costume) y))

(defmethod render ((costume costume) engine)
  (s2e:draw-sprite engine
                   (costume-asset costume)
                   (costume-index costume)
                   (+ (costume-x costume) *game-screen-x*)
                   (+ (costume-y costume) *game-screen-y*)))

(defmacro with-costume-geometry ((costume x y w h) &body body)
  `(let ((,x (costume-x ,costume))
         (,y (costume-y ,costume))
         (,w (costume-w ,costume))
         (,h (costume-h ,costume)))
     ,@body))

(defclass stencil (component)
  ((asset :accessor stencil-asset
          :initarg :asset)
   (index :accessor stencil-index
	  :initarg :index)))

;; Costume Animation
(defclass strategy-animation ()
  ((animation :accessor s-animation :initarg :animation)))

(defgeneric animate (strategy-animation) )
(defgeneric animation-replace (strategy-animation) )

(defclass animation-loop (strategy-animation)())
(defmethod animate ((s animation-loop))
  (let ((a (s-animation s)))
    (when (last-index-p a)
      (setf (animation-position a) 0))))

(defclass animation-ping-pong (strategy-animation)())
(defmethod animate ((s animation-ping-pong))
  (let ((a (s-animation s)))
    (cond
      ((last-index-p a) (progn
                          (decf (animation-position a) 2)
                          (set-backward-state a)))
      ((first-index-p a) (progn
                           (incf (animation-position a) 2)
                           (set-forward-state a))))))

(defclass animation-run-stop (strategy-animation)())
(defmethod animate ((s animation-run-stop))
  (let ((a (s-animation s)))
    (cond
      ((last-index-p a) (progn
                          (decf (animation-position a))
                          (set-stop-state a)))
      ((first-index-p a) (progn
                           (incf (animation-position a))
                           (set-stop-state a))))))

(defclass animation-run-destroy (strategy-animation)
  ((next :accessor animation-next :initarg :next))
  (:default-initargs :next nil))

(defmethod animate ((s animation-run-destroy))
  (let ((a (s-animation s)))
    (cond
      ((last-index-p a) (progn
                          (set-destroy-flag a)
                          (decf (animation-position a))))
      ((first-index-p a) (progn
                           (set-destroy-flag a)
                           (incf (animation-position a)))))))

(defmethod animation-replace ((s animation-run-destroy))
  (animation-next s))

(defclass animation (component)
  ((state :accessor animation-state :initarg :state :documentation "must be 1, -1, or 0")
   (strategy :accessor animation-strategy :initarg :strategy)
   (indexes :accessor animation-indexes :initarg :indexes)
   (key :accessor animation-key :initarg :key :documentation ":front, :left or :right")
   (position :accessor animation-position :initarg :position)
   (destroy-flag :accessor animation-destroy-flag :initarg :destroy-flag)
   (last-tick :accessor animation-lt
	      :initform 0
	      :documentation "Last Time tick")
   (speed :accessor animation-speed
	  :initarg :speed
	  :documentation "max delta time to switch costume"))
  (:default-initargs
   :destroy-flag nil
   :state 1
   :key :front
   :position 0
   :speed 150))

(defmethod current-indexes ((a animation))
  (getf (animation-indexes a) (animation-key a)))

(defmethod current-index ((a animation))
  (aref (current-indexes a) (animation-position a)))

(defmethod last-index-p ((a animation))
  (> (animation-position a) (1- (length (current-indexes a)))))

(defmethod first-index-p ((a animation))
  (< (animation-position a) 0))

(defmethod update-costume-index ((a animation) costume)
  (update-index costume (current-index a)))

(defmethod update-direction ((a animation) direction)
  "direction muste be either :front, :left or :right"
  (when (and (getf (animation-indexes a) direction nil) (not (eql (animation-key a) direction)))
   (setf (animation-key a) direction
         (animation-position a) 0)))

(defmethod set-forward-state ((a animation))
  (setf (animation-state a) 1))
(defmethod set-backward-state ((a animation))
  (setf (animation-state a) -1))
(defmethod set-stop-state ((a animation))
  (setf (animation-state a) 0))

(defmethod set-destroy-flag ((a animation))
  (setf (animation-destroy-flag a) t))

(defmethod animate ((a animation))
  (incf (animation-position a) (animation-state a))
  (animate (animation-strategy a)))

(defmethod animation-replace ((a animation))
  (animation-replace (animation-strategy a)))

;; Move Component

(defun move-up (x y step)
  (list x (- y step)))

(defun move-down (x y step)
  (list x (+ y step)))

(defun move-left (x y step)
  (list (- x step) y))

(defun move-right (x y step)
  (list (+ x step) y))

(defclass move-strategy ()
  ((component :accessor move-component :initarg :component)))

(defgeneric update-stack (move-strategy entity-manager entity))
(defmethod update-stack ((ms move-strategy) entity-manager entity)
  "Do nothing")

(defclass move-directed (move-strategy)
  ((directions :accessor ms-directions
               :initarg :directions))
  (:default-initargs :directions ()))

(defmethod update-stack ((ms move-directed) entity-manager entity)
  (dolist (dir (ms-directions ms))
    (pushnew dir (move-stack (move-component ms)))))

(defun horizontal-direction (x1 x2)
  (let ((direction-h (- x1 x2)))
    (cond
      ((< direction-h 0) 'move-right)
      ((> direction-h 0) 'move-left))))

(defun vertical-direction (y1 y2)
  (let ((direction-v (- y1 y2)))
    (cond
      ((< direction-v 0) 'move-down)
      ((> direction-v 0) 'move-up))))

(defclass move-follow (move-strategy)
  ((target :accessor ms-target :initarg :target
           :documentation "target is an entity ID"))
  (:default-initargs :target 'game-player-entity))

(defmethod update-stack ((ms move-follow) entity-manager entity)
  (let* ((target (funcall (ms-target ms) *game*))
         (target-costume (entity-component entity-manager target 'costume))
         (entity-costume (entity-component entity-manager entity 'costume))
         (entity-move (move-component ms)))
    (when (and target-costume entity-costume)
      (let ((direction-h (horizontal-direction (costume-x entity-costume)
                                               (costume-x target-costume)))
            (direction-v (vertical-direction (costume-y entity-costume)
                                             (costume-y target-costume))))
        (when direction-h (pushnew direction-h (move-stack entity-move)))
        (when direction-v (pushnew direction-v (move-stack entity-move)))))))

(defclass move-follow-horizontal (move-follow) ())

(defmethod update-stack ((ms move-follow-horizontal) entity-manager entity)
  (let* ((target (funcall (ms-target ms) *game*))
         (target-costume (entity-component entity-manager target 'costume))
         (entity-costume (entity-component entity-manager entity 'costume))
         (entity-move (move-component ms)))

    (when (and target-costume entity-costume)
      (let ((direction-h (horizontal-direction (costume-x entity-costume)
                                               (costume-x target-costume))))
        (when direction-h (pushnew direction-h (move-stack entity-move)))))))


(defclass move-keyboard (move-strategy) ())

(defmethod update-stack ((ms move-keyboard) entity-manager entity)
  "Copy only 'move-* action to stack of the move component. Discard FIRE action..."
  (let ((keyboard (entity-component entity-manager entity 'keyboard))
        (move (move-component ms)))
    (dolist (action (keyboard-stack keyboard))
      (when (cl-ppcre:scan-to-strings "MOVE" (string-upcase action))
        (pushnew action (move-stack move))))
    (when (not (member 'move-up (keyboard-stack keyboard)))
      (pushnew 'move-down (move-stack move)))))

(defclass move (component)
  ((strategy :accessor move-strategy :initarg :strategy)
   (stack :accessor move-stack :initarg :stack)
   (crossp :accessor move-crossp :initarg :crossp)
   (last-tick :accessor move-lt :initarg :last-tick)
   (speed :accessor move-speed
	  :initarg :speed))
  (:default-initargs :stack () :speed 4 :last-tick 0 :crossp ())
  (:documentation "Used to mananage moves"))

(defmethod initialize-instance :after ((m move) &key)
  (setf (move-strategy m) (make-instance 'move-strategy :component m)))

(defmethod update-stack ((m move) entity-manager entity)
  (update-stack (move-strategy m) entity-manager entity))

(defmethod clear-stack ((m move))
  (setf (move-stack m) '()))

(defmethod move-next-positions ((m move) x y)
  "returns all possible positions based on
   of the stack of identified movements.
   Plus the combined position of the identified movements"
  (let ((step 1))
    (cons
     (loop for direction in (move-stack m)
           for xi = x then (first pos) 
           for yi = y then (second pos)
           for pos = (funcall direction xi yi step)
           finally (return (list (first pos) (second pos))))
     (mapcar #'(lambda (direction) (funcall direction x y step)) (move-stack m)))))

(defmethod move-strategy-change ((m move) strategy)
  (setf (move-strategy m) strategy))


;; ------------------------------

(defclass keyboard (component)
  ((stack :accessor keyboard-stack :initarg :stack))
  (:default-initargs :stack ()))

(defmethod kb-stack-action-p ((kb keyboard) action)
  (member action (keyboard-stack kb)))

(defmethod kb-stack-add-action ((kb keyboard) action)
  (pushnew action (keyboard-stack kb)))

(defmethod kb-stack-remove-action ((kb keyboard) action)
  (setf (keyboard-stack kb) (remove action (keyboard-stack kb))))

(defclass player-data (component)
  ((score :accessor player-score :initform 0)
   (bag :accessor player-bag :initform (make-instance 'entity-manager))
   (energy :accessor player-energy :initform 99)
   (cheat :accessor player-cheat :initform nil)
   (apprentices :accessor player-apprentices :initform 0)
   (last-tick :accessor player-lt :initform 0)))

(defmethod increase-score ((p player-data) &optional (score 50))
  (incf (player-score p) score))

(defmethod all-apprentices-releasedp ((p player-data))
  (= (player-apprentices p) 8))

(defmethod bag-item ((p player-data))
  (entity-component (player-bag p) (first (find-entities (player-bag p) 'item)) 'item))

(defmethod store-to-bag ((p player-data) item-name)
  (add-element (player-bag p) item-name 8 340))

(defmethod remove-from-bag-* ((p player-data) item-name1)
  (let* ((entity (first (find-entities (player-bag p) 'item)))
        (item-name2 (item-name (entity-component (player-bag p) entity 'item))))
    (when (eql item-name1 item-name2) 
        (remove-entity (player-bag p) entity))
    item-name2))

(defmethod remove-from-bag ((p player-data))
  (let ((entity (first (find-entities (player-bag p) 'item))))
    (when entity
      (let ((item-name (item-name (entity-component (player-bag p) entity 'item))))
        (remove-entity (player-bag p) entity)
        item-name))))

(defmethod bag-emptyp ((p player-data))
  (not (find-entities (player-bag p) 'item)))

(defmethod increase-energy ((p player-data) &optional (step 1))
  (s2e:with-ticks ((player-lt p) 60)
    (unless (> (+ (player-energy p) step) 99)
      (incf (player-energy p) step))))

(defmethod decrease-energy ((p player-data) &optional (step 1))
  (s2e:with-ticks ((player-lt p) 140)
    (unless (< (- (player-energy p) step) 0)
      (decf (player-energy p) step))))

(defmethod energy-emptyp ((p player-data))
  (zerop (player-energy p)))

(defclass contact (component)
  ((predicate :accessor contact-predicate
              :initarg :predicate)
   (priority :accessor contact-priority
             :initarg :priority)
   (send :accessor contact-send-handler
         :initarg :send
         :documentation "Call (functions) sent to others contact components")
   (receive :accessor contact-receive-handler
            :initarg :receive
            :documentation "list of authorized call")
   (vulnerability :accessor contact-vulnerability
                  :initarg :vulnerability
                  :documentation "List of vulnerabilities"))
  (:default-initargs :predicate 'overlapsp :receive () :priority 1)
  (:documentation "Component used to manage collision detection"))

(defmethod contact-action-validp ((contact contact) action)
  (member action (contact-receive-handler contact)))

(defmethod vulnerablep ((contact contact) item)
  (member (item-name item) (contact-vulnerability contact)))

(defclass item (component)
  ((name :accessor item-name :initarg :name)
   (action :accessor item-action :initarg :action))
  (:default-initargs :action ()))

(defclass apprentice (component)
  ((releasedp :accessor apprentice-releasedp :initform nil)))


(defclass door (component)
  ((id :accessor door-id :initarg :id)
   (lock :accessor door-lock :initarg :lock)
   (key :accessor door-key :initarg :key)
   (in :accessor door-in :initarg :in
       :documentation "move-left for door-right, move-right for door-left")
   (out :accessor door-out :initarg :out
        :documentation "move-left for door-left, move-right for door-right")
   (target :accessor door-target :initarg :target))
  (:default-initargs :key nil))

(defmethod initialize-instance :after ((door door) &key)
  "If door component has no slot key, so the door is unlocked"
  (setf (door-lock door) (and (door-key door) t)))

(defmethod unlock ((door door) item &key (cheat nil))
  ""
  (when (or cheat (and item (eql (item-name item) (door-key door))))
    (setf (door-lock door) nil)))

(defmethod lockedp ((door door))
  (door-lock door))

(defclass btime (component)
  ((count :accessor btime-count :initarg :count)
   (start :accessor btime-start :initarg :start))
  (:default-initargs :count 2560 :start 2560)
  (:documentation "42 mn to terminate the game.
                   Each second => clear 4 pixels
                   Timebook is 10240 pixels (128x80)
                   10240 / 4 = 2560"))
