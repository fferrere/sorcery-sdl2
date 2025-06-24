(in-package :sorcery-sdl2)

(defclass area ()
  ((id :accessor area-id
       :initarg :id)
   (location :accessor area-loc
	     :initarg :location)
   (quadtree :accessor area-qt :initarg :quadtree)
   (entity-manager :accessor area-em
                   :initarg :em)))

(defun add-components (entity-manager entity &rest components)
  (dolist (c components)
    (add-component entity-manager entity c)))

(defgeneric add-element (area elmt-type  x y &rest args))

(defmacro define-element ((name &key (quadtreep nil)) &body body)
  `(defmethod add-element ((a area) (elmt-type (eql ',name)) x y &rest args)
     (let* ((entity-manager (area-em a))
            (entity (make-entity entity-manager)))
       (when ,quadtreep
         (qt:quadtree-insert (area-qt a) entity x y +element-width+ +element-height+))
       (add-components entity-manager entity ,@body))))

(defmacro define-bag-entity ((name) &body body)
  `(defmethod add-element (entity-manager (elmt-type (eql ',name)) x y &rest args)
     (let ((entity (make-entity entity-manager)))
       (add-components entity-manager entity ,@body))))

(defmacro move (strategy (&key (speed 4) (crossp nil)) &rest args)
  `(let ((mv (make-instance 'move :speed ,speed :crossp ,crossp)))
     (setf (move-strategy mv)
           (make-instance ,strategy :component mv ,@args))
     mv))

(defmacro costume (&key index (depth :background))
  `(make-instance 'costume
                  :x x :y y
                  :w +element-width+ :h  +element-height+
                  :index ,index
                  :asset :sprites
                  :depth ,depth))

(defmacro animation (strategy (&key indexes (speed 150) (state 1)) &rest args)
  `(let ((a (make-instance 'animation :speed ,speed :indexes ,indexes :state ,state)))
     (setf (animation-strategy a)
           (make-instance ,strategy :animation a ,@args))
     a))

(defmacro stencil (&key index)
  `(make-instance 'stencil
                  :index ,index
                  :asset :sprites-stencils))

(defmacro contact (&key predicate vulnerability send receive (priority 1))
  `(make-instance 'contact
                  :predicate ,predicate
                  :vulnerability ,vulnerability
                  :receive ,receive
                  :priority ,priority
                  :send ,send))

(defmacro item (name &key action)
  `(make-instance 'item :name ',name :action ,action))

(defmacro apprentice ()
  `(make-instance 'apprentice))

(defmacro keyboard ()
  `(make-instance 'keyboard))

(defmacro player ()
  `(make-instance 'player-data))

(defmacro door (&key in out)
  `(make-instance 'door :id (getf args :id nil)
                          :target (getf args :target nil)
                          :key (getf args :key nil)
                          :in ,in
                          :out ,out
                          :lock (getf args :lock nil)))

(defmacro define-item (name index &key action)
  `(progn
     (define-element (,name)
       (costume :index ,index)
       (stencil :index ,index)
       (item ,name :action ,action)
       (contact :predicate 'contact-overlapsp :send '(pickup)))
     (define-bag-entity (,name)
       (costume :index ,index)
       (item ,name :action ,action))))

(define-item amphora 0)
(define-item coatofarms 2)
(define-item goldkey 3)
(define-item fleurdelys 5)
(define-item goblet 6)
(define-item goldenchalice 7)
(define-item jewelledcrown 8)
(define-item littlelyre 9)
(define-item magicwand 10)
(define-item moon 11)
(define-item spellbook 13)
(define-item scroll 14)

(define-item axe 1)
(define-item flail 4)
(define-item spellbag 12 :action 'add-4-magic-missiles)
(define-item sword 15)
(define-item shootingstar 16 :action 'add-8-magic-missiles)

(define-element (blast)
  (costume :index 28 :depth :foreground)
  (animation 'animation-run-destroy (:indexes '(:front #(28 29 30 31)))))

(define-element (eye)
  (costume :index 32)
  (animation 'animation-ping-pong (:indexes '(:front #(32 33 34 35))))
  (stencil :index 20)
  (contact :predicate 'contact-overlapsp
           :vulnerability '(flail)
           :send '(damage-player) ;;  remove-missile
           :receive '(player-kill-monster kill-monster))
  (move 'move-follow (:speed 8)))

(define-element (ghost)
  (costume :index 48)
  (animation 'animation-ping-pong (:indexes '(:left #(44 45 46 47)
                                        :front #(48 49 50 51)
                                        :right #(52 53 54 55))))
  (stencil :index 22)
  (contact :predicate 'contact-overlapsp
           :vulnerability '(axe)
           :send '(damage-player) ;;  remove-missile
           :receive '(player-kill-monster kill-monster))
  (move 'move-follow (:speed 6 :crossp t)))

(define-element (head)
  (costume :index 56)
  (animation 'animation-ping-pong (:indexes '(:front #(56 57 58 59))))
  (stencil :index 23)
  (contact :predicate 'contact-overlapsp
           :vulnerability '(flail)
           :send '(damage-player) ;; remove-missile
           :receive '(player-kill-monster kill-monster))
  (move 'move-follow (:speed 7)))

(define-element (magus)
  (costume :index 76)
  (stencil :index 25)
  (animation 'animation-ping-pong (:indexes '(:left #(72 73 74 75)
                                      :front #(76 77 78)
                                      :right #(79 80 81 82))))
  (contact :predicate 'contact-overlapsp
           :vulnerability '(sword)
           :send '(damage-player) ;; remove-missile
           :receive '(player-kill-monster kill-monster))
  (move 'move-follow-horizontal (:speed 8)))

(define-element (wildboar)
  (costume :index 83)
  (stencil :index 26)
  (animation 'animation-ping-pong (:indexes '(:front #(83 84 85 86))))
  (contact :predicate 'contact-overlapsp
           :vulnerability '(flail)
           :send '(damage-player) ;; remove-missile
           :receive '(player-kill-monster kill-monster))
  (move 'move-follow (:speed 8)))

(define-element (fire)
  (costume :index 98)
  (stencil :index 30)
  (animation 'animation-loop (:indexes '(:front #(98 99))))
  (contact :predicate 'contact-overlapsp
           :send '(damage-player)))

(define-element (pedestal)
  (contact :predicate 'sanctuary-pedestal-contactp
           :send '(sanctuary-pedestal-end-game)))

(define-element (apprentice)
  (costume :index 36)
  (stencil :index 21)
  (apprentice)
  (animation 'animation-ping-pong (:indexes '(:front #(36 37 38 39))))
  (contact :predicate 'contact-overlapsp
           :send '(release-apprentice)))

(define-element (apprentice-sanctuary)
  (costume :index 36)
  (apprentice)
  (animation 'animation-ping-pong (:indexes '(:front #(36 37 38 39)))))

(define-element (splash)
  (costume :index 87 :depth :foreground)
  (animation 'animation-run-destroy (:indexes '(:front #(87 88 89 90)))
             :next 'drown))

(define-element (drown)
  (costume :index 91 :depth :foreground)
  (animation 'animation-loop (:indexes '(:front #(91 92)))))


(define-element (wheel-of-light)
  (costume :index 40)
  (move 'move-directed (:speed 8 :crossp t) :directions '(move-up))
  (animation 'animation-loop (:indexes '(:front #(40 41 42 43)))))

(define-element (sorcerer)
  (costume :index 64)
  (stencil :index 24)
  (animation 'animation-ping-pong (:indexes '(:front #(64 65 66 67)
                                              :left #(60 61 62 63)
                                              :right #(68 69 70 71))))
  (move 'move-keyboard (:speed 3))
  (keyboard)
  (player)
  (contact :predicate 'contact-overlapsp
           :send '(player-kill-monster)
           :priority 0
           :receive '(pickup drown damage-player release-apprentice
                      heal-player open-door
                      open-removable-door
                      sanctuary-pedestal-end-game)))

(define-element (door-left :quadtreep t)
  (costume :index 20)
  (animation 'animation-run-stop (:state 0 :indexes '(:front #(20 21 22 23))))
  (stencil :index 18)
  (contact :predicate 'door-left-contactp
           :send '(open-door))
  (door :in 'move-left :out 'move-right))

(define-element (door-right :quadtreep t)
  (costume :index 24)
  (animation 'animation-run-stop (:state 0 :indexes '(:front #(24 25 26 27)))) 
  (stencil :index 19)
  (contact :predicate 'door-right-contactp
           :send '(open-door))
  (door :in 'move-right :out 'move-left))

(define-element (door-green :quadtreep t)
  (costume :index 93)
  (stencil :index 27)
  (contact :predicate 'door-green-contactp
           :send '(open-removable-door))
  (door))

(define-element (door-trap :quadtreep t)
  (costume :index 95 :depth :foreground)
  (stencil :index 28)
  (contact :predicate 'door-trap-contactp
           :send '(open-removable-door))
  (door))

(define-element (cauldron :quadtreep t)
  (costume :index 17 :depth :foreground)
  (stencil :index 17)
  (animation 'animation-loop (:indexes '(:front #(17 18))))
  (contact :predicate 'cauldron-contactp
           :send '(heal-player)))

(defmethod add-element ((a area) (elmt-type (eql 'magic-missile)) x y &rest args)
  (let* ((entity-manager (area-em a))
         (entity (make-entity entity-manager))
         (move-component (make-instance 'move :speed 2 :crossp t)))
    (setf (move-strategy move-component)
          (make-instance 'move-directed :directions (getf args :move-directions)
                                         :component move-component))
    (add-components entity-manager entity 
                    (make-instance 'costume
                                   :x x :y y
                                   :w +element-width+
                                   :h +element-height+
                                   :index 97
                                   :asset :sprites
                                   :depth :background)
                    move-component
                    (make-instance 'stencil :index 29 :asset :sprites-stencils)
                    (make-instance 'contact
                                   :predicate 'contact-overlapsp
                                   :receive '(remove-missile)
                                   :send '(kill-monster)))))

(defun add-4-magic-missiles (game x y)
  (let ((area (game-current-area game)))
    (add-element area 'magic-missile x y :move-directions '(move-up))
    (add-element area 'magic-missile x y :move-directions '(move-down))
    (add-element area 'magic-missile x y :move-directions '(move-left))
    (add-element area 'magic-missile x y :move-directions '(move-right))))

(defun add-8-magic-missiles (game x y)
  (let ((area (game-current-area game)))
    (add-4-magic-missiles game x y)
    (add-element area 'magic-missile x y :move-directions '(move-up move-left))
    (add-element area 'magic-missile x y :move-directions '(move-up move-right))
    (add-element area 'magic-missile x y :move-directions '(move-down move-left))
    (add-element area 'magic-missile x y :move-directions '(move-down move-right))))

;; (defmethod add-element ((a area) (elmt-type (eql 'bag)) x y &rest args)
;;   (let* ((entity-manager (area-em a))
;;          (entity (make-entity entity-manager)))
;;     (add-components entity-manager entity 
;;                     (make-instance 'costume
;;                                    :x 0 :y 300
;;                                    :w +element-width+
;;                                    :h +element-height+
;;                                    :index 114
;;                                    :asset :sprites
;;                                    :depth :background)
;;                     (make-instance 'costume
;;                                    :x 48 :y 300
;;                                    :w +element-width+
;;                                    :h +element-height+
;;                                    :index 115
;;                                    :asset :sprites
;;                                    :depth :background)
;;                     (make-instance 'costume
;;                                    :x 0 :y 348
;;                                    :w +element-width+
;;                                    :h +element-height+
;;                                    :index 116
;;                                    :asset :sprites
;;                                    :depth :background)
;;                     (make-instance 'costume
;;                                    :x 48 :y 348
;;                                    :w +element-width+
;;                                    :h +element-height+
;;                                    :index 117
;;                                    :asset :sprites
;;                                    :depth :background))))

(define-element (river-block)
  (costume :index 110)
  (stencil :index 32)
  (animation 'animation-loop (:indexes '(:front #(110 111 112 113))))
  (contact :predicate 'contact-overlapsp
           :send '(drown)))

(defmethod add-element ((a area) (elmt-type (eql 'river)) x y &rest args)
  (let ((river-size (getf args :size)))
    (loop repeat river-size
          for xi = x then (+ xi 32)
          do (add-element a 'river-block xi y))))

(define-element (waterfall-block)
  (costume :index 106)
  (animation 'animation-loop (:indexes '(:front #(106 107 108 109)))))

(defmethod add-element ((a area) (elmt-type (eql 'waterfall)) x y &rest args)
  (let ((width (getf args :width))
        (height (getf args :height)))
    (loop repeat height
          for yi = y then (+ yi 16)
          do (loop repeat width
                   for xi = x then (+ xi 32)
                   do (add-element a 'waterfall-block xi yi)))))

(defmethod add-tile ((a area) quadtree index x y depth)
  (let* ((entity-manager (area-em a))
         (entity (make-entity entity-manager)))
    (add-component entity-manager entity
                   (make-instance 'costume :index index :asset :tiles :depth depth
                                           :x x :y y :w +tile-width+ :h +tile-height+))
    (add-component entity-manager entity
                   (make-instance 'stencil :index index :asset :tiles-stencils))
    (when (equal depth :background)
      (qt:quadtree-insert quadtree entity x y +tile-width+ +tile-height+))))

(defun new-area (id name bg-tiles fg-tiles sprites)
  (let* ((entity-manager (make-instance 'entity-manager))
         (qt (make-instance 'qt:quadtree :x 0 :y 0 :width 640 :height 288))
         (area (make-instance 'area :id id :location name :em entity-manager
                                    :quadtree qt)))

    ;; Add entity to control the player position under the central pedestal
    ;; in the Sanctuary
    (when (eql name 'sanctuary)
      (add-element area 'pedestal 0 0)) 
    
    ;; Add background tiles sprites
    (loop for tile in bg-tiles
	  do (apply #'add-tile area (append (list qt) tile '(:background))))

    ;; Add foreground tiles sprites
    (loop for tile in fg-tiles
	  do (apply #'add-tile area (append (list qt) tile '(:foreground))))
    
    ;; Add sprites
    (loop for sprite in sprites
          do (apply #'add-element area sprite))
    
    area))


(defun build-area (world area)
  (let ((keys '(:location :bg-tiles :fg-tiles :elements))
        (id (getf area :id)))
    (setf (gethash id world)
          (apply #'new-area id (mapcar #'(lambda (key) (getf area key)) keys)))))

(defun build-intro ()
  (let ((area (make-instance 'area
                             :id 'intro
                             :em (make-instance 'entity-manager))))
    (add-component (area-em area) (make-entity (area-em area)) 
                   (make-instance 'costume :index 0 :asset :intro
                                           :x 0 :y 0 :w 640 :h 288))
    (loop for pos in '((10 . 38) (286 . 72) (566 . 38))
          for x = (car pos)
          for y = (cdr pos)
          do (add-element area 'fire x y))
    area))

(defun build-world (game)
  "Build Word areas according *world-data*. 
   Return world, a hash table, which :
   - key is the id of the area
   - value a area object"
  (let ((world (game-world game)))
    (dolist (area *world-data*)
      (build-area world area))
    (setf (gethash 'intro world) (build-intro))))

(defun world-area (world name)
  (gethash name world nil))
