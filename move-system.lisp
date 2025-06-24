(in-package :sorcery-sdl2)

(defun overlapsp (game entity-manager entity1 x1 y1 w1 h1 entity2 x2 y2 w2 h2)
  (let ((stencil1 (entity-component entity-manager entity1 'stencil))
        (stencil2 (entity-component entity-manager entity2 'stencil)))
    (and stencil1 stencil2
         (s2e:intersectp x1 y1 w1 h1 x2 y2 w2 h2)
         (let ((sheet1 (stencil-asset stencil1))
               (index1 (stencil-index stencil1))
               (sheet2 (stencil-asset stencil2))
               (index2 (stencil-index stencil2)))
           (s2e:stencils-intersectp game sheet1 index1 x1 y1 sheet2 index2 x2 y2)))))

(defun entities-overlapsp (game entity-manager
                          entity x-next-position y-next-position width height
                          entities)
  (loop for other in (remove entity entities)
        for other-costume = (entity-component entity-manager other 'costume)
        for other-x = (costume-x other-costume)
        for other-y = (costume-y other-costume)
        for other-w = (costume-w other-costume)
        for other-h = (costume-h other-costume)

        when (overlapsp game entity-manager
                       entity x-next-position y-next-position width height
                       other other-x other-y other-w other-h)
          do (return-from entities-overlapsp t))
  nil)

(defun inside-board-p (x y width height)
  (and (>= x 0)
       (>= y 0)
       (<= (+ x width) *game-screen-w*)
       (<= (+ y height) *game-screen-h*)))

(defun update-animation-direction (animation move)
  (let ((stack (move-stack move)))
   (cond
     ((member 'move-left stack) (update-direction animation :left))
     ((member 'move-right stack) (update-direction animation :right))
     (t (update-direction animation :front)))))

(defmacro with-coords ((x y) (pos) &body body)
  `(let ((,x (first ,pos)) (,y (second ,pos)))
     ,@body))

(defclass move-system (system) ())

(defmethod update ((system move-system) entity-manager game)
  "- Update stack of move components
   - Compute next positions
   - Check collisions
   - Update costume coords
   - clear move stacks
   - Remove entities outside the board (magic-missiles)"
  
  (let ((area (game-current-area game)))
    
    (dolist (entity (find-entities entity-manager 'move))
      (let ((move (entity-component entity-manager entity 'move)))
        (s2e:with-ticks ((move-lt move) (move-speed move))
          (let ((allowed-pos '())
                (costume (entity-component entity-manager entity 'costume))
                (animation (entity-component entity-manager entity 'animation)))
            (update-stack move entity-manager entity)
            (when animation
              (update-animation-direction animation move))
            (with-costume-geometry (costume x y w h)
                
              (dolist (pos (move-next-positions move x y))
                (with-coords (xi yi) (pos)
                  (when (and xi yi)
                    (cond
                      ((move-crossp move) (push pos allowed-pos))
                      ((and (inside-board-p xi yi w h)
                            (not (entities-overlapsp game
                                                     entity-manager
                                                     entity
                                                     xi yi w h
                                                     (qt:quadtree-retrieve
                                                      (area-qt area)
                                                      xi yi w h))))
                       (push pos allowed-pos))))))
                
              (with-coords (xn yn) ((first (reverse allowed-pos)))
                (when (and xn yn) (update-coords costume xn yn))))
            (clear-stack move)
            ;; remove entity if outside board
            (with-costume-geometry (costume x2 y2 w h)
              (when (and (move-crossp move) (not (inside-board-p x2 y2 w h)))
                (pushnew entity (game-trash game))))))))))
