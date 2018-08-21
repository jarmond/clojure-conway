(ns conway.core
  (:gen-class))

; Dimensions of board
(def dim 50)
; Randomize 5%
(def sparse-fraction 0.15)
(def timestep-ms 100)
(def render-ms 80)
(def randomizer-ms 3000)
(def running true)
(def restart (ref false))

(def board
  (vec (map (fn [_]
              (vec (map (fn [_] (ref :dead))
                        (range dim))))
            (range dim))))

; x is column, y is row
(defn position [[x y]]
  (-> board (nth y) (nth x)))

(defn rand-state []
  (case (rand-int 2)
    0 :dead
    1 :alive))

(defn random-start [sparse?]
  (dosync
   (doseq [row board]
     (doseq [pos row]
       (when (or (not sparse?) (< (rand) sparse-fraction))
         (alter pos (fn [_] (rand-state))))))))

(def directions
  [[1 0] [0 1] [-1 0] [0 -1]])

(defn bound [b x]
  "Bound value to [0,b)."
  (mod x b))

(defn bound-position [pos]
  "Bound a position to the board."
  (mapv (partial bound dim) pos))

(defn translate [[x y] [dx dy]]
  (bound-position [(+ x dx) (+ y dy)]))

(defn alive? [pos]
  (let [p (position pos)]
    (= @p :alive)))

(defn neighbour-count [pos]
  (->> directions
       (map (partial translate pos))
       (filter alive?)
       (count)))


(defn update-cell [pos]
  (let [c (neighbour-count pos)
        r (position pos)]
    (cond
      (or (< c 2) (> c 3)) (alter r (constantly :dead))
      (= c 3) (alter r (constantly :alive)))))

(defn update-board []
  (dosync
   (dorun (for [x dim y dim]
       (update-cell [x y])))))

(def updater (agent nil))
(defn updater-loop [_]
  (when running
    (send-off *agent* #'updater-loop))
  (dosync
   (when @restart
     (random-start false)
     (alter restart (constantly false))))
  (update-board)
  (. Thread (sleep timestep-ms))
  nil)

(def randomizer (agent nil))
(defn randomizer-loop [_]
  (when running
    (send-off *agent* #'randomizer-loop))
  (. Thread (sleep randomizer-ms))
  (dosync
   (random-start true)))

(defn reset-board []
  (dosync (alter restart (constantly true))))

;;;; UI

(defn cell->str [cell]
  (if (= cell :dead) "." "#"))

(defn print-board []
  (dosync
   (doseq [row board]
     (println (map (comp cell->str deref) row)))))

;;;; GUI

(import
 '(java.awt Color Graphics Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

; pixels per square
(def scale 10)

(defn fill-cell [#^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-cell [#^Graphics g cell [x y]]
  (fill-cell g x y (if (= cell :alive)
                     (. Color black)
                     (. Color white))))

(defn render [g]
  (let [v (dosync (vec (for [x (range dim) y (range dim)]
                         @(position [x y]))))
        img (new BufferedImage (* scale dim) (* scale dim)
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (dorun
     (for [x (range dim) y (range dim)]
       (render-cell bg (v (+ y (* x dim))) [x y])))
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(def panel (doto (proxy [JPanel] []
                   (paint [g] (render g)))
             (.setPreferredSize (new Dimension
                                     (* scale dim)
                                     (* scale dim)))))

(def frame (doto (new JFrame) (.add panel) .pack .show))

(def animator (agent nil))

(defn animation [_]
  (when running
    (send-off *agent* #'animation))
  (. panel (repaint))
  (. Thread (sleep render-ms))
  nil)

;;;; Main

(defn text-ui
  "Run Conway's Game of Life"
  [& args]
  (random-start false)
  (loop [i 0]
    (println "Iteration " i)
    (print-board)
    (update-board)
    (. Thread sleep timestep-ms)
    (recur (inc i)))
   0)

(defn panel-ui [& args]
  (random-start false)
  (send-off updater updater-loop)
  (send-off animator animation)
  (send-off randomizer randomizer-loop))
