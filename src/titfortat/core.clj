(ns titfortat.core
  (:use [titfortat.players]))
;decision
;           Cooperate Defect
;Cooperate  3  3      0   5
;Defact     5  0      1   1

(defmulti  point-system (fn [x y] [x y]))
(defmethod point-system [:cooperate :cooperate] [x y] 3)
(defmethod point-system [:defact :defact]       [x y] 1)
(defmethod point-system [:defact :cooperate]    [x y] 5)
(defmethod point-system [:cooperate :defact]    [x y] 0)

(defn points [player opponent]
  (update-in player [:score] +
             (point-system (first (:decisions player))
                           (first (:decisions opponent)))))

(defn game
"Takes 2 players and how many roundes the play"
  [p1 p2 rounds]
  (if (pos? rounds)
    (let [p1-c (choose p1 p2) p2-c (choose p2 p1)]
      (recur
       (points p1-c p2-c)
       (points p2-c p1-c)
       (dec rounds)))
    [(:name p1) (:score p1) (:name p2) (:score p2)]))

(defn allvsall [coll]
  (loop [coll coll output []]
    (if (nil? coll)
      output
      (recur (next coll)
             (into output
                   (vec (map (fn [_] [(first coll) _]) coll)))))))

(declare scoresort name-score score-filter)

(defn tournament [players rounds]
  (scoresort (map (fn [[p o]] (game p o rounds))
                  (allvsall players))
             players))

(defn scoresort [scoredata players]
  (apply merge
         (map name-score
            (map (partial score-filter scoredata) 
                   (set (map :name players))))))

(defn name-score [coll]
  {(first (first coll))
   (apply + (map (fn [[pname val]] val) coll))})

(defn score-filter [scoredata index-name]
    (filter
     (fn [[name value]] (= name index-name))
     (partition 2 (flatten scoredata))))
