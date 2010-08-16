(ns titfortat.core)
;decision
;           Cooperate Defect
;Cooperate  3  3      0   5
;Defact     5  0      1   1
(declare tft-move ftft-move)

(defprotocol Player
  (choose  [player opponent])
  (getname [player]))

(defn decison-cons [player  decision]
  (assoc player :decisions
         (vec (cons decision (:decisions player)))))

(defrecord nice [score decisions]
  Player
  (choose [this opp] (decison-cons this :cooperate))
  (getname [_] "nice"))

(defrecord asshole [score decisions]
  Player
  (choose [this opp] (decison-cons this :defact))
  (getname [_] "asshole"))

(defrecord randomplayer [score decisions]
  Player
  (choose [this opp] (decison-cons this
                                   (nth [:defact :cooperate]
                                        (rand-int 2))))
  (getname [_] "randomplayer"))

(defrecord titfortat [score decisions]
  Player
  (choose [this opp]
          (decison-cons this
                        (tft-move decisions (:decisions opp))))
  (getname [_] "titfortat"))

(defrecord forgiving-titfortat [score decisions]
  Player
  (choose [this opp]
          (decison-cons this
                         (ftft-move decisions (:decisions opp)))))

(defn ftft-move [my-dec opp-dec]
  (let [dec  (tft-move my-dec opp-dec)]
    (if (= dec :defact)
      (if (= (rand-int 100) (rand-int 100)) :cooperate :defact)
      :cooperate)))

(defn tft-move [my-dec opp-dec]
  (if (empty? my-dec)
    :cooperate
    (if (= :cooperate (first opp-dec))
      :cooperate
      :defact)))

(def players [(nice. 0 [])
              (asshole. 0 [])
              (titfortat. 0 [])
              (forgiving-titfortat. 0 [])
              (randomplayer. 0 [])])

(defmulti  point-system (fn [x y] [x y]))
(defmethod point-system [:cooperate :cooperate] [x y] 3)
(defmethod point-system [:defact :defact]       [x y] 1)
(defmethod point-system [:defact :cooperate]    [x y] 5)
(defmethod point-system [:cooperate :defact]    [x y] 0)

(defn points [player opponent]
  (update-in player [:score] +
             (point-system (first (:decisions player))
                           (first (:decisions opponent)))))

(defn game [p1 p2 rounds]
  (if (pos? rounds)
    (let [p1-c (choose p1 p2) p2-c (choose p2 p1)]
      (recur
       (points p1-c p2-c)
       (points p2-c p1-c)
       (dec rounds)))
    [(getname p1) (:score p1) (getname p2) (:score p2)]))

(defn allvsall [coll]
  (loop [coll coll output []]
    (if (seq coll)
      (recur (next coll)
             (into output (vec (map (fn [_] [(first coll) _])  coll))))
      output)))

(declare scoresort name-score score-filter)

(defn tournament [player rounds]
  (scoresort (map (fn [[p o]] (game p o rounds)) (allvsall players))  players))

(defn scoresort [scoredata players]
  (apply merge
         (map name-score
            (map (partial score-filter scoredata) 
                   (set (map getname players))))))

(defn name-score [coll]
  {(first (first coll))
   (apply + (map (fn [[pname val]] val) coll))})

(defn score-filter [scoredata index-name]
    (filter
     (fn [[name value]]
       (= name index-name))
     (partition 2 (flatten scoredata))))

