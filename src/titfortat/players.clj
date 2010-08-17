(ns titfortat.players)

(declare tft-move ftft-move decison-cons)

(defn randomnum [n]
  (< (rand-int 100) n))

(defprotocol Player
  (choose  [player opponent]))

(defrecord nice [score decisions name]
  Player
  (choose [this opp] (decison-cons this :cooperate)))

(defrecord asshole [score decisions name]
  Player
  (choose [this opp] (decison-cons this :defact)))

(defrecord randomplayer [score decisions name]
  Player
  (choose [this opp] (decison-cons this
                                   (rand-nth [:defact :cooperate]))))

(defrecord titfortat [score decisions name]
  Player
  (choose [this opp]
          (decison-cons this
                        (tft-move decisions (:decisions opp)))))

(defrecord forgiving-titfortat [score decisions name]
  Player
  (choose [this opp]
          (decison-cons this
                        (ftft-move decisions (:decisions opp)))))

(defn ftft-move [my-dec opp-dec]
  (let [dec  (tft-move my-dec opp-dec)]
    (if (= dec :defact)
      (if (randomnum 5) :cooperate :defact)
      :cooperate)))

(defn tft-move
"calculate a titfortats players move"
[my-dec opp-dec]
  (if (empty? opp-dec)
    :cooperate
    (if (= :cooperate (first opp-dec))
      :cooperate
      :defact)))

(defn decison-cons [player  decision]
  (assoc player :decisions
         (vec (cons decision (:decisions player)))))

(defmacro init-player
  ([sym] `(~(symbol (str sym ".")) 0 [] ~sym)))


