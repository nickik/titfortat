(ns titfortat.test.core
  (:use [titfortat.core] :reload-all)
  (:use [titfortat.players]) 
  (:use [clojure.test])
  (:import [titfortat.players nice])
  (:import [titfortat.players asshole]))

(deftest allvsall-test
  (is (= (allvsall [1 2 3]) [[1 1] [1 2] [1 3] [2 2] [2 3] [3 3]])))

(deftest score-filter-test
  (is (= (score-filter ["name" 5 "name" 6 "test" 9] "name")
         '(("name" 5) ("name" 6)))))

(deftest name-score-test'0
  (is (= (name-score [["name" 5] ["name" 5]])
         {"name" 10})))

(deftest scoresort-test
  (let [players [(nice. 0 [] "nice") (asshole. 0 [] "asshole")]
        data    [["nice" 3 "nice" 3]
                 ["nice" 0 "asshole" 5]
                 ["asshole" 1 "asshole" 1]]]
    (is (= (scoresort data  players) {"nice" 6, "asshole" 7}))))

(deftest point-system-test
  (are [x y] (= x y)
       1 (point-system :defact :defact)
       3 (point-system :cooperate :cooperate)
       5 (point-system :defact :cooperate)
       0 (point-system :cooperate :defact)))

(deftest points-test
  (is (= (:score (points
           (asshole. 0 [:defact] "asshole")
           (nice. 0 [:cooperate] "nice")))
         5)))

(deftest game-test
  (let [p1 (init-player "asshole")
        p2 (init-player "nice")]
    (is (= (game p1 p2 5)
           ["asshole" 25 "nice" 0]))))

(deftest tournament-test
  (let [p1 (init-player "asshole")
        p2 (init-player "nice")]
    (is (= (tournament [p1 p2] 5) {"nice" 30, "asshole" 35}))))
