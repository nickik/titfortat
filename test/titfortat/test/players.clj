(ns titfortat.test.players
  (:use [titfortat.players])
  (:use [clojure.test]))

(deftest tft-move-test
  (is (= (tft-move [] []) :cooperate))
  (is (= (tft-move [] [:defact]) :defact))
  (is (= (tft-move [] [:cooperate]) :cooperate)))

(deftest decision-cons-test
  (is (= (decison-cons {:score 0 :decisions [] :name "nice"} :defact)
         {:score 0, :decisions [:defact], :name "nice"})))

(deftest randomnum-test
  (is (some true? (for [_ (range 100)] (randomnum 5)))))







