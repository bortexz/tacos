(ns bortexz.tacos.timeseries-test
  (:require [clojure.test :refer [deftest testing is]]
            [bortexz.tacos.timeseries :as ts]
            [clojure.data.avl :as avl]))

(deftest base-ops
  (testing "create"
    (let [sm (avl/sorted-map 1 1 2 2)]
      (is (= sm (ts/create sm)))
      (is (= sm (ts/create {1 1 2 2})))
      (is (= sm (ts/create [[1 1] [2 2]])))))

  (testing "latest"
    (let [c (ts/create {1 2 3 4})
          [lk lv] (ts/latest c)]
      (is (= 3 lk))
      (is (= 4 lv))))

  (testing "earliest"
    (let [c (ts/create {1 2 3 4})
          [lk lv] (ts/earliest c)]
      (is (= 1 lk))
      (is (= 2 lv))))

  (testing "nearest"
    (let [c (ts/create {1 2 5 6})
          [nk nv] (ts/nearest c >= 3)]
      (is (= 5 nk))
      (is (= 6 nv))))

  (testing "shift"
    (let [c (ts/create {1 2 3 4})
          [sk sv] (ts/shift c 1 1)]
      (is (= 3 sk))
      (is (= 4 sv)))

    (let [c (ts/create {1 2 3 4})
          [sk sv] (ts/shift c 1 2)]
      (is (nil? sk))
      (is (nil? sv)))))

(deftest view-ops
  (testing "view"
    (let [sm (ts/create (zipmap (range 10) (range 10)))
          v  (ts/view sm {:startk 3 :endk 8 :vf val})]
      (is (= [4 5 6 7 8 9] (mapv inc v)))
      (is (= [3 4 5 6 7 8] (reduce conj [] v)))
      (is (= 5 (nth v 2)))
      (is (= [0 1 2 3 4 5] (reduce-kv (fn [v idx _] (conj v idx)) [] v)))
      (is (= 6 (count v)))))

  (testing "tail"
    (let [sm (ts/create (zipmap (range 10) (range 10)))
          t  (ts/tail sm 5)]
      (is (= [5 6 7 8 9] (into [] t))))
    (let [sm (ts/create (zipmap (range 10) (range 10)))
          t  (ts/tail sm 5 {:endk 5})]
      (is (= [1 2 3 4 5] (into [] t)))))

  (testing "select"
    (let [sm (ts/create (zipmap (range 10) (range 10)))
          t  (ts/tail sm 5)]
      (is (= [0 5] (ts/select t <=)))
      (is (= [4 9] (ts/select t >=))))))

(deftest keep-latest-test
  (testing "keep-latest"
    (let [sm (ts/create (zipmap (range 10) (range 10)))
          exp (ts/create (zipmap (range 5 10) (range 5 10)))
          kl (ts/keep-latest sm 5)]
      (is (= exp kl)))))

(deftest delta-timeline-test
  (testing "delta timeline"
    (let [sm (ts/create (zipmap (range 10) (range 10)))
          exp (ts/create (zipmap (range 9 12) (range 9 12)))
          dt (ts/delta-timeline [10 11] 3)
          new-sm (ts/-apply-timeline dt sm (fn [v ts] ts))]
      (is (= new-sm exp)))))
