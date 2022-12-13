 (ns bortexz.tacos-test
   (:require [clojure.test :refer [deftest is]]
             [bortexz.tacos :as ta]
             [clojure.edn :as edn]
             [bortexz.tacos.timeseries :as ts]
             [bortexz.utils.math :as umath]
             [bortexz.graphcom :as g]))

(defn- create-source-nodes
  [{:keys [input max-size transform] :or {transform double}}]
  (let [[candles-tl candles-ts] (ta/delta-source {:input input} {:max-size max-size})

        cmap (ta/spread-map candles-tl candles-ts {:ks [:open :close :high :low :volume]
                                                   :transform transform})

        sources (merge {::tl candles-tl
                        ::candles candles-ts}
                       cmap
                       {:src (:close cmap)
                        :price (:close cmap)})]
    sources))

(def ^:private input          (g/input-node))
(def ^:private sample-candles (ts/create
                               (map (juxt :timestamp identity)
                                    (edn/read-string (slurp "resources/test_candles.edn")))))
(def ^:private base-sources   (create-source-nodes {:input input :max-size (count sample-candles) :transform double}))
(def ^:private base-tl        (::tl base-sources))

(defn- process
  [nodes i]
  (let [g   (g/graph (merge {::input input} nodes))
        ctx (g/context g)]
    (-> ctx
        (g/process {::input i})
        (g/values))))

;; Indicator utils tests

(deftest map-some
  (let [gv (process (merge {::sut (ta/map-some base-tl + (:close base-sources) (:open base-sources))}
                           base-sources)
                    sample-candles)]
    (is (every? (fn [[k v]]
                  (== v (+ (get (:open gv) k) (get (:close gv) k))))
                (::sut gv)))))

(deftest sources-map
  (let [gv (process {::sut (ta/sources-map base-tl (select-keys base-sources [:open :close]))} sample-candles)
        v  (val (ts/latest (::sut gv)))]
    (is (and (contains? v :open) (contains? v :close)))))

(deftest spread-map
  (let [gv (process (merge (ta/spread-map base-tl (::candles base-sources) {:ks [:open :close]})
                           {:candles (::candles base-sources)})
                    sample-candles)
        candles (:candles gv)
        open (:open gv)
        close (:close gv)]
    (is (every? (fn [[k v]] (= v (:open (get candles k)))) open))
    (is (every? (fn [[k v]] (= v (:close (get candles k)))) close))))


(deftest mean
  (let [{:keys [open close]} base-sources
        sut (ta/mean base-tl open close)

        gv (process (merge base-sources {::sut sut}) sample-candles)
        openv (:open gv)
        closev (:close gv)]
    (is (every? (fn [[k v]]
                  (== v (umath/mean [(get openv k) (get closev k)])))
                (::sut gv)))))

;; Indicators

(defn base->
  ([i] (i base-tl base-sources))
  ([i opts] (i base-tl base-sources opts)))

(defn base-map->
  ([i ks]
   (ta/sources-map base-tl (zipmap ks (i base-tl base-sources))))
  ([i ks opts]
   (ta/sources-map base-tl (zipmap ks (i base-tl base-sources opts)))))

(def ^:private indicators-map
  {::ma (base-> ta/simple-moving-average {:period 10})
   ::wma (base-> ta/weighted-moving-average {:period 10})
   ::ema (base-> ta/exponential-moving-average {:period 15})
   ::dema (base-> ta/double-exponential-moving-average {:period 5})
   ::tema (base-> ta/triple-exponential-moving-average {:period 5})
   ::rma (base-> ta/relative-moving-average {:period 15})
   ::ssma (base-> ta/smoothed-moving-average {:period 15})
   ::vwma (base-> ta/volume-weighted-moving-average {:period 15})
   ::macd (base-map-> ta/moving-average-convergence-divergence [:macd :signal :hist] {:fast-opts {:period 5}
                                                                                      :slow-opts {:period 10}
                                                                                      :signal-opts {:period 9}})
   ::ppo (base-map-> ta/percentage-price-oscillator [:ppo :signal :hist] {:fast-opts {:period 5}
                                                                          :slow-opts {:period 10}
                                                                          :signal-opts {:period 9}})
   ::meandev (base-> ta/mean-deviation {:period 10})
   ::stdev (base-> ta/standard-deviation {:period 10})
   ::psar  (base-> ta/parabolic-stop-and-reverse {:increment 0.02 :start 0.02 :max-value 0.2})
   ::stoch (base-map-> ta/stochastic-oscillator
                       [:k-line :d-line]
                       {:k-line-opts {:period 10}
                        :avg-k-opts {:period 10}
                        :avg-d-opts {:period 10}})
   ::rsi (base-> ta/relative-strength-index {:period 10})
   ::cci (base-> ta/commodity-channel-index {:period 10})
   ::rvi (base-> ta/relative-volatility-index {:period 10})
   ::rrvi (base-> ta/refined-relative-volatility-index {:period 10})
   ::br (base-> ta/base-range)
   ::tr (base-> ta/true-range)
   ::abr (base-> ta/average-base-range  {:period 10})
   ::atr (base-> ta/average-true-range {:period 14})
   ::bb (base-map-> ta/bollinger-bands [:up :mid :low] {:period 10 :multiplier 2})
   ::kc (base-map-> ta/keltner-channels [:up :mid :low] {:period 10 :multiplier 2})
   ::adx (base-map-> ta/average-directional-index [:adx :dm+ :dm-] {:period 10})
   ::obv (base-> ta/on-balance-volume)
   ::adl (base-map-> ta/accumulation-distribution-line [:adl :mfm :mfv])
   ::highest (base-> ta/highest-val {:period 10})
   ::lowest (base-> ta/lowest-val {:period 10})
   ::since-highest (base-> ta/since-highest-val {:period 10})
   ::since-lowest (base-> ta/since-lowest-val {:period 10})
   ::aroon (base-map-> ta/aroon-oscillator [:aroon :aup :adown] {:period 10})
   ::chexits (base-map-> ta/chandelier-exits [:long :short] {:period 10 :multiplier 1})
   ::donchian (base-map-> ta/donchian-channels [:mid :highest :lowest] {:period 10})
   ::supertrend (base-> ta/supertrend {:multiplier 3 :period 10})
   ::ker (base-> ta/kaufman-efficiency-ratio {:period 10})
   ::kama (base-> ta/kaufman-adaptive-moving-average {:fast-period 5 :slow-period 10 :er-period 10})})

(def ^:private indicators-vals
  (process indicators-map sample-candles))

(comment
  ; manual test, with portal and Tradingview
  (tap> (update-vals indicators-vals (fn [i] (into (sorted-map) i))))
  (tap> (into (sorted-map) (::ma indicators-vals))))
