(ns bortexz.tacos
  (:require [clojure.math.numeric-tower :as math-nt]
            [bortexz.utils.math :as umath]
            [bortexz.graphcom :as g]
            [better-cond.core :as bc]
            [bortexz.tacos.timeseries :as ts]))

(defn derived
  "Creates a new derived timeseries node (a.k.a indicator) given the following args:
   - `tl` A graphcom node whose emitted value implements [[tacos.timeseires/Timeline]].
   - `sources` Map of dependency nodes. Timeline node will be merged into sources under `::timeline` keyword automatically.
   - `compute-ts` 3-arity fn that will be called with args [current-value sources-values timestamp], and must return the 
     value of this timeseries at `timestamp`. Note that timestamp can be any type that can be compared with `compare`. 
     You could use integers (e.g epoch milliseconds), java.time instants, ISO strings, etc
   
   Returns node that will emit a derived timeseries as value."
  [tl sources compute-ts]
  (g/compute-node
   (merge sources {::timeline tl})
   (fn derived-handler- [current-value {::keys [timeline] :as sources}]
     (ts/-apply-timeline timeline current-value (fn [v ts] (compute-ts v sources ts))))))

(defn delta-source
  "Sources:
   - `input` input node that contain partial timeseries of new items to introduce into the graph.

   Opts:
   - `max-size` the maximum size used for [[tacos.timeseries/delta-timeline]].
   
   Returns tuple of nodes [delta-tl src] that can be used to introduce new values into a graph of derived timeseries:
   - `delta-tl` is a node that emits [[tacos.timeseries/delta-timeline]] using `max-size` and the timestamps of timeseries 
      in `input`.
   - `src` is a timeseries node that accumulates new values from `input`, up to a maximum of `max-size`, presumably to 
     be used as the source for other derived timeseries."
  [{:keys [input] :as sources} {:keys [max-size] :as opts}]
  (let [tl  (g/compute-node
             sources
             (fn delta-entrypoint-tl- [_ {:keys [input]}]
               (ts/delta-timeline (keys input) max-size)))

        src (derived tl sources (fn [_ {:keys [input]} k] (get input k)))]
    [tl src]))

(defn map-some
  "Returns node that applies f to each timestamp value of each node, when current timestamp values for all nodes
   are non-nil.
   
   e.g: 
   - sum each timestamp's val of different timeseries `(map-some tl + x y z)`
   - mean of x,y,z nodes `(map-some tl (fn [& xs] (/ (reduce + 0 xs) (count xs))) x y z)`"
  ([tl f n1]
   (derived tl
            {:n1 n1}
            (fn map-some-1 [_ {:keys [n1]} k]
              (bc/when-some [v1 (get n1 k)]
                (f v1)))))
  ([tl f n1 n2]
   (derived tl
            {:n1 n1 :n2 n2}
            (fn map-some-2 [_ {:keys [n1 n2]} k]
              (bc/when-some [v1 (get n1 k)
                             v2 (get n2 k)]
                (f v1 v2)))))
  ([tl f n1 n2 n3]
   (derived tl
            {:n1 n1 :n2 n2 :n3 n3}
            (fn map-some-3 [_ {:keys [n1 n2 n3]} k]
              (bc/when-some [v1 (get n1 k)
                             v2 (get n2 k)
                             v3 (get n3 k)]
                (f v1 v2 v3)))))
  ([tl f n1 n2 n3 n4]
   (derived tl
            {:n1 n1 :n2 n2 :n3 n3 :n4 n4}
            (fn map-some-3 [_ {:keys [n1 n2 n3 n4]} k]
              (bc/when-some [v1 (get n1 k)
                             v2 (get n2 k)
                             v3 (get n3 k)
                             v4 (get n4 k)]
                (f v1 v2 v3 v4)))))
  ([tl f n1 n2 n3 n4 & nodes]
   (let [xm (zipmap (range) (into [n1 n2 n3 n4] nodes))
         ks (vec (range (count xm)))]
     (derived tl
              xm
              (fn map-some-n [_ xm k]
                (bc/when-let [xs (some->
                                  (reduce (fn [acc idx]
                                            (if-let [tsv (get (get xm idx) k)]
                                              (conj! acc tsv)
                                              (reduced nil)))
                                          (transient [])
                                          ks)
                                  (persistent!))]
                  (apply f xs)))))))

(defn sources-map
  "Returns timeseries node whose values combine current timestamp for all sources in a hash map, under same keyword as
   specified in `sources`, when all have a value.
   
   E.g candles (sources-map tl {:high high :low low :close close :open open}) will have values as map of 
   `{:open ,,, :close ,,, :high ,,, :low ,,,}`.
   
   Useful to combine multiple indicators that share the same timeline into one to use it as a single timeseries."
  [tl sources]
  (let [ks (keys sources)]
    (derived
     tl
     sources
     (fn [_ srcs ts]
       (some->
        (reduce (fn [acc k]
                  (if-let [v (get (get srcs k) ts)]
                    (assoc! acc k v)
                    (reduced nil)))
                (transient {})
                ks)
        (persistent!))))))

(defn spread-map
  "Given a timeseries node `map-source` that has maps as values and map of opts, returns a map of new nodes with 
   keys `ks` where values are new nodes that each one contains the values of key in map-src, possibly transformed.

   Sources:
   - `map-source`

   Opts:
   - `ks` set of keys to extract as indicators
   - `transform` function to transform each of the values, if needed. Defaults to identity.
   
   E.g (spread-map tl candles-node [:open :close :high :low]) returns a map of nodes #{:open :close :high :low}
   each one with the corresponding candle value."
  [tl map-source {:keys [ks transform] :or {transform identity}}]
  (zipmap ks (map #(map-some tl (comp transform %) map-source) ks)))

(defn momentum
  "src(i) - src(i-period)
   
   Sources:
   - `src`
   
   Opts:
   - `period` Defaults to 1."
  ([tl sources] (momentum tl sources {}))
  ([tl {:keys [src] :as sources} {:keys [period] :or {period 1}}]
   (derived
    tl
    sources
    (fn momentum- [_ {:keys [src]} k]
      (bc/when-let [curr (get src k)
                    prev (ts/shift src k (- period) {:vf val})]
        (- curr prev))))))

(defn rate-of-change
  "(src - src(i-period)) / src(i-period), vals between -1 and 1, except the case when src(i-period) is zero,
   then returns opt `nan-val`
   
   Sources:
   - `src`
   
   Opts:
   - `period` Defaults to 1.
   - `nan-val` defaults to Double/NaN"
  ([tl sources] (rate-of-change tl sources {}))
  ([tl {:keys [src] :as sources} {:keys [period nan-val] :or {period 1 nan-val Double/NaN}}]
   (derived
    tl
    sources
    (fn rate-of-change- [_ {:keys [src]} k]
      (bc/when-let [curr (get src k)
                    prev (ts/shift src k (- period) {:vf val})]
        (if (zero? prev)
          nan-val
          (/ (- curr prev) prev)))))))

(defn envelope
  "Returns tuple of nodes [upper lower], where:
   pos = src + (src * multiplier)
   neg = src - (src * multiplier)
   
   Sources:
   - `src
   
   Opts:
   - `multiplier` percentage number. e.g. 0.01 will create a 1% envelope around `src`."
  [tl {:keys [src]} {:keys [multiplier]}]
  (let [upper (map-some tl (fn [x] (+ x (* x multiplier))) src)
        lower (map-some tl (fn [x] (- x (* x multiplier))) src)]
    [upper lower]))

(defn mean
  "Returns node that computes the mean of all nodes specified.
   
   e.g ohlc4 = (mean tl open close high low) where open,close,high,low are indicators of each candle value."
  [tl & srcv]
  (apply map-some tl (fn [& xs] (umath/mean xs)) srcv))

(defn hl2
  "(high + low)/2
   
   Sources:
   - `high`
   - `low`"
  [tl {:keys [high low]}]
  (mean tl high low))

(defn hlc3
  "(high+low+close)/3
   
   Sources:
   - `high`
   - `low`
   - `close`"
  [tl {:keys [high low close]}]
  (mean tl high low close))

(defn ohlc4
  "(open+high+low+close)/4
   
   Sources:
   - `open`
   - `high`
   - `low`
   - `close`"
  [tl {:keys [open high low close]}]
  (mean tl open high low close))

(defn heikin-ashi
  "Returns node that calculates Heikin ashi candles whose values are maps of keys #{:open :close :high :low}

   Sources:
   - `open`
   - `close`
   - `high`
   - `low`
   
   Reference: 
   - https://www.investopedia.com/trading/heikin-ashi-better-candlestick/"
  [tl {:keys [open close high low] :as sources}]
  (derived
   tl
   sources
   (fn heikin-ashi- [ha {:keys [open close high low]} k]
     (let [[o c h l :as ohlc] (mapv #(get % k) [open close high low])]
       (when (every? some? ohlc)
         (let [prev-k (ts/shift close k -1 {:vf key})
               prev-ha (get ha prev-k)
               ha-close (umath/mean ohlc)
               ha-open (if prev-ha
                         (/ (+ (:open prev-ha) (:close prev-ha)) 2)
                         (/ (+ o c) 2))]
           {:open ha-open
            :close ha-close
            :high (max ha-open ha-close h)
            :low (min ha-open ha-close l)}))))))

(defn simple-moving-average
  "Reference:
   - https://www.tradingview.com/support/solutions/43000502589-moving-average/
   
   Sources:
   - `src`
   
   Opts:
   - `period`
     
   Notes:
   - Assumes that only latest time points are added (either new later timestamp, or replacement of current latest).
     This should be the case most of the time. For earlier than latest arriving time points, you need to recompute all 
     from earlier time point to latest for correct results, by specifying them on the timeline.
   
   - Keeps an internal summatory to work on log-n (n=size of timeseries) instead of reducing tail all the time."
  [tl {:keys [src] :as sources} {:keys [period]}]
  (let [acc (derived
             tl
             sources
             (fn [x {:keys [src]} k]
               (let [to-remove (ts/shift src k (- period) {:vf val})
                     to-add (get src k)
                     prev-k (ts/shift src k -1 {:vf key})
                     prev (when prev-k (get x prev-k))
                     {:keys [sum]} (or prev {:sum 0 :ready? false})]
                 {:sum (when to-add (+ (or sum 0) to-add (if to-remove (- to-remove) 0)))
                  :ready? (some? to-remove)})))]
    (derived
     tl
     {:acc acc}
     (fn [_ {:keys [acc]} k]
       (let [{:keys [sum ready?]} (get acc k)]
         (when ready? (/ sum period)))))))

(defn weighted-moving-average
  "Reference:
   - https://www.tradingview.com/support/solutions/43000594680-weighted-moving-average/

   Sources:
   - `src`
   
   Opts:
   - `period`"
  [tl {:keys [src] :as sources} {:keys [period]}]
  (let [divisor (/ (* period (inc period)) 2)]
    (derived
     tl
     sources
     (fn weighted-moving-average- [_ {:keys [src]} k]
       (bc/when-let [t (ts/tail src period {:endk k :vf val})]
         (/ (reduce-kv (fn [acc idx v]
                         (+ acc (* (inc idx) v)))
                       0
                       t)
            divisor))))))

(defn exponential-moving-average
  "Reference:
   - https://www.tradingview.com/support/solutions/43000592270-exponential-moving-average/
   
   Sources:
   - `src`
   
   Opts:
   - `period`
   - `multiplier` defaults to (/ 2 (inc period))."
  [tl {:keys [src] :as sources} {:keys [multiplier period]}]
  (let [multiplier (or multiplier (/ 2 (inc period)))]
    (derived
     tl
     sources
     (fn exponential-moving-average- [ema {:keys [src]} k]
       (bc/when-let [prev-k (ts/shift src k -1 {:vf key})
                     :let   [prev-ema (get ema prev-k)]
                     curr   (get src k)]
         (if prev-ema
           (+ (* curr multiplier)
              (* prev-ema (- 1 multiplier)))
           (ts/moving-average src prev-k period)))))))

(defn double-exponential-moving-average
  "Reference:
   - https://www.tradingview.com/support/solutions/43000589132-double-exponential-moving-average-ema/
   
   Sources:
   - `src`
   
   Opts:
   - `period` sent to internal exponential moving averages
   - `multiplier` sent to internal exponential moving averages
   - `ema1-opts` if specified, will be used instead of top level `opts` for ema1
   - `ema2-opts` if specified, will be used instead of top level `opts` for ema2"
  [tl {:keys [src] :as sources} {:keys [ema1-opts ema2-opts period] :as opts}]
  (let [ema1 (exponential-moving-average tl sources (or ema1-opts opts))
        ema2 (exponential-moving-average tl {:src ema1} (or ema2-opts opts))]
    (map-some tl
              (fn [ema1 ema2]
                (- (* 2 ema1) ema2))
              ema1
              ema2)))

(defn triple-exponential-moving-average
  "Reference:
   - https://www.tradingview.com/support/solutions/43000591346-triple-ema/

   Sources:
   - `src`
   
   Opts:
   - `period` sent to internal exponential moving averages
   - `multiplier` sent to internal exponential moving averages
   - `ema1-opts` if specified, will be used instead of top level `opts` for ema1
   - `ema2-opts` if specified, will be used instead of top level `opts` for ema2
   - `ema3-opts` if specified, will be used instead of top level `opts` for ema3"
  [tl {:keys [src] :as sources} {:keys [period ema1-opts ema2-opts ema3-opts] :as opts}]
  (let [ema1 (exponential-moving-average tl sources (or ema1-opts opts))
        ema2 (exponential-moving-average tl {:src ema1} (or ema2-opts opts))
        ema3 (exponential-moving-average tl {:src ema2} (or ema3-opts opts))]
    (map-some tl
              (fn [ema1 ema2 ema3]
                (+ (* 3 ema1)
                   (- (* 3 ema2))
                   ema3))
              ema1
              ema2
              ema3)))

(defn relative-moving-average
  "Relative (WildeR's) moving average, is an Exponential moving average using 1/period as multiplier.

   Sources:
   - `src`
   
   Opts:
   - `period`"
  [tl {:keys [src] :as sources} {:keys [period]}]
  (exponential-moving-average tl sources {:period period :multiplier (/ period)}))

(defn smoothed-moving-average
  "Reference:
   - https://download.esignal.com/products/workstation/help/charts/studies/rmi.htm

   Sources:
   - `src`
   
   Opts:
   - `period`"
  [tl {:keys [src] :as sources} {:keys [period]}]
  (derived
   tl
   sources
   (fn relative-moving-average- [curr {:keys [src]} k]
     (bc/when-let [c (get src k)]
       (let [prev-k (ts/shift src k -1 {:vf key})
             prev   (get curr prev-k)]
         (if-not prev
           (ts/moving-average src k period)
           (/ (+ (* prev (dec period)) c) period)))))))

(defn hull-moving-average
  "Reference:
   - https://www.tradingview.com/support/solutions/43000589149-hull-moving-average/

   Sources:
   - `src`
   
   Opts:
   - `period`"
  [tl {:keys [src] :as sources} {:keys [period] :as opts}]
  (let [base-wma (weighted-moving-average tl sources opts)
        n2-wma   (weighted-moving-average tl sources {:period (int (math-nt/floor (double (/ period 2))))})
        comb     (map-some tl (fn [b n2] (- (* n2 2) b)) base-wma n2-wma)]
    (weighted-moving-average tl {:src comb} {:period (int (math-nt/floor (math-nt/sqrt period)))})))

(defn volume-weighted-moving-average
  "Calculates volume-weighted-moving-average where given price and volume are timeseries nodes.
   
   Reference:
   - https://www.tradingsetupsreview.com/volume-weighted-moving-average-vwma/

   Sources: 
   - `price`
   - `volume`
   
   Opts:
   - `period`"
  [tl {:keys [src volume] :as sources} {:keys [period]}]
  (derived
   tl
   sources
   (fn volume-weighted-moving-average- [_ {:keys [src volume]} k]
     (bc/when-let [volume-tail (ts/tail volume period {:endk k :vf val})
                   src-tail (ts/tail src period {:endk k :vf val})]
       (/ (->> (map * volume-tail src-tail)
               (reduce + 0))
          (reduce + 0 volume-tail))))))

(defn moving-average-convergence-divergence-line
  "MACD base line. ma(src, fast-opts) - ma(src, slow-opts).
   
   Sources:
   - `src`
   
   Opts:
   - `fast-avg-indicator` defaults to [[exponential-moving-average]]
   - `fast-sources` if specified, overrides top level `sources` passed to `fast-avg-indicator`
   - `fast-opts` passed to `fast-avg-indicator`
   - `slow-avg-indicator` defaults to [[exponential-moving-average]]
   - `slow-sources` if specified, overrides top level `sources` passed to `slow-avg-indicator`
   - `slow-opts` passed to `slow-avg-indicator`"
  [tl
   {:keys [src] :as sources}
   {:keys [fast-avg-indicator fast-sources fast-opts slow-avg-indicator slow-sources slow-opts]
    :or   {fast-avg-indicator simple-moving-average
           slow-avg-indicator simple-moving-average}}]
  (map-some
   tl
   -
   (fast-avg-indicator tl (or fast-sources sources) fast-opts)
   (slow-avg-indicator tl (or slow-sources sources) slow-opts)))

(defn moving-average-convergence-divergence
  "Returns tuple of indicators [macd-line signal histogram].
   macd-line = macd-line(src, opts) See [[macd-line]]
   signal = signal-avg-indicator(macd, signal-opts)
   hist = macd - signal

   Reference:
   - https://www.tradingview.com/support/solutions/43000502344-macd-moving-average-convergence-divergence/

   Sources will be passed to [[moving-average-convergence-divergence-line]], if using defaults:
   - `src`
   
   Opts are those accepted by [[moving-average-convergence-divergence-line]], plus:
   - `signal-avg-indicator` defaults to [[exponential-moving-average]], must accept source as `:src` key
   - `signal-opts`"
  [tl
   {:keys [src] :as sources}
   {:keys [signal-avg-indicator signal-opts]
    :or   {signal-avg-indicator exponential-moving-average}
    :as opts}]
  (let [macd   (moving-average-convergence-divergence-line tl sources opts)
        signal (signal-avg-indicator tl {:src macd} signal-opts)
        hist   (map-some tl - macd signal)]
    [macd signal hist]))

(defn percentage-price-oscillator-line
  "percentage-price-oscillator base line, values between -1 and 1.
   
   Sources: 
   - `src`
   
   Opts:
   - `fast-avg-indicator` defaults to [[exponential-moving-average]]
   - `fast-sources` if specified, overrides top level `sources` passed to `fast-avg-indicatorº`
   - `fast-opts` passed to `fast-avg-indicator`
   - `slow-avg-indicator` defaults to [[exponential-moving-average]]
   - `slow-opts` passed to `slow-avg-indicator`"
  [tl
   {:keys [src] :as sources}
   {:keys [fast-avg-indicator fast-sources fast-opts slow-avg-indicator slow-sources slow-opts]
    :or   {fast-avg-indicator exponential-moving-average
           slow-avg-indicator exponential-moving-average}}]
  (map-some
   tl
   (fn [ma1 ma2] (/ (- ma1 ma2) ma2))
   (fast-avg-indicator tl (or fast-sources sources) fast-opts)
   (slow-avg-indicator tl (or slow-sources sources) slow-opts)))

(defn percentage-price-oscillator
  "returns tuple of indicators [ppo-line signal histogram], ppo-line vals between -100 and 100.
   Similar to [[moving-average-convergence-divergence]], but returns percentage instead of absolute price changes.

   Reference:
   - https://www.investopedia.com/terms/p/ppo.asp

   Sources:
   - `src`

   Opts are those accepted by [[ppo-line]], plus:
   - `signal-avg-indicator` defaults to [[exponential-moving-average]], must accept a source on `:src` key
   - `signal-opts` passed to `signal-avg-indicator`"
  [tl
   {:keys [src] :as sources}
   {:keys [signal-avg-indicator signal-opts]
    :or   {signal-avg-indicator exponential-moving-average}
    :as opts}]
  (let [ppo    (percentage-price-oscillator-line tl sources opts)
        signal (signal-avg-indicator tl {:src ppo} signal-opts)
        hist   (map-some tl - ppo signal)]
    [ppo signal hist]))

(defn mean-deviation
  "deviation of prices from a moving average.
   
   Sources: 
   - `src`
   
   Opts:
   - `avg-indicator` defaults to [[simple-moving-average]], must accept a source under `:src` key
   - `period`
   - `avg-opts` overrides top level `opts` for `avg-indicator` if specified
   "
  [tl
   {:keys [src] :as sources}
   {:keys [period avg-indicator avg-opts] :or {avg-indicator simple-moving-average} :as opts}]
  (derived
   tl
   (merge sources {:ma (avg-indicator tl sources (or avg-opts opts))})
   (fn mean-deviation- [_ {:keys [src ma]} k]
     (bc/when-let [curr-ma (get ma k)
                   tail    (ts/tail src period {:endk k :vf val})]
       (/ (reduce (fn [acc curr] (+ acc (abs (- curr curr-ma)))) 0 tail)
          period)))))

(defn standard-deviation
  "standard deviation of prices during period
   
   Sources:
   - `src`

   Opts:
   - `period`"
  [tl {:keys [src] :as sources} {:keys [period]}]
  (derived
   tl
   sources
   (fn standard-deviation- [_ {:keys [src]} k]
     (when-let [tail (ts/tail src period {:endk k :vf val})]
       (umath/standard-deviation tail)))))

(defn parabolic-stop-and-reverse
  "Reference:
   - https://www.investopedia.com/trading/introduction-to-parabolic-sar/
   
   Reference impl:
   - https://github.com/ta4j/ta4j/blob/master/ta4j-core/src/main/java/org/ta4j/core/indicators/ParabolicSarIndicator.java
   
   Sources:
   - `close`
   - `high`
   - `low`

   Opts:
   - `start`
   - `increment`
   - `max-value`

   Values are maps of:
   - `ct` current-trend (true if uptrend, false if downtrend)
   - `sar` sar val
   - `af` acceleration factor
   - `ep` extreme point"
  [tl {:keys [close high low] :as sources} {:keys [start increment max-value]}]
  (derived
   tl
   sources
   (fn parabolic-stop-and-reverse*- [sar {:keys [close high low]} k]
     (let [[prev-k prev-c] (ts/shift close k -1)
           prev-sar        (get sar prev-k)
           curr-c          (get close k)
           curr-h          (get high k)
           curr-l          (get low k)]
       (cond
         (and prev-sar curr-h curr-l)
         (let [{:keys [ct ep af sar]} prev-sar]
           (if ct
              ;; uptrend
             (let [curr-ep (max ep curr-h)
                   curr-af (if (> curr-ep ep)
                             (min (+ af increment))
                             af)
                   new-sar (+ sar (* curr-af (- curr-ep sar)))
                   uptrend (<= new-sar curr-l)]
               (if (not uptrend)
                 {:ct false
                  :sar ep
                  :af start
                  :ep curr-l}

                 {:ct true
                  :sar new-sar
                  :ep curr-ep
                  :af curr-af}))

              ;; downtrend
             (let [curr-ep (min ep curr-l)
                   curr-af (if (< curr-ep ep)
                             (min max-value (+ af increment))
                             af)
                   new-sar (- sar (* curr-af (- sar curr-ep)))
                   downtrend (>= new-sar curr-h)]

               (if (not downtrend)
                 {:ct true
                  :sar ep
                  :af start
                  :ep curr-h}

                 {:ct false
                  :sar new-sar
                  :ep curr-ep
                  :af curr-af}))))

         (and (not prev-sar) prev-c curr-c curr-l curr-h)
         (let [ct (> curr-c prev-c)
               sar (if ct curr-l curr-h)]
           {:ct ct
            :sar sar
            :af start
            :ep sar})

         :else nil)))))

(defn stochastic-kline
  "non-smoothed k-line component of [[stochastic-oscillator]], between 0 and 1.
   
   Sources:
   - `close`
   - `high`
   - `low`

   Opts:
   - `period`"
  [tl {:keys [close high low] :as sources} {:keys [period]}]
  (derived
   tl
   sources
   (fn stochastic-kline- [_ {:keys [close high low]} k]
     (bc/when-let [tail-h  (ts/tail high period {:endk k :vf val})
                   tail-l  (ts/tail low period {:endk k :vf val})
                   curr-c  (get close k)
                   lowest  (reduce min tail-l)
                   highest (reduce max tail-h)]
       (/ (- curr-c lowest)
          (- highest lowest))))))

(defn stochastic-oscillator
  "returns tuple of nodes [k-line d-line] of stochastic oscillator, with vals between 0 and 1.
   
   Reference: 
   - https://www.tradingview.com/support/solutions/43000502332-stochastic-stoch/
   
   Sources:
   - `close`
   - `high`
   - `low`
   
   Opts:
   - `k-line-opts` opts passed to [[stochastic-kline]]
   - `avg-k-indicator` defaults to [[simple-moving-average]], must accept source as `:src`
   - `avg-k-opts` passed to `avg-k-indicator`
   - `avg-d-indicator` defaults to [[simple-moving-average]], must accept source as `:src`
   - `avg-d-opts` passed to `avg-d-indicator`"
  [tl
   {:keys [close high low] :as sources}
   {:keys [k-line-opts avg-k-indicator avg-k-opts avg-d-indicator avg-d-opts]
    :or  {avg-k-indicator simple-moving-average
          avg-d-indicator simple-moving-average}}]
  (let [k-line-base (stochastic-kline tl sources k-line-opts)
        k-line      (avg-k-indicator tl {:src k-line-base} avg-k-opts)
        d-line      (avg-d-indicator tl {:src k-line} avg-d-opts)]
    [k-line d-line]))

(defn relative-strength-g-l
  "returns tuple of nodes [gains losses], part of [[relative-strength-index]].
   gains: if (pos? x - (x-1)) then x else 0
   losses: if (neg? x - (x-1)) then -x else 0
   
   Sources:
   - `src`"
  [tl {:keys [src] :as sources}]
  [(map-some tl (fn [x] (if (pos? x) x 0)) (momentum tl sources))
   (map-some tl (fn [x] (if (neg? x) (abs x) 0)) (momentum tl sources))])

(defn relative-strength-index
  "Values between 0 and 1.
   
   Reference:
   - https://www.tradingview.com/support/solutions/43000502338-relative-strength-index-rsi/
   
   Sources 
   - `src`
   
   Opts:
   - `avg-indicator` used to average both [[relative-strength-g-l]]. Defaults to [[relative-moving-average]], 
     must accept source as `:src`
   - `avg-opts` if specified, overrides top level opts passed to `avg-indicator`"
  [tl
   {:keys [src] :as sources}
   {:keys [avg-indicator avg-opts] :or {avg-indicator relative-moving-average} :as opts}]
  (let [[g l] (relative-strength-g-l tl sources)]
    (map-some tl
              (fn [g l]
                (cond
                  (zero? l) 1
                  (zero? g) 0
                  :else (/ g (+ g l))))
              (avg-indicator tl {:src g} (or avg-opts opts))
              (avg-indicator tl {:src l} (or avg-opts opts)))))

(defn commodity-channel-index
  "Reference:
   - https://www.tradingtechnologies.com/xtrader-help/x-study/technical-indicator-definitions/commodity-channel-index-cci/
   
   Sources:
   - `src` can be any number series, although the original formula specifies typical price to be [[hlc3]].

   Opts:
   - `avg-indicator` defaults to [[simple-moving-average]], must accept src as `:src`
   - `avg-opts` if specified, overrides top level `opts` on `avg-indicator`
   - `mean-dev-opts` if specified, overrides top level `opts` on internal [[mean-deviation]]
   - `constant-factor` double. defaults to 0.015"
  [tl
   {:keys [src] :as sources}
   {:keys [avg-indicator avg-opts mean-dev-opts period constant-factor]
    :or {avg-indicator simple-moving-average
         constant-factor 0.015}
    :as opts}]
  (derived
   tl
   {:src src
    :ma (avg-indicator tl sources (or avg-opts opts))
    :mean-dev (mean-deviation tl sources (or mean-dev-opts opts))}
   (fn commodity-channel-index- [curr {:keys [mean-dev ma src]} k]
     (bc/when-let [price    (get src k)
                   ma       (get ma k)
                   mean-dev (get mean-dev k)
                   cci-val  (if-not (zero? mean-dev) 
                              (/ (- price ma) (* mean-dev constant-factor))
                              Double/NaN)]
       (if (Double/isNaN cci-val)
         (get curr (ts/shift src k -1 {:vf key}))
         cci-val)))))

(defn relative-volatility-u-s
  "Returns nodes tuple [u s] of [[relative-volatility-index]] u and s components. Similar to [[relative-strength-g-l]],
   but use standard-deviation of price-change instead of absolute price change.

   Sources:
   - `src`

   Opts are those of [[standard-deviation]]
   
   Reference:
   - https://www.fmlabs.com/reference/default.htm?url=RVI.htm (RVIOrig)"
  [tl {:keys [src] :as sources} {:keys [period] :as opts}]
  (let [std (standard-deviation tl sources opts)
        vch (momentum tl sources)
        u   (map-some tl (fn [vch std] (if (not (neg? vch)) std 0)) vch std)
        s   (map-some tl (fn [vch std] (if (neg? vch) std 0)) vch std)]
    [u s]))

(defn relative-volatility-index
  "Values from 0 to 1.
  
   Reference:
   - https://www.fmlabs.com/reference/default.htm?url=RVI.htm (RVIOrig)

   Sources:
   - `src`
   
   Opts:
   - `avg-indicator` defaults to [[exponential-moving-average]], must accept source as `:src`
   - `stdev-opts` if specified, overrides top level `opts` passed to [[relative-volatility-u-s]]
   - `avg-opts` if specified, overrides top level `opts` passed to `avg-indicator`"
  [tl
   {:keys [src] :as sources}
   {:keys [avg-indicator avg-opts stdev-opts period]
    :or {avg-indicator exponential-moving-average} :as opts}]
  (let [[u s]  (relative-volatility-u-s tl sources (or stdev-opts opts))]
    (map-some tl
              (fn [u s] (/ u (+ s u)))
              (avg-indicator tl {:src u} (or avg-opts opts))
              (avg-indicator tl {:src s} (or avg-opts opts)))))

(defn refined-relative-volatility-index
  "(rvi(high) + rvi(low) / 2), with values between 0 and 1.
   
   Reference:
   - https://www.fmlabs.com/reference/default.htm?url=RVI.htm (RVI)

   Sources:
   - `high`
   - `low`
   
   Opts are those accepted by [[relative-volatility-index]]"
  ([tl {:keys [high low] :as sources} {:keys [avg-indicator avg-opts stdev-opts period] :as opts}]
   (mean tl (relative-volatility-index tl {:src high} opts) (relative-volatility-index tl {:src low} opts))))

(defn base-range
  "high - low
   
   Sources:
   - `high`
   - `low`"
  [tl {:keys [high low]}]
  (map-some tl - high low))

(defn true-range
  "Reference:
   - https://www.investopedia.com/terms/a/atr.asp (TR)
   
   Sources:
   - `close`
   - `high`
   - `low`"
  [tl {:keys [close high low] :as sources}]
  (derived
   tl
   sources
   (fn true-range- [_ {:keys [close high low]} k]
     (bc/when-let [curr-h (get high k)
                   curr-l (get low k)
                   prev-c (ts/shift close k -1 {:vf val})
                   hl-dif (- curr-h curr-l)
                   hc     (abs (- curr-h prev-c))
                   lc     (abs (- curr-l prev-c))]
       (max hl-dif hc lc)))))

(defn average-base-range
  "moving average of [[base-range]]. 
   
   Sources:
   - `high`
   - `low`
   
   Opts: 
   - `avg-indicator` defaults to [[relative-moving-average]], must accept source as `:src`
   - `period` passed to `avg-indicator` as :period"
  [tl
   {:keys [high low] :as sources}
   {:keys [avg-indicator period]
    :or   {avg-indicator relative-moving-average}}]
  (avg-indicator tl {:src (base-range tl sources)} {:period period}))

(defn average-true-range
  "moving average of [[true-range]]. 
   
   Reference:
   - https://www.investopedia.com/terms/a/atr.asp (ATR)
   
   Sources:
   - `close`
   - `high`
   - `low`
   
   Opts:
   - `avg-indicator` defaults to [[relative-moving-average]], must accept source as `:src`
   - `period` passed to `avg-indicator` as :period"
  [tl
   {:keys [close high low] :as sources}
   {:keys [avg-indicator period]
    :or   {avg-indicator relative-moving-average}}]
  (avg-indicator tl {:src (true-range tl sources)} {:period period}))

(defn bollinger-bands
  "Returns tuple of nodes [upper middle lower] for bollinger bands.
   
   Reference:
   - https://www.investopedia.com/terms/b/bollingerbands.asp

   Sources:
   -  `src`
  
   Opts:
   - `avg-indicator` for middle band. Defaults to [[simple-moving-average]], must accept source as `:src`
   - `avg-opts` if specified, overrides top level `opts` for `avg-indicator`
   - `stdev-opts` if specified, overrides top level `opts` for [[standard-deviation]]
   "
  [tl
   {:keys [src] :as sources}
   {:keys [avg-indicator period avg-opts stdev-opts multiplier]
    :or {avg-indicator simple-moving-average}
    :as opts}]
  (let [middle (avg-indicator tl sources (or avg-opts opts))
        offset (standard-deviation tl sources (or stdev-opts opts))
        upper  (map-some tl (fn [m off] (+ m (* multiplier off))) middle offset)
        lower  (map-some tl (fn [m off] (- m (* multiplier off))) middle offset)]
    [upper middle lower]))

(defn keltner-channels
  "Returns tuple of nodes [upper middle lower]

   Reference:
   - https://www.tradingview.com/support/solutions/43000502266-keltner-channels-kc/
   
   Sources:
   - `close` (atr)
   - `high` (atr)
   - `low` (atr)
   - `src` (middle band)

   Opts:
   - `avg-indicator` for middle band. Defaults to [[exponential-moving-average]], will be passed top level sources.
   - `avg-opts` if specified, overrides top level `opts` for `avg-indicator`
   - `range-indicator` for use as range indicator. Defaults to [[average-true-range]]. Will be passed top level 
     `sources` unchanged.
   - `range-opts` if specified, overrides top level `opts` for range indicator.
   - `multiplier`"
  [tl
   {:keys [close high low src] :as sources}
   {:keys [avg-indicator avg-opts range-indicator range-opts multiplier]
    :or   {avg-indicator exponential-moving-average
           range-indicator average-true-range}
    :as opts}]
  (let [middle (avg-indicator tl sources (or avg-opts opts))
        rng    (range-indicator tl sources (or range-opts opts))
        upper  (map-some tl (fn [m rng] (+ m (* multiplier rng))) middle rng)
        lower  (map-some tl (fn [m rng] (- m (* multiplier rng))) middle rng)]
    [upper middle lower]))

(defn average-directional-index
  "Returns tuple of [adx di+ di-] nodes, where adx is between 0 and 1.
   
   Reference:
   - https://www.investopedia.com/ask/answers/112814/how-average-directional-index-adx-calculated-and-what-formula.asp
   
   Sources:
   - `close`
   - `high`
   - `low`
   
   Opts:
   - `range-indicator` range indicator to use, will be called with top level `sources`. Defaults to [[average-true-range]]
   - `range-opts` if specified, overrides top level `opts` passed to range indicator.
   - `line-avg-indicator` defaults to [[exponential-moving-average]]
   - `line-avg-opts` if specified, overrides top level opts passed to `line-avg-indicator`
   - `adx-avg-indicator` defaults to [[exponential-moving-average]]
   - `adx-avg-opts` if specified, overrides top level `opts` on `adx-avg-indicator`"
  [tl
   {:keys [close high low] :as sources}
   {:keys [adx-avg-indicator adx-avg-opts range-indicator range-opts line-avg-indicator line-avg-opts]
    :or   {adx-avg-indicator exponential-moving-average
           range-indicator average-true-range
           line-avg-indicator exponential-moving-average}
    :as opts}]
  (let [high-ch (momentum tl {:src high})
        low-ch  (momentum tl {:src low})
        rng     (range-indicator tl sources (or range-opts opts))

        base-di (fn [x1 x2]
                  (map-some tl
                            (fn [x1 x2 rng]
                              (/ (if (and (> x1 x2) (> x1 0)) x1 0)
                                 rng))
                            x1
                            x2
                            rng))

        +di (line-avg-indicator tl {:src (base-di high-ch low-ch)} (or line-avg-opts opts))
        -di (line-avg-indicator tl {:src (base-di low-ch high-ch)} (or line-avg-opts opts))

        adx (map-some tl
                      (fn [p n] (abs (/ (- p n) (+ p n))))
                      +di
                      -di)

        adx (adx-avg-indicator tl {:src adx} (or adx-avg-opts opts))]
    [adx +di -di]))

(defn on-balance-volume
  "Reference:
   - https://www.investopedia.com/terms/o/onbalancevolume.asp
   
   Sources:
   - `src`
   - `volume`"
  [tl {:keys [src volume] :as sources}]
  (derived
   tl
   sources
   (fn on-balance-volume- [obv {:keys [src volume]} k]
     (bc/when-let [[prev-k prev-c] (ts/shift src k -1)
                   :let [prev-obv (get obv prev-k)]
                   curr-c (get src k)
                   curr-v (get volume k)]
       (cond
         (not prev-obv)    0
         (> curr-c prev-c) (+ prev-obv curr-v)
         (< curr-c prev-c) (- prev-obv curr-v)
         (= curr-c prev-c) prev-obv)))))

(defn accumulation-distribution-line
  "Returns tuple of [adl money-flow-multiplier money-flow-volume] indicators.
   
   Reference:
   - https://www.tradingview.com/support/solutions/43000501770-accumulation-distribution-adl/
   
   Sources:
   - `close`
   - `high`
   - `low`
   - `volume`"
  [tl {:keys [close high low volume] :as sources}]
  (let [mfm (derived
             tl
             sources
             (fn [curr {:keys [close high low]} k]
               (bc/when-let [h (get high k)
                             c (get close k)
                             l (get low k)]
                 (if (= h l)
                   (get curr (ts/shift close k -1 {:vf key}))
                   (/ (- (- c l) (- h c)) (- h l))))))

        mfv (map-some tl * mfm volume)
        adl (derived
             tl
             {:mfv mfv}
             (fn [v {:keys [mfv]} k]
               (when-let [curr-mfv (get mfv k)]
                 (if-let [prev (get v (ts/shift mfv k -1 {:vf key}))]
                   (+ prev curr-mfv)
                   curr-mfv))))]
    [adl mfm mfv]))

(defn highest-val
  "Returns highest value of latest period items
   
   Sources:
   - `src`
   
   Opts:
   - `period`"
  [tl {:keys [src] :as sources} {:keys [period] :as opts}]
  (derived
   tl
   sources
   (fn [_ {:keys [src]} k]
     (bc/when-let [t (ts/tail src period {:endk k :vf val})]
       (second (ts/select t >=))))))

(defn lowest-val
  "Returns lowest value of latest period items
   
   Sources:
   - `src`
   
   Opts:
   - `period`"
  [tl {:keys [src] :as sources} {:keys [period] :as opts}]
  (derived
   tl
   sources
   (fn [_ {:keys [src]} k]
     (bc/when-let [t (ts/tail src period {:endk k :vf val})]
       (second (ts/select t <=))))))

(defn since-highest-val
  "Returns number of items since highest value on latest `period` items
   
   Sources:
   - `src`
   
   Opts:
   - `period`"
  [tl {:keys [src] :as sources} {:keys [period] :as opts}]
  (derived
   tl
   sources
   (fn [_ {:keys [src]} k]
     (bc/when-let [t (ts/tail src period {:endk k :vf val})
                   [sidx _] (ts/select t >=)]
       (- period (inc sidx))))))

(defn since-lowest-val
  "Returns number of items since lowest value on latest `period` items
   
   Sources:
   - `src`
   
   Opts:
   - `period`"
  [tl {:keys [src] :as sources} {:keys [period] :as opts}]
  (derived
   tl
   sources
   (fn [_ {:keys [src]} k]
     (bc/when-let [t (ts/tail src period {:endk k :vf val})
                   [sidx _] (ts/select t <=)]
       (- period (inc sidx))))))

(defn aroon-oscillator
  "Returns tuple of nodes [aroon-oscillator aroon-up aroon-down], vals between 0 and 1
   
   Reference:
   - https://www.investopedia.com/terms/a/aroonoscillator.asp
   
   Sources:
   - `high`
   - `low`
   
   Opts:
   - `period`"
  [tl {:keys [high low]} {:keys [period] :as opts}]
  (let [aup (map-some
             tl
             (fn [x] (/ (- period x) period))
             (since-highest-val tl {:src high} opts))

        adown (map-some
               tl
               (fn [x] (/ (- period x) period))
               (since-lowest-val tl {:src low} opts))

        aroon (map-some tl - aup adown)]
    [aroon aup adown]))

(defn chandelier-exits
  "Returns tuple of chandelier exits nodes [long short]
   
   Reference:
   - https://school.stockcharts.com/doku.php?id=technical_indicators:chandelier_exit
   
   Sources:
   - `close`
   - `high`
   - `low`

   Opts:
   - `range-indicator` Defaults to [[average-true-range]], will be passed top level `sources`.
   - `range-opts` if specified, overrides top level `opts` passed to `range-indicator`
   - `highest-opts` if specified, overrides top level `opts` passed to [[highest-val]]
   - `lowest-opts` if specified, overrides top level `opts` passed to [[lowest-val]]
   - `multiplier`"
  [tl
   {:keys [close high low] :as sources}
   {:keys [range-indicator range-opts highest-opts lowest-opts multiplier]
    :or {range-indicator average-true-range}
    :as opts}]
  (let [highest (highest-val tl {:src high} (or highest-opts opts))
        lowest  (lowest-val tl {:src low} (or lowest-opts opts))
        rng     (range-indicator tl sources (or range-opts opts))
        long    (map-some tl (fn [h atr] (- h (* multiplier atr))) highest rng)
        short   (map-some tl (fn [l atr] (+ l (* multiplier atr))) lowest rng)]
    [long short]))

(defn donchian-channels
  "Returns tuple of [mid high low] nodes
   
   Sources:
   - `high`
   - `low`
   
   Opts:
   - `highest-opts` if specified, overrides top level `opts` on [[highest-val]] 
   - `lowest-opts` if specified, overrides top level `opts` on [[lowest-val]]
   
   Reference:
   - https://www.investopedia.com/terms/d/donchianchannels.asp"
  [tl {:keys [high low]} {:keys [highest-opts lowest-opts] :as opts}]
  (let [highest (highest-val tl {:src high} (or highest-opts opts))
        lowest (lowest-val tl {:src low} (or lowest-opts opts))
        mid (mean tl highest lowest)]
    [mid highest lowest]))

(defn supertrend
  "Reference impl: 
   - https://github.com/twopirllc/pandas-ta/blob/main/pandas_ta/overlap/supertrend.py
   
   Sources:
   - `close`
   - `high`
   - `low`
   
   Opts:
   - `range-indicator` defaults to [[average-true-range]], will be passed top level `sources`
   - `range-opts` if specified, overrides top level `opts` for `range-indicator`
   - `multiplier`

   Values are map of:
   - `trend` e/o #{:long :short}
   - `value` number
   - `_upper`
   - `_lower`"
  [tl
   {:keys [close high low] :as sources}
   {:keys [multiplier range-indicator range-opts] :or {range-indicator average-true-range} :as opts}]
  (let [rng  (range-indicator tl sources (or range-opts opts))
        hl2' (hl2 tl sources)
        basic-upper (map-some tl (fn [hlm rng] (+ hlm (* multiplier rng))) hl2' rng)
        basic-lower (map-some tl (fn [hlm rng] (- hlm (* multiplier rng))) hl2' rng)]
    (derived
     tl
     {:close close :upper basic-upper :lower basic-lower}
     (fn [st {:keys [close upper lower]} k]
       (bc/when-let [prev-k (ts/shift close k -1 {:vf key})
                     :let [prev-st (get st prev-k)]
                     curr-c  (get close k)
                     curr-up (get upper k)
                     curr-lo (get lower k)]
         (cond
           (and prev-st (> curr-c (:_upper prev-st)))
           {:trend :long :value curr-lo :_upper curr-up :_lower curr-lo}

           (and prev-st (< curr-c (:_lower prev-st)))
           {:trend :short :value curr-up :_upper curr-up :_lower curr-lo}

           :else
           (let [trend (or (:trend prev-st) :long)
                 lower (if (and (= :long trend) prev-st (< curr-lo (:_lower prev-st)))
                         (:_lower prev-st)
                         curr-lo)
                 upper (if (and (= :short trend) prev-st (> curr-up (:_upper prev-st)))
                         (:_upper prev-st)
                         curr-up)]
             {:trend trend :_upper upper :_lower lower :value (case trend :long lower :short upper)})))))))

(defn kaufman-efficiency-ratio
  "Reference:
   - https://school.stockcharts.com/doku.php?id=technical_indicators:kaufman_s_adaptive_moving_average (ER)
   
   Sources:
   - `src`
   
   Opts:
   - `period`"
  [tl {:keys [src] :as sources} {:keys [period] :as opts}]
  (derived
   tl
   {:mom (momentum tl sources)}
   (fn kaufman-efficiency-ratio- [_ {:keys [mom]} k]
     (bc/when-let [ch      (some-> (get mom k) abs)
                   ch-tail (ts/tail mom period {:endk k :vf val})]
       (/ ch (reduce (fn [a n] (+ a (abs n))) 0 ch-tail))))))

(defn kaufman-adaptive-moving-average
  "Reference:
   - https://school.stockcharts.com/doku.php?id=technical_indicators:kaufman_s_adaptive_moving_average (KAMA)
   
   Sources:
   - `src`
   
   Opts:
   - `er-period` period to use on [[kaufman-efficiency-ratio]]
   - `slow-period`
   - `fast-period`"
  [tl {:keys [src] :as sources} {:keys [er-period slow-period fast-period]}]
  (let [ssc (/ 2 (inc slow-period))
        fsc (/ 2 (inc fast-period))
        sc  (map-some
             tl
             (fn [er]
               (math-nt/expt
                (+ (* er (- fsc ssc)) ssc)
                2))
             (kaufman-efficiency-ratio tl sources {:period er-period}))]
    (derived
     tl
     {:src src :sc sc}
     (fn [kama {:keys [src sc]} k]
       (bc/when-let [curr-p    (get src k)
                     curr-sc   (get sc k)
                     :let [prev-kama (get kama (ts/shift src k -1 {:vf key}))]]
         (if prev-kama
           (+ prev-kama (* curr-sc (- curr-p prev-kama)))
           (ts/moving-average src k er-period)))))))
