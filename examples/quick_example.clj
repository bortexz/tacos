(ns quick-example
  (:require [bortexz.graphcom :as g]
            [bortexz.tacos :as ta]
            [bortexz.tacos.timeseries :as ts]))
  
  (let [new-candles (g/input-node)
        
        ;; `tl` is called a `timeline`, and controls new keys to be updated for indicators. In this case, delta-source
        ;; creates both a timeline node and a timeseries node from an input. The timeline will ensure that only incoming
        ;; new timestamps are calculated for all indicators using it, while the `src` will accumulate the new values
        ;; from input, up to max-size timestamps.
        [tl src] (ta/delta-source {:input new-candles} {:max-size 10})
        
        ;; `spread-map` takes a timeseries node of maps and returns map of nodes with each key extracted into its own 
        ;; timeseries node
        {:keys [high low close]} (ta/spread-map tl src {:ks [:close :high :low]
                                                        :transform double})

        ;; sma indicator
        sma (ta/simple-moving-average tl {:src close} {:period 3})

        ;; bb returns 3 nodes, each for each band, they can be combined together into a map with `sources-map`, which
        ;; does the contrary to `spread-map`, takes map of indicators, and returns node of timeseries with values as a
        ;; map containing all sources values
        [upper-bb mid-bb low-bb]  (ta/bollinger-bands tl {:src close} {:period 3 :multiplier 2})
        bb  (ta/sources-map tl {:upper upper-bb :middle mid-bb :lower low-bb})

        ;; atr indicator
        atr (ta/average-true-range tl {:high high :low low :close close} {:period 3})

        ;; create graphcom graph and context
        g   (g/graph {:input new-candles
                      :candles src
                      :sma sma
                      :bb bb
                      :atr atr})
        ctx (g/context g)

        ;; sample candles batches. Timeseries are currently avl/sorted-maps.
        candles1 (ts/create
                  [[1 {:open 5 :close 7 :high 9 :low 4}]
                   [2 {:open 7 :close 6 :high 8 :low 5}]
                   [3 {:open 6 :close 8 :high 11 :low 3}]])
        candles2 (ts/create
                  [[4 {:open 8 :close 10 :high 12 :low 7}]
                   [5 {:open 10 :close 9 :high 10 :low 8}]
                   [6 {:open 9 :close 13 :high 14 :low 9}]
                   [7 {:open 13 :close 12 :high 15 :low 11}]])
        candles3 (ts/create
                  [[7 {:open 12 :close 10 :high 13 :low 9}]
                   [8 {:open 10 :close 8 :high 11 :low 8}]
                   [9 {:open 8 :close 14 :high 15 :low 7}]])]

    ;; compute the graph inserting each candles, get final values
    (-> ctx
        (g/process {:input candles1})
        (g/process {:input candles2})
        (g/process {:input candles3})
        (g/values)
        (select-keys [:candles :sma :bb :atr])))

; -> result
{:candles
 {1 {:open 5, :close 7, :high 9, :low 4},
  2 {:open 7, :close 6, :high 8, :low 5},
  3 {:open 6, :close 8, :high 11, :low 3},
  4 {:open 8, :close 10, :high 12, :low 7},
  5 {:open 10, :close 9, :high 10, :low 8},
  6 {:open 9, :close 13, :high 14, :low 9},
  7 {:open 12, :close 10, :high 13, :low 9},
  8 {:open 10, :close 8, :high 11, :low 8},
  9 {:open 8, :close 14, :high 15, :low 7}}
 ;; Nil values are removed, many indicators need a minimum number of previous values on their source to compute
 ;; a value.
 :sma
 {3 7.0,
  4 8.0,
  5 9.0,
  6 10.666666666666666,
  7 10.666666666666666,
  8 10.333333333333334,
  9 10.666666666666666},
 :bb
 {3 {:upper 8.632993161855453, :middle 7.0, :lower 5.3670068381445475},
  4 {:upper 11.265986323710905, :middle 8.0, :lower 4.734013676289096},
  5 {:upper 10.632993161855453, :middle 9.0, :lower 7.3670068381445475},
  6 {:upper 14.066013009061855, :middle 10.666666666666666, :lower 7.267320324271476},
  7 {:upper 14.066013009061855, :middle 10.666666666666666, :lower 7.267320324271476},
  8 {:upper 14.442942668645985, :middle 10.333333333333334, :lower 6.223723998020683},
  9 {:upper 15.655543182365253, :middle 10.666666666666666, :lower 5.677790150968078}},
 :atr
 {5 5.333333333333333,
  6 5.222222222222222,
  7 4.814814814814815,
  8 4.209876543209877,
  9 5.473251028806585}}