(ns bortexz.tacos.timeseries
  (:require [clojure.data.avl :as avl]
            [bortexz.utils.math :as umath]
            [better-cond.core :as bc]))

(defn- rank-of
  "Like avl/rank-of but returns nil if item not found"
  [coll k]
  (let [r (avl/rank-of coll k)]
    (when-not (neg? r) r)))

(defprotocol -IntoTimeseries
  "Protocol that constructs a timeseries from a given object"
  (-into-timeseries [_]))

(extend-protocol -IntoTimeseries
  clojure.lang.IPersistentCollection
  (-into-timeseries [x] (into (avl/sorted-map) x))

  clojure.data.avl.AVLMap
  (-into-timeseries [x] x))

(deftype View [coll
               start-idx
               end-idx
               size
               vf]

  -IntoTimeseries
  (-into-timeseries [_]
    (avl/subrange coll
                  >= (key (nth coll start-idx))
                  <= (key (nth coll end-idx))))

  clojure.lang.Counted
  (count [_] size)

  clojure.lang.Seqable
  (seq [_]
    ((fn view-seq [idx]
       (when (<= idx end-idx)
         (cons (vf (nth coll idx))
               (lazy-seq (view-seq (inc idx))))))
     start-idx))

  clojure.lang.Indexed
  (nth [_ idx]
    (let [i (+ start-idx idx)]
      (if (< 0 i size)
        (vf (nth coll i))
        (throw (IndexOutOfBoundsException.)))))

  (nth [_ idx not-found]
    (let [i (+ start-idx idx)]
      (if (< 0 i size)
        (vf (nth coll i))
        not-found)))

  clojure.lang.IReduce
  (reduce [this rf]
    (case (.count this)
      0 (rf)
      1 (nth this 0)
      (loop [ret (rf (val (nth coll start-idx)) (val (nth coll (inc start-idx))))
             s (inc (inc start-idx))]
        (if-not (<= s end-idx)
          ret
          (let [item (nth coll s nil)
                rr (rf ret (vf item))]
            (if (reduced? rr)
              @rr
              (recur rr (inc s))))))))

  clojure.lang.IReduceInit
  (reduce [_ rf init]
    (loop [ret init
           s start-idx]
      (if-not (<= s end-idx)
        ret
        (let [item (nth coll s nil)
              rr (rf ret (vf item))]
          (if (reduced? rr)
            @rr
            (recur rr (inc s)))))))

  clojure.core.protocols.IKVReduce
  (kv-reduce [_ f init]
    (loop [ret init
           s start-idx
           idx 0]
      (if-not (<= s end-idx)
        ret
        (let [item (nth coll s)
              rr (f ret idx (vf item))]
          (if (reduced? rr)
            @rr
            (recur rr (inc s) (inc idx))))))))

(defn create
  "Creates a new timeseries. Timeseries are currently implemented as data.avl sorted maps with timestamp as keys. Note 
   that anything that can be compared can be used as a timestamp (epoch millis integer, java.time.Instant, etc...)
   
   0-arity creates an empty timeseries. 
   1-arity creates a timeseries from object implementing IntoTimeseries. Objects implementing IntoTimeseries:
   - Clojure collections, each item being a tuple [timestamp value].
   - Views created with [[view]] or [[tail]].
   - AVL Sorted map, returning the map unchanged."
  ([] (avl/sorted-map))
  ([obj] (-into-timeseries obj)))

(defn latest
  "Returns the latest item of timeseries `coll` as tuple [timestamp value], or nil if `coll` is empty."
  [coll]
  (nth coll (dec (count coll)) nil))

(defn earliest
  "Returns the earliest item of timeseries `coll` as tuple [timestamp value], or nil if `coll` is empty."
  [coll]
  (nth coll 0 nil))

(defn nearest
  "Returns the nearest item [timestamp value] to timestamp `k` on timeseries `coll` using `test`, that can be
   either >, >=, <, <=."
  [coll test k]
  (avl/nearest coll test k))

(defn shift
  "Returns item at distance `n` from timestamp `k`. If timestamp `k` does not exist, or new position does
   not exist, returns nil. If item exists, returns tuple [timestamp value].

   Optionally accepts opts:
   - `vf` fn to get on map-entry [timestamp val] if existing, defaults to `identity`.

   e.g. (shift coll k -1) takes the previous item to timestamp `k`
   e.g. (shift coll k 1) takes the next item to timestamp `k`."
  ([coll k n] (shift coll k n {}))
  ([coll k n {:keys [vf] :or {vf identity}}]
   (bc/when-let [k-rank (rank-of coll k)]
     (some-> (nth coll (+ k-rank n) nil) vf))))

(defn view
  "Returns a view object for traversing timeseries `coll` with given `opts`. 
   
   A view is reducedible, seqable, countable, indexed and can be converted into a new timeseries.

   Values of this view will be retrieved from each mapentry [timestamp value] using `vf` (defaults to `val`).

   A new timeseries can be created from this view calling `(create <view>)`
   
   `nth` can be used on the view to get the `nth` value inside the view.

   When reduced using reduce-kv, the `k` will be the index inside the view (starting at 0, ending at `(dec (count view)))` 
   as if reducing-kv a vector of items.

   If `startk` or `endk` are specified but do not exist on coll, returns nil.
   
   opts:
   - `startk` First timestamp of the view. Defaults to earliest timestamp of timeseries.
   - `endk` Last timestamp of the view. Defaults to latest timestamp of timeseries.
   - `vf` fn to apply to tuple [timestamp value] to get a value. Defaults to `identity`."
  ([coll] (view coll {}))
  ([coll {:keys [startk endk vf] :or {vf identity}}]
   (bc/when-let [start-idx (if startk
                             (rank-of coll startk)
                             0)
                 end-idx (if endk
                           (rank-of coll endk)
                           (dec (count coll)))]
     (View. coll start-idx end-idx (inc (- end-idx start-idx)) vf))))

(defn tail
  "Returns a [[view]] of the tail of timeseries `coll`, up to `n` items.
   
   opts:
   - `endk` last timestamp of the tail, defaults to the latest of the timeseries if not specified. If endk is specified,
     but does not exist on coll, returns nil.
   - `vf` fn that accepts an item [timestamp value] and returns a val (e.g `key`, `val`, ...). Defaults to `val`.
   - `full?` If false, returns tails that contain < n items, otherwise returns nil instead of an incomplete tail view.
     Defaults to true."
  ([coll n] (tail coll n {}))
  ([coll n {:keys [endk vf full?] :or {vf val full? true}}]
   (bc/when-let [end (if endk
                       (rank-of coll endk)
                       (dec (count coll)))
                 n-1 (dec n)
                 _?  (nat-int? end)
                 _?  (or (not full?)
                         (and full? (>= end n-1)))

                 start (max 0 (- end n-1))]
     (View. coll start end (inc (- end start)) vf))))

(defn select
  "Selects a value from `view` by testing each value against the previously selected value with `testf`, a 2-arity fn that
   accepts new value and previosly selected value. If `testf` returns true, then the new value becomes the selected value.
   Iteration starts with first value as previosly selected, and second value as next value. 
   Returns [view-idx val] where `view-idx` is the index in view of the selected value, and `val` is selected value.
   
   E.g 
   - latest [idx val] of previous 10 vals from endk `(select (tail coll 10 {:endk endk :vf val}) >=)`"
  [view testf]
  (reduce-kv (fn [[sidx sv :as sel] idx v]
               (if (or (nil? sel) (testf v sv))
                 [idx v]
                 sel))
             nil
             view))

(defn keep-latest
  "Keeps only `n` latest values of timeseries `coll`. Same as `(create (tail coll n))`, but faster when number of items
   to be removed is small (currently < 5), as it dissoc's the earliest items instead of creating a new timeseries."
  [coll n]
  (let [size (count coll)]
    (if (> size n)
      (let [diff (- size n)]
        (if (< diff 5)
          (reduce dissoc coll (map #(key (nth coll %)) (range diff)))
          (second (avl/split-at diff coll))))
      coll)))

(defn moving-average
  "Calculates the moving average of given `coll` at timestamp `k` for `period` items, or nil if there aren't enough values."
  [coll k period]
  (when-let [t (tail coll period {:endk k :vf val})]
    (umath/mean t)))

(defprotocol Timeline
  "Protocol enabling incremental timeseries.
   
   A timeline is responsible for keeping the timeseries updated with new timestamps, as well as prune earliest items 
   when are no longer needed."
  (-apply-timeline [this value get-ts]
    "Given:
     - `timeseries` current timeseries value
     - `get-ts` 2-arity fn accepting accumulated `timeseries` and `timestamp`, returns the value of this timeseries at
       `timestamp`.

     Returns updated timeseries value."))

(defrecord DeltaTimeline [new-tss max-size]
  Timeline
  (-apply-timeline [_ value get-ts]
    (let [v (reduce (fn [acc k]
                      (if-let [v (get-ts acc k)]
                        (assoc acc k v)
                        acc))
                    (or value (create))
                    new-tss)]
      (keep-latest v max-size))))

(defn delta-timeline
  "Returns implementation of [[Timeline]] that updates given `new-timestamps` (ordered collection of new timestamps) on 
   a timeseries, and keeps a maximum of `max-size` items."
  [new-timestamps max-size]
  (->DeltaTimeline new-timestamps max-size))

