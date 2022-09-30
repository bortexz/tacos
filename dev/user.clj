(ns user
  (:require [hato.client :as http]
            [jsonista.core :as json]))

(defn bitmex-candles
  []
  (->
   (http/request
    {:method :get
     :url "https://www.bitmex.com/api/v1/trade/bucketed"
     :query-params {:binSize "1d"
                    :partial false
                    :symbol "XBTUSD"
                    :columns "open,close,high,low,volume"
                    :count 200
                    :reverse true}})
   :body
   (json/read-value (json/object-mapper {:decode-key-fn true}))
   (->> (reverse)
        (vec))))

(defn write-test-candles
  [candles]
  (spit "resources/test_candles.edn" candles))
