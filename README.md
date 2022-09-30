# tacos
[![Clojars Project](https://img.shields.io/clojars/v/io.github.bortexz/tacos.svg)](https://clojars.org/io.github.bortexz/tacos)[![Cljdoc badge](https://cljdoc.org/badge/io.github.bortexz/tacos)](https://cljdoc.org/d/io.github.bortexz/tacos)

Collection of timeseries technical analysis indicators as [graphcom](https://github.com/bortexz/graphcom) nodes.

## Install

### Clojure CLI/deps.edn
```clojure
io.github.bortexz/graphcom {:mvn/version "0.0.2"}
```

### Leiningen/Boot
```clojure
[io.github.bortexz/graphcom "0.0.2"]
```

## Description

Tacos provides timeseries technical analysis indicators that are easily composable and are calculated in an incremental way (i.e you specify only new data points, and indicators keep their own accumulated timeseries up to a maximum number of timestamps). As the computation engine, tacos uses [graphcom](https://github.com/bortexz/graphcom), and indicators return graphcom nodes that are calculated when a graph is processed.

The value emitted by the indicator nodes is an [avl/sorted-map](https://github.com/clojure/data.avl) (also refered throughout this library as a timeseries), with ascending order. [timeseries](./src/bortexz/tacos/timeseries.clj) namespace contains functions to create new timeseries, as well as some utilities to navigate them. It also contains a protocol **Timeline**, whose purpose is to enable indicators to compute only new data points received. The default implementation **delta-timeline** is quite simple, it will compute new timestamps present on the input node (the root node that contains only new data points), store the result into each indicator's timeseries, and will ensure that only a maximum number of data points is kept. All indicators need a Timeline node as first parameter. An entrypoint that contains both a delta-timeline node and a source node that accumulates input values into a timeseries can be created using `bortexz.tacos/delta-source`.

[tacos](./src/bortexz/tacos.clj) namespace contains functions to create graphcom nodes that compute timeseries indicators. Most computations use the clojure numeric abstractions, so it would be easy to bring this library to run in ClojureScript, as well as allowing both Double and BigDecimal to be used. For now, Double should be considered the supported type for computations.

You can create your own derived indicators by using the `derived` function. Indicators can be composed within each other to create other indicators. Frequently, a single indicator will be a combination of other indicators. Because graphcom recursively adds dependency nodes, just introducing the node returned by an indicator into a graph, all dependencies are also added into the graph. As an example of this, see the source code for a triple exponential moving average:

Formula:
```
EMA1 = EMA of price
EMA2 = EMA of EMA1
EMA3 = EMA of EMA2

TEMA = (3 x EMA1) - (3 x EMA2) + (EMA3)
```

Code:
```clojure
(let [ema1 (exponential-moving-average tl sources opts)
      ema2 (exponential-moving-average tl {:src ema1} opts)
      ema3 (exponential-moving-average tl {:src ema2} opts)]
  (map-some tl
            (fn [ema1 ema2 ema3]
              (+ (* 3 ema1)
                 (- (* 3 ema2))
                 ema3))
              ema1
              ema2
              ema3))
```

See the [docs](https://cljdoc.org/d/io.github.bortexz/tacos) for the complete list of indicators currently available.

Jump to the [quick example](./examples/quick_example.clj) to see this library in action.

## Status and contributions

The library works in it's current status, and offers a minimum base for adding/building custom indicators that share a common timeline. There are still some areas of improvement I am considering. Any change that involves structural changes will bump the MAJOR version. Changes in any indicator formula or small breaking changes will update the MINOR version. PATCH versions are strictly non-breaking (adding new indicators).

Further exploration:
- Explore other timeline implementations, see if the protocol holds up (e.g time offseted timelines for ichimoku components)
- In terms of performance, there might be better suited data structures for a timeseries than a sorted map. Specifically, timeseries have clear access/insert/removal patterns (access tends towards the tail on computations, removal on the head, insertions on the tail, mostly latest value or new val after latest). Self-balancing binary search tree's like AVL are great sorted maps (this library relies on `nth` and `rank-of` of avl for faster tail reductions and indexed access) but there might be other data structures specifically crafted for these access/insert/removal patterns that might speed up this library. Changes in this area will be considered structural and will bump the MAJOR version as well as include migration notes.
- Visualization utils for quick exploration on REPL + portal/VEGA
- Testing: Accuracy testing and comparison to other implementations like ta4j or pantas-ta would be desirable. Currently, I test manually by checking TradingView.
- Better specification/testing for what numeric types are supported.
- ClojureScript support. Most indicators use clojure's numeric abstractions, so this already takes us quite close.
- More indicators are always welcome, as well as fixes to the current ones.

Feel free to share your thoughts on any of the above. Issues and PR's welcome :)

## Credits

Both [ta4j](https://github.com/ta4j/ta4j) and [pandas-ta](https://github.com/twopirllc/pandas-ta) have been an invaluable resource of inspiration and implementation details for this library.

## License

Copyright © 2022 Alberto Fernandez

Distributed under the Eclipse Public License version 1.0.
