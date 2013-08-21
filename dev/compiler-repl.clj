(require '[cljs.closure :as cljsc])
(defn compile-project []
  (cljsc/build
   "cljs"
   {:optimizations :simple :output-to "pub/code/index.js" :output-dir "pub/code" :pretty-print true}))