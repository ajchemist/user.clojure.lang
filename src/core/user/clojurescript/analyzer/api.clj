(ns user.clojurescript.analyzer.api
  (:require
   [cljs.analyzer.api :as a.api]
   ))


(defn macro?
  [ns-qualified-sym]
  (let [ns  (symbol (namespace ns-qualified-sym))
        sym (symbol (name      ns-qualified-sym))]
    (-> (a.api/ns-interns ns)
      (get sym (Exception. (str sym " is not found in " ns)))
      :macro)))


(defmacro import-var
  [[_quote sym] [_quote var]]
  (try
    (when-not (macro? var)
      `(def ~(symbol sym) ~var))
    (catch Throwable e nil)))


(defmacro import-namespace
  [[_quote ns]]
  `(do
     ~@(->> (a.api/ns-interns ns) ; maps
         (remove (fn [e] (:macro (val e))))
         (remove (fn [e] (:private (val e))))
         (map (fn [[k# _]] `(def ~(symbol k#) ~(symbol (name ns) (name k#))))))))


(defmacro debug-ns-interns
  [[_quote ns]]
  (println (a.api/ns-interns ns)))


(comment
  (import-var 'ifl #'cljs.core/abc)

  cljs.env/*compiler*
  (a.api/find-ns)

  (cljs.analyzer/requires-analysis?)

  (cljs.closure/cljs-dependencies )
  (cljs.closure/set-options {} )
  (cljs.closure/cljs-dependencies
   {:optimizations :none
    :output-to  "target-cljs/out.js"
    :output-dir "target-cljs/out"
    :asset-path "out"
    :parallel-build true}
   ["goog.string" "cljs.core"])
  )
