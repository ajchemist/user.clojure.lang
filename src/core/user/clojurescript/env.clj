(ns user.clojurescript.env
  (:require
   [cljs.env]
   ))


(defn read-cljs-compiler-options
  ([]
   (read-cljs-compiler-options nil))
  ([ks]
   (when-let [compiler (resolve 'cljs.env/*compiler*)]
     (get-in compiler (into [:options] ks)))))


(defmacro CLJS_DEBUG
  []
  (when-let [m (read-cljs-compiler-options [:closure-defines])]
    (let [b (get m 'goog.DEBUG)]
      (if (some? b)
        b
        (get m "goog.DEBUG" true)))))
