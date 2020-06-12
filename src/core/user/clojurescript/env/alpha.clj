(ns user.clojurescript.env.alpha
  (:require
   [cljs.env]
   ))


(defn read-cljs-compiler-options
  ([]
   (read-cljs-compiler-options nil))
  ([ks]
   (when-let [compiler (resolve 'cljs.env/*compiler*)]
     (get-in (get compiler :options) ks))))


(defmacro CLJS_DEBUG
  []
  (when-let [m (read-cljs-compiler-options [:closure-defines])]
    (let [b (get m 'goog.DEBUG)]
      (if (some? b)
        b
        (get m "goog.DEBUG" true)))))


(defn- nodejs-target?
  []
  (identical? (get-in @cljs.env/*compiler* [:options :target]) :nodejs))


(defmacro if-nodejs-target
  [then else]
  (if (nodejs-target?)
    then
    else))


(defmacro when-nodejs-target
  [& body]
  (when (nodejs-target?)
    `(do ~@body)))


(defmacro when-not-nodejs-target
  [& body]
  (when-not (nodejs-target?)
    `(do ~@body)))
