(ns user.clojure.core.alpha
  (:refer-clojure :exclude [find-ns resolve time])
  (:require
   [user.clojure.core.patch.alpha]
   [user.clojure.import.alpha :as import]
   )
  (:import
   java.util.Date
   java.text.SimpleDateFormat
   ))


(import/import-namespace 'user.clojure.core.patch.alpha)


;;


(defn nano-elapsed
  "from is an nano second"
  [from]
  (double (- (System/nanoTime) from)))


(defn ms-elapsed
  "from is an nano second"
  [from]
  (/ (nano-elapsed from) 1e6))


(defmacro with-nano-start
  "$start is an anaphor of nano time start"
  [& body]
  `(let [~'$start (System/nanoTime)]
     ~@body))


;; * log


(def timbre-enabled?
  (try
    (require 'taoensso.timbre)
    true
    (catch Throwable _ false)))


(def ^SimpleDateFormat ^:dynamic *date-format* (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss.SSS"))


(defmacro log-info
  [& args]
  (if timbre-enabled?
    `(taoensso.timbre/info ~@args)
    `(println
      (. *date-format* format (Date.))
      (.getName (Thread/currentThread))
      [(str *ns*)]
      ~@args)))


(defn- -nano-time
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  {:added "1.0"}
  [xs]
  (let [[key] xs
        key?  (or (string? key) (isa? (class key) clojure.lang.Named))
        pre   (str (when key? (str key " ")) "\"Elapsed time:")
        body  (if key? (rest xs) xs)]
    `(let [start# (. System (nanoTime))
           ret#   (do ~@body)]
       (log-info ~pre (/ (double (- (. System (nanoTime)) start#)) 1e6) "msecs\"")
       ret#)))


(defmacro time
  "Return is the same as the return of `(do ~@xs)"
  [& xs]
  (-nano-time xs))


;; * sort


(defn sort-by-order
  {:style/indent [:defn]}
  [keyfn order coll]
  (let [order-table (into {} (map-indexed (fn [i e] [e i])) order)]
    (sort-by #(get order-table (keyfn %)) coll)))
