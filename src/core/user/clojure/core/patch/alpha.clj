(ns user.clojure.core.patch.alpha
  (:refer-clojure :exclude [find-ns resolve import])
  (:require
   [user.clojure.macro :refer [suppress]]
   )
  (:import
   java.util.concurrent.ConcurrentHashMap
   ))


;; * namespace


;; https://github.com/zcaudate/hara/blob/master/src/hara/namespace/resolve.clj


(defn nsym
  [x]
  (let [nsp  (namespace x)
        nsym (or nsp x)]
    (symbol nsym)))


(defn resolve-ns
  "resolves the namespace or else returns nil if it does not exist

   (resolve-ns 'clojure.core) => 'clojure.core

   (resolve-ns 'clojure.core/some) => 'clojure.core

   (resolve-ns 'clojure.hello) => nil"
  [x]
  (when-let [nsym (nsym x)]
    (suppress (do (require nsym) nsym))))


(defn resolve
  [x]
  (resolve-ns x)
  (clojure.core/resolve x))


(defn assert-ns
  [x]
  (when-let [nsym (nsym x)]
    (suppress (do (require nsym) nsym)
      #(println
         (format "resolve-ns: %s, %s" nsym (. ^Throwable % getMessage))))))


(defn find-ns
  [x]
  (resolve-ns x)
  (clojure.core/find-ns x))


(comment
  *e
  (resolve-ns 'clojure.core)
  (assert-ns 'clojure.hello)

  ;; (defmacro  [name sym] `[~name ~sym])
  ;; (intern )
  )


;; * fn


(defn func-resolve [func]
  (let [var (cond
              (symbol? func) (resolve func)
              (var? func) func
              :else func)]
    (when (ifn? var)
      var)))


(defn funcall
  ([func]
   (when-let [func (func-resolve func)]
     (func)))
  ([func & args]
   (when-let [func (func-resolve func)]
     (apply func args))))


;; * nil


(definline re-nil [x]
  `(let [x# ~x]
     (if (identical? ::nil x#) nil x#)))


(definline de-nil [x]
  `(let [x# ~x]
     (if (nil? x#) ::nil x#)))


;; * better memoize


(defmacro memoize-form [m f & args]
  `(let [k# (vector ~@args)]
     (let [v# (.get ~m k#)]
       (if-not (nil? v#)
         (re-nil v#)
         (let [v# (de-nil (~f ~@args))]
           (re-nil (or (.putIfAbsent ~m k# v#) v#)))))))


(defn fast-memoize
  "A version of `memoize` which has equivalent behavior, but is faster."
  [f]
  (let [m (ConcurrentHashMap.)]
    (fn
      ([]
       (memoize-form m f))
      ([x]
       (memoize-form m f x))
      ([x y]
       (memoize-form m f x y))
      ([x y z]
       (memoize-form m f x y z))
      ([x y z w]
       (memoize-form m f x y z w))
      ([x y z w u]
       (memoize-form m f x y z w u))
      ([x y z w u v]
       (memoize-form m f x y z w u v))
      ([x y z w u v & rest]
       (let [k (list* x y z w u v rest)
             v (.get ^ConcurrentHashMap m k)]
         (if-not (nil? v)
           (re-nil v)
           (let [v (de-nil (apply f k))]
             (or (.putIfAbsent m k v) v))))))))


;; * deep merge


(defn deep-merge
  [& maps]
  (apply
   merge-with
   (fn [x y]
     (cond
       (map? y) (deep-merge x y)
       :else y))
   maps))


;; * case


(defn- token-preprocess
  ([x]
   (token-preprocess eval x))
  ([eval-fn x]
   (cond
     (symbol? x)
     (try
       (eval-fn x)
       (catch Exception _ x))

     (sequential? x)
     (map (partial token-preprocess eval-fn) x)

     :else x)))


(defmacro preprocessed-case
  {:style/indent [0]}
  [{:keys [test eval-fn default]} & body]
  (let [preprocessed-body
        (mapcat
         (fn [[x y]]
           (list
            (if (fn? eval-fn)
              (token-preprocess eval-fn x)
              (token-preprocess x))
            y))
         (partition 2 body))]
    `(case ~test
       ~@preprocessed-body
       ~default)))


(defmacro case-enum
  "Like `case`, but explicitly dispatch on Java enum ordinals."
  {:style/indent [:defn]
   :url "http://stackoverflow.com/questions/16777814/is-it-possible-to-use-clojures-case-form-with-a-java-enum"}
  [e & clauses]
  (letfn [(enum-ordinal [e] `(.ordinal ^Enum ~e))]
    `(case ~(enum-ordinal e)
       ~@(concat
          (mapcat (fn [[test result]]
                    [(eval (enum-ordinal test)) result])
                  (partition 2 clauses))
          (when (odd? (count clauses))
            (list (last clauses)))))))
