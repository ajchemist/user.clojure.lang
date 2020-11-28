(ns user.clojure.import.alpha
  (:refer-clojure :exclude [import def])
  (:import
   clojure.lang.Var
   ))


(defn import-var
  "Imports a single var from one namespace to the current one.
   (import-var 'ifl #'clojure.core/if-let)
   => anything ; #'hara.namespace.import-test/ifl
   (eval '(ifl [a 1] (inc a))) => 2
   "
  {:style/indent [:defn]}
  [name ^Var var]
  (when (.hasRoot var)
    (intern
      *ns*
      (vary-meta name #(merge %2 %) (meta var))
      @var)))


(defmacro def [name target] `(import-var '~name (resolve '~target)))


(defn import-namespace
  "Imports all or a selection of vars from one namespace to the current one.
   (import-namespace 'hara.common.checks '[bytes? long?])
   (eval '(long? 1))  => true
   (eval '(bytes? 1)) => false
   "
  {:style/indent [:defn]}
  ([ns]
   (import-namespace ns nil))
  ([ns vars]
   (let [all-vars      (ns-publics ns)
         selected-vars (if vars (select-keys all-vars vars) all-vars)]
     (run!
       (fn [[n v]] (import-var n v))
       selected-vars))))


(defmacro import
  "Imports all or a selection of vars from one namespace to the current one.
   (import hara.common.checks [bytes? long?]) => nil
   (eval '(long? 1))  => true
   (eval '(bytes? 1)) => false
   (import hara.common.checks :all) => nil
   (eval '(bigint? 1)) => false
   "
  {:style/indent [:defn]}
  [nsp vars & more]
  `(do
     (require (quote ~nsp))
     (import-namespace
       (quote ~nsp)
       ~(when-not (= :all vars)
          `(quote ~vars)))
     ~(when more
        `(import ~@more))))
