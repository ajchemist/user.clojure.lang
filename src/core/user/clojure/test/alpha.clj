(ns user.clojure.test.alpha
  (:require
   [clojure.test :as test :refer [testing is are]]
   [clojure.string :as str]
   [clojure.java.shell :as shell]
   ))


(defn exec
  [& cmd]
  (testing cmd
    (println "Running" (str "\"" (str/join " " cmd) "\""))
    (let [{:keys [exit out err]} (apply shell/sh cmd)]
      (is (= exit 0))
      (when-not (str/blank? err)
        (binding [*out* *err*]
          (println err)))
      (when-not (str/blank? out)
        (println out)))))


(defn wrap-res
  [f]
  (let [{:keys [fail error] :as res} (f)]
    (when (pos? (+ fail error))
      (throw (ex-info "Test unsuccessful." res)))))


(defn main-wrap-res
  [f]
  (try
    (let [{:keys [fail error]} (f)]
      (when (pos? (+ fail error))
        (System/exit 1)))
    (finally
      (shutdown-agents))))
