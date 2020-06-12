(ns user.clojurescript.analyzer.api-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [user.clojurescript.analyzer.api :as u.ana.api]
   ))


(deftest main
  (try
    #_(u.ana.api/debug-ns-interns 'clojure.string)
    (u.ana.api/import-namespace 'clojure.string)
    (u.ana.api/import-var 'yatrim #'clojure.string/trim)
    (finally
      (is true)))
  )
