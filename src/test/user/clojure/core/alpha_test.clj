(ns user.clojure.core.alpha-test
  (:refer-clojure :exclude [find-ns import resolve])
  (:require
   [clojure.test :as test :refer [deftest is are testing]]
   [user.clojure.core.patch.alpha :refer :all]
   ))


;;


(def fast-memoize-thread-sleep
  (fast-memoize
    (fn [n] (dotimes [_ n] (Thread/sleep 1)) n)))


(deftest main
  (is
    (are [x y] (= x y)
      (nsym :clojure.core)
      (nsym :clojure.core/some)
      (nsym 'clojure.core)
      (nsym 'clojure.core/some)))


  (is
    (= (deep-merge
         {:a {:b {:c 1}}
          :x {:y 2
              :z 3
              :w {:p 4}}}
         {:a {:b {:c 10}}
          :x {:y 20
              :z 30
              :w {:p 40
                  :q 50}}}
         {:a {:b {:c 100}}
          :x {:y 200
              :z 300
              :w {:p 400
                  :q 500
                  :r 600}}})
       {:a {:b {:c 100}}
        :x {:y 200
            :z 300
            :w {:p 400
                :q 500
                :r 600}}}))
  )


(comment
  (time (fast-memoize-thread-sleep 1000))
  (time (fast-memoize-thread-sleep 1000))
  )
