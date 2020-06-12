(ns user.clojure.macro
  "
  References:
  - https://github.com/zcaudate/hara/blob/master/src/hara/common/error.clj"
  )


;; * compile-time conditional block


(defmacro if-clj  {:style/indent [0]} [then else] (if (:ns &env) else then))
(defmacro if-cljs {:style/indent [0]} [then else] (if (:ns &env) then else))


(defmacro compile-if
  "Evaluates `test`. If it returns logical true (and doesn't throw), expands
  to `then`, otherwise expands to `else`. "
  {:style/indent [1]}
  ([test then     ] `(compile-if ~test ~then nil)) ; Back compatibility
  ([test then else]
   (if (try (eval test) (catch Throwable _ false))
     then
     else)))


(defmacro compile-when [test & body] `(compile-if ~test (do ~@body) nil))


;; * error


;;
;; If we place too much importance on exceptions, exception handling code
;; starts littering through the control code. Most internal code
;; do not require definition of exception types as exceptions are
;; meant for the programmer to look at and handle.
;;
;; Therefore, the exception mechanism should get out of the way
;; of the code. The noisy `try .... catch...` control structure
;; can be replaced by a `suppress` statement so that errors can be
;; handled seperately within another function or ignored completely.
;;


(defmacro throwable-class
  []
  (if (:ns &env)
    'js/Error
    'Throwable))


(defmacro exception
  [& xs]
  (if (:ns &env)
    `(js/Error. ~@xs)
    `(Exception. ~@xs)))


(defmacro error
  "Throws an exception when called.

   (error \"This is an error\")
   => (throws Exception \"This is an error\")

   (error (Exception. \"This is an error\")
          \"This is a chained error\")
   => (throws Exception \"This is a chained error\")"
  {:added "2.0"}
  [e & [opt? & more]]
  (cond
    (or (string? e) (keyword? e))
    `(throw (exception (str ~e ~opt? ~@more)))
    :else
    `(if (instance? ~(throwable-class) ~e)
       (throw (exception (str ~opt? ~@more) e))
       (throw (exception (str ~e ~opt? ~@more))))))


(defmacro suppress
  "Suppresses any errors thrown in the body.

   (suppress (error \"Error\")) => nil

   (suppress (error \"Error\") :error) => :error

   (suppress (error \"Error\") (fn [e] (.getMessage e))) => \"Error\""
  {:style/indent [1]}
  ([body]
   `(try ~body (catch ~(throwable-class) t#)))
  ([body catch-val]
   `(try ~body
         (catch ~(throwable-class) t#
           (cond (fn? ~catch-val) (~catch-val t#)
                 :else            ~catch-val)))))


;; * Anaphoric


(defmacro aif
  ([test then]
   `(let [~'it ~test]
      (if ~'it ~then)))
  ([test then else]
   `(let [~'it ~test]
      (if ~'it ~then ~else))))
