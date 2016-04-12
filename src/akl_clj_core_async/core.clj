(ns akl-clj-core-async.core
  (:require [clojure.core.async :as a]
            [clojure.test :as t :refer [deftest is]]))

(defmacro is-timeout
  "This fails a test if it takes longer than 1s to complete.
   You don't need to understand this, but bonus points if you can explain it."
  ([form] `(is-timeout ~form nil))
  ([form msg] `(let [test-ch# (a/go (is ~form ~msg))
                     timeout-ch# (a/timeout 1000)
                     [_# ch#] (a/alts!! [test-ch#
                                         timeout-ch#])]
                 (is (not= ch# timeout-ch#) "Test timed out"))))

(defn make-hello-channel
  "Should return a channel with the string hello on it."
  []
  (let [ch (a/chan 1)]

    ch))

(deftest hello-channel
  (let [ch (make-hello-channel)]
    (is-timeout (= "hello" (a/<!! ch)))))

(defn go-do-an-exceptional-thing
  []
  (let [ch (a/chan)]
    (a/go
      (throw (ex-info "Something went wrong" {}))
      (a/>! ch "a value"))
    ch))

(deftest exceptional-thing
  []
  (let [ch (go-do-an-exceptional-thing)]
    (is-timeout (a/<!! ch))))

(defn go-put-a-lot-of-things-on-a-channel
  []
  (let [ch (a/chan)]
    (dotimes [n 1025]
      (a/go
        (a/>! ch n)))
    ch))

(deftest get-lots-of-things-off-the-channel
  (let [ch (go-put-a-lot-of-things-on-a-channel)]
    (loop [counter 0]
      (when (< counter 1025)
        (is-timeout (a/<!! ch))
        (recur (inc counter))))))

(defn do-a-long-running-thing []
  (a/go (Thread/sleep 700)
            1))

(deftest long-running-things
  (let [long-running-things (repeatedly 100 do-a-long-running-thing)]
    (dorun long-running-things)
    (is-timeout (= 100 (count (map a/<!! long-running-things))))))

;So if you got to here, you're done. Congratulations! Go deploy some core.async code!
;Or, come share in some war stories, let me know how the talk went and what you'd like to see next time.
