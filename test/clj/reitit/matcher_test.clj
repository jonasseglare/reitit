(ns reitit.matcher-test
  (:import [reitit SegmentTrie])
  (:require [clojure.test :refer :all]))

(defn main-fn []
  (SegmentTrie/main nil))

(deftest splitting-test
  (is (= ["b" "c"]
         (SegmentTrie/split "/b/c"))))


(deftest mini-parse-test
  (let [t (doto (SegmentTrie.)
            (.add "/api/kattskit" nil))
        m (.matcher t)]
    (is (SegmentTrie/lookup m "/api/kattskit"))
    (is (not (SegmentTrie/lookup m "/api/kattsk1t")))))


(deftest wild-test
  (let [t (doto (SegmentTrie.)
            (.add "/api/:a/:b" nil))
        m (.matcher t)]
    (let [y (SegmentTrie/lookup m "/api/mj/ao")]
      (is (=
           (.params y)
           {:a "mj"
            :b "ao"}))))

  ;; How does it work if the same parameter occurs twice?
  (let [t (doto (SegmentTrie.)
            (.add "/api/:a/:b/:a" nil))
        m (.matcher t)]

    ;; Works as expected
    (let [y (SegmentTrie/lookup m "/api/mj/ao/mj")]
      (is (=
           (.params y)
           {:a "mj"
            :b "ao"})))

    ;; The first occurrence dominates.
    (let [y (SegmentTrie/lookup m "/api/mj/ao/kk")]
      (is (=
           (.params y)
           {:a "mj"
            :b "ao"})))))


(deftest catchall-test
  (let [t (doto (SegmentTrie.)
            (.add "/api/*mjao" nil))
        m (.matcher t)]
    (is (SegmentTrie/lookup m "/api/a/b/c"))
    (is (not (SegmentTrie/lookup m "/ipa/a/b/c")))))

(deftest amgituities
  (let [t (doto (SegmentTrie.)
            (.add "/api/:id/mu" nil)
            (.add "/api/mu/:index" nil))
        m (.matcher t)]
    (is (= {:index "mu"} (.params (SegmentTrie/lookup
                                   m "/api/mu/mu")))))
  (let [t (doto (SegmentTrie.)
            (.add "/api/mu/:index" nil)
            (.add "/api/:id/mu" nil))
        m (.matcher t)]
    (is (= {:index "mu"} (.params (SegmentTrie/lookup
                                   m "/api/mu/mu"))))))


;;; What the parsing algorithm seems to do:
;;;  * If a route is successfully matched, attach data to it.
;;;  * Prioritize catch-all matches over other matches
;;;  * Prioritize static matches over wild card matches
;;;  * If a wild-card appears twice, map it to the
;;;    substring of the first occurrence.

(deftest check-the-above-assumptions
  (let [t (doto (SegmentTrie.)
            (.add "/api/braellerhur" {:info "Grymt bra!"})
            (.add "/api/b/*stuff" nil)
            (.add "/api/b/:otherstuff" nil)
            (.add "/api/c/:a" nil)
            (.add "/api/c/:b" nil)
            (.add "/api/d/:b" nil)
            (.add "/api/d/k" :static))
        m (.matcher t)]
    (is (= (.data (SegmentTrie/lookup m "/api/braellerhur"))
           {:info "Grymt bra!"}))
    (is (= (.params (SegmentTrie/lookup m "/api/b/kattskit"))
           {:stuff "kattskit"}))
    (is (= (.params (SegmentTrie/lookup m "/api/c/kattskit"))
           {:b "kattskit"}))
    (is (= (.data (SegmentTrie/lookup m "/api/d/k"))
           :static))))
