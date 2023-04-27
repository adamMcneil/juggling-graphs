(defmacro check [expected actual]
  `(let [e# ~expected
         a# ~actual]
     (when-not (= e# a#)
       (throw (ex-info "check failed" {:expected e#
                                       :actual a#})))))

(def catch-slot 1)

(def open-slot 0)

(defn open-slot? [s]
  (= s open-slot))

(defn initial-state [ball-count]
  (-> ball-count (repeat catch-slot) vec))

(check [1 1] (initial-state 2))

(check [1 1 1] (initial-state 3))

(defn toss [state n]
  (let [state' (->> state rest vec)]
    (if (open-slot? n)
      (do
        (when-not (open-slot? (first state))
          (throw (ex-info "cannot toss 0 from this state" {:state state})))
        state')
      (let [toss-state (conj (vec (repeat (dec n) open-slot)) catch-slot)]
        (->> (range (max (count toss-state) (count state')))
             (mapv (fn [i]
                     (max (get toss-state i open-slot)
                          (get state' i open-slot)))))))))

(check [1 1 1] (toss [1 1 1] 3))

(check [1 1 0 1] (toss [1 1 1] 4))

(check [1 1 0 0 1] (toss [1 1 1] 5))

(defn filter-state [allow-rests? state]
  (or allow-rests?
      (not (open-slot? (first state)))))

(check true (filter-state true [0 1]))

(check false (filter-state false [0 1]))

(check true (filter-state true [1 0 1]))

(check true (filter-state false [1 0 1]))

(defn pad-state [n state]
  (into state (vec (repeat n open-slot))))

(check [1 0 0 0 0] (pad-state 3 [1 0]))

(check [1 0 1 0 0] (pad-state 2 [1 0 1]))

(defn maximize-state [max-toss state]
  (pad-state (- (inc max-toss)
                (count state))
             state))

(check [1 0 0 0] (maximize-state 3 [1]))

(check [1 1 1 0] (maximize-state 3 [1 1 1]))

(defn possible-tosses [max-toss state]
  (if (open-slot? (first state))
    [0]
    (->> (maximize-state max-toss state)
         (map-indexed (fn [i s]
                        (when (open-slot? s)
                          i)))
         (remove nil?)
         vec)))

(check [0] (possible-tosses 3 [0 1]))

(check [3] (possible-tosses 3 [1 1 1]))

(check [3 4 5] (possible-tosses 5 [1 1 1]))

(check [1 2 3] (possible-tosses 3 [1 0 0]))

(check [1 2 3 4 5 6] (possible-tosses 6 [1 0 0]))

(defn find-reachable-states [max-toss allow-rests? starting-state]
  (loop [[s & more-s] #{starting-state}
         reachable #{}]
    (if s
      (let [reachable' (conj reachable s)]
        (recur (into more-s (->> (possible-tosses max-toss s)
                                 (map #(toss s %))
                                 (filter (partial filter-state allow-rests?))
                                 (remove reachable')))
               reachable'))
      reachable)))

(check #{[1 1] [1 0 1]} (find-reachable-states 3 false [1 1]))

(check #{[1 1] [0 1 1] [1 0 1]} (find-reachable-states 3 true [1 1]))

(defn maybe-transition [allow-rests? state n]
  (let [new-state (toss state n)]
    (when (filter-state allow-rests? new-state)
      [state n new-state])))

(check nil (maybe-transition false [1 0 1] 3))

(check [[1 0 1] 3 [0 1 1]]
       (maybe-transition true [1 0 1] 3))

(defn find-immediate-transitions-from [max-toss allow-rests? state]
  (->> (possible-tosses max-toss state)
       (map (partial maybe-transition allow-rests? state))
       (remove nil?)))

(check [[[1 1] 2 [1 1]]
        [[1 1] 3 [1 0 1]]
        [[1 1] 4 [1 0 0 1]]]
       (find-immediate-transitions-from 4 false [1 1]))

(defn find-all-transitions [ball-count max-toss allow-rests?]
  (->> (initial-state ball-count)
       (find-reachable-states max-toss allow-rests?)
       (mapcat (partial find-immediate-transitions-from max-toss allow-rests?))))

(check [[[1 1] 2 [1 1]]
        [[1 1] 3 [1 0 1]]
        [[1 0 1] 1 [1 1]]]
       (find-all-transitions 2 3 false))

(check [[[1 1] 2 [1 1]]
        [[1 1] 3 [1 0 1]]
        [[0 1 1] 0 [1 1]]
        [[1 0 1] 1 [1 1]]
        [[1 0 1] 3 [0 1 1]]]
       (find-all-transitions 2 3 true))

(defn state-str [state]
  (->> state
       (map {open-slot "☐"
             catch-slot "✗"})
       (clojure.string/join " ")))

(check "✗ ☐ ✗" (state-str [1 0 1]))

(defn make-graph-data [ball-count max-toss allow-rests?]
  (str "digraph G {\n"
       (->> (find-all-transitions ball-count max-toss allow-rests?)
            (map (fn [[start-state n end-state]]
                   (str "\t\"" (state-str start-state) "\" -> \"" (state-str end-state) "\" [label=\"" n "\"];\n")))
            (apply str))
       "}"))

(defn count-states [ball-count max-toss allow-rests?]
  (println (str ball-count " " max-toss " " allow-rests? " "
                (->> (find-all-transitions ball-count max-toss allow-rests?) 
                     (map first) 
                     distinct 
                     count))))

(check
"digraph G {
\t\"✗ ✗\" -> \"✗ ✗\" [label=\"2\"];
\t\"✗ ✗\" -> \"✗ ☐ ✗\" [label=\"3\"];
\t\"✗ ☐ ✗\" -> \"✗ ✗\" [label=\"1\"];
}"
(make-graph-data 2 3 false))

(defn make-graph
  ([ball-count max-toss allow-rests?]
   (make-graph ball-count max-toss allow-rests? (str "juggle-" ball-count "-" max-toss "-" allow-rests?)))
  ([ball-count max-toss allow-rests? base-file-name]
   (let [dot-file-name (str base-file-name ".dot")]
     (spit dot-file-name (make-graph-data ball-count max-toss allow-rests?))
     (count-states ball-count max-toss allow-rests?)
    ;;  (clojure.java.shell/sh "dot" "-Tpng" "-O" dot-file-name)
     
     ))
  )

(->> (for [ball-count (range 3 4)
           max-toss (range 6 7)
           allow-rests? [true]]
       (when (>= max-toss ball-count)
         (make-graph ball-count max-toss allow-rests?)))
     dorun)

;; (make-graph 3 5 false "juggle")