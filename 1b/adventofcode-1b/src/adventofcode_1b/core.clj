(ns adventofcode-1b.core
    (:gen-class))

(defn parse-int [s]
    (Integer. (re-find  #"\d+" s )))

(defn turn-left [direction]
    (case direction
        \N \W
        \W \S
        \S \E
        \E \N))

(defn turn-right [direction]
    (case direction
        \N \E
        \W \N
        \S \W
        \E \S))

(defn position-move-in-direction [position direction steps]
    (case direction
        \N (update position :x + steps)
        \E (update position :y + steps)
        \S (update position :x - steps)
        \W (update position :y - steps)))

(defn position-move [current-position direction turn-fn next-step]
    (position-move-in-direction current-position (turn-fn direction) (parse-int next-step)))

(defn visited-positions [current-position direction turn-fn next-step]
    (map
        (fn [num]
            (position-move-in-direction current-position (turn-fn direction) (+ num 1)))
        (range 0 (parse-int next-step))))

(defn move [state next-step turn-fn]
    (hash-map
        :position (position-move (state :position) (state :direction) turn-fn next-step)
        :direction (turn-fn (state :direction))
        :visited-positions (into [] (concat
            (state :visited-positions)
            (visited-positions (state :position) (state :direction) turn-fn next-step)))))

(defn travel-blocks [state-init steps]
    (reduce
        (fn [state next-step]
            (case (get next-step 0)
                \R (move state next-step turn-right)
                \L (move state next-step turn-left)
                (throw (Exception. "Wtf? next-step should always start with R or L"))))
        state-init
        steps))

(defn abs [n] (max n (- n)))

(defn distance-from-init [position]
    (reduce-kv
        (fn [m k v]
            (+ m (abs v)))
        0
        position))

(defn first-position-visited-twice [seq]
    (def occurences (reduce
        (fn [result position]
            (def has-single-occurence (.contains (result :single-occurence) position))
            (hash-map
                :single-occurence (if has-single-occurence
                    (result :single-occurence)
                    (concat (result :single-occurence) [position]))
                :double-occurence (if has-single-occurence
                    (concat (result :double-occurence) [position])
                    (result :double-occurence))
                ))
        (hash-map :single-occurence [] :double-occurence [])
        seq))
    (first (occurences :double-occurence)))

(defn first-position-visited-twice-distance [s]
    (distance-from-init
        (first-position-visited-twice
            ((travel-blocks
                (hash-map
                    :position (hash-map :x 0, :y 0),
                    :direction \N,
                    :visited-positions [])
                (clojure.string/split s #", ")) :visited-positions))))

(defn -main [& args]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
        (println (first-position-visited-twice-distance line))))
