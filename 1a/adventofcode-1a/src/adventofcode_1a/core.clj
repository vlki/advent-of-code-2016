(ns adventofcode-1a.core
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

(defn move [position direction steps]
    (case direction
        \N (update position :x + steps)
        \E (update position :y + steps)
        \S (update position :x - steps)
        \W (update position :y - steps)))

(defn travel-blocks [position-init steps]
    (reduce
        (fn [state next-step]
            (case (get next-step 0)
                \R (hash-map
                    :position (move (state :position) (turn-right (state :direction)) (parse-int next-step))
                    :direction (turn-right (state :direction)))
                \L (hash-map
                    :position (move (state :position) (turn-left (state :direction)) (parse-int next-step))
                    :direction (turn-left (state :direction)))
                (throw (Exception. "Wtf? next-step should always start with R or L"))))
        position-init
        steps))

(defn abs [n] (max n (- n)))

(defn blocks-away [s]
    (reduce-kv
        (fn [m k v]
            (+ m (abs v)))
        0
        ((travel-blocks
            (hash-map :position (hash-map :x 0, :y 0), :direction \N)
            (clojure.string/split s #", ")) :position)))

(defn -main [& args]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
        (println (blocks-away line))))
