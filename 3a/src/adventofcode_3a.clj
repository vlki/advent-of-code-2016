(ns adventofcode-3a
    (:gen-class))

(defn parse-int [s]
    (Integer. (re-find  #"\d+" s )))

(defn is-line-valid-triangle [line]
    (def line-parts (clojure.string/split line #"\s+"))
    (def sides (sort (map parse-int line-parts)))

    ;; (sides[0] + sides[1]) > sides[2]
    (> (+ (nth sides 0) (nth sides 1)) (nth sides 2)))

(defn -main [& args]
    (def input-lines-seq (line-seq (java.io.BufferedReader. *in*)))

    (def valid-triangles-num
        (count
            (filter
                (fn [line]
                    (is-line-valid-triangle line))
                input-lines-seq)))

    (println valid-triangles-num))
