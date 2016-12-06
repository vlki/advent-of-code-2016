(ns adventofcode-3b
    (:gen-class))

(defn parse-int [s]
    (Integer. (re-find  #"\d+" s )))

(defn is-valid-triangle [sides]
    ;; (sides[0] + sides[1]) > sides[2]
    (def sorted-sides (sort sides))
    (> (+ (nth sorted-sides 0) (nth sorted-sides 1)) (nth sorted-sides 2)))

(defn line-to-nums [line]
    (def line-parts (clojure.string/split line #"\s+"))
    (map parse-int line-parts))

(defn -main [& args]
    (def input-lines-seq (line-seq (java.io.BufferedReader. *in*)))

    ; (println input-lines-seq)
    (def sides-list
        (reduce
            (fn [list sides-triple]
                (concat list sides-triple))
            []
            (map
                (fn [sides-triple]
                    (def sides0 [(nth (nth sides-triple 0) 0) (nth (nth sides-triple 1) 0) (nth (nth sides-triple 2) 0)])
                    (def sides1 [(nth (nth sides-triple 0) 1) (nth (nth sides-triple 1) 1) (nth (nth sides-triple 2) 1)])
                    (def sides2 [(nth (nth sides-triple 0) 2) (nth (nth sides-triple 1) 2) (nth (nth sides-triple 2) 2)])
                    [sides0 sides1 sides2])
                (partition 3 (map line-to-nums input-lines-seq)))))

    ; (println sides-list)

    (def valid-triangles-num
        (count (filter is-valid-triangle sides-list)))

    (println valid-triangles-num))
