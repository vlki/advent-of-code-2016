(ns adventofcode-2a
    (:gen-class))

;; Keypad is as follows:
;; 1 2 3
;; 4 5 6
;; 7 8 9
(defn position-from-command [position command-char]
    (case position
        1 (case command-char \U 1 \R 2 \D 4 \L 1)
        2 (case command-char \U 2 \R 3 \D 5 \L 1)
        3 (case command-char \U 3 \R 3 \D 6 \L 2)
        4 (case command-char \U 1 \R 5 \D 7 \L 4)
        5 (case command-char \U 2 \R 6 \D 8 \L 4)
        6 (case command-char \U 3 \R 6 \D 9 \L 5)
        7 (case command-char \U 4 \R 8 \D 7 \L 7)
        8 (case command-char \U 5 \R 9 \D 8 \L 7)
        9 (case command-char \U 6 \R 9 \D 9 \L 8)))

(defn apply-commands-line [in-state commands-line]
    (def commands-seq (seq (char-array commands-line)))

    (def out-position
        (reduce
            (fn [position command-char]
                (position-from-command position command-char))
            (in-state :last-position)
            commands-seq))

    { :code (concat (in-state :code) [out-position]) :last-position out-position })

(defn -main [& args]
    (def init-state { :code [] :last-position 5 })
    (def input-lines-seq (line-seq (java.io.BufferedReader. *in*)))

    (def end-state
        (reduce
            (fn [state line]
                (apply-commands-line state line))
            init-state
            input-lines-seq))

    (println end-state))
