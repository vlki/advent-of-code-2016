(ns adventofcode-2b
    (:gen-class))

;; Keypad is as follows:
;;     1
;;   2 3 4
;; 5 6 7 8 9
;;   A B C
;;     D
(defn position-from-command [position command-char]
    (case position
        1 (case command-char \U 1 \R 1 \D 3 \L 1)
        2 (case command-char \U 2 \R 3 \D 6 \L 2)
        3 (case command-char \U 1 \R 4 \D 7 \L 2)
        4 (case command-char \U 4 \R 4 \D 8 \L 3)
        5 (case command-char \U 5 \R 6 \D 5 \L 5)
        6 (case command-char \U 2 \R 7 \D \A \L 5)
        7 (case command-char \U 3 \R 8 \D \B \L 6)
        8 (case command-char \U 4 \R 9 \D \C \L 7)
        9 (case command-char \U 9 \R 9 \D 9 \L 8)
        \A (case command-char \U 6 \R \B \D \A \L \A)
        \B (case command-char \U 7 \R \C \D \D \L \A)
        \C (case command-char \U 8 \R \C \D \C \L \B)
        \D (case command-char \U \B \R \D \D \D \L \D)))

(defn apply-commands-line [in-state commands-line]
    (def commands-seq (seq (char-array commands-line)))
    (def in-position (in-state :last-position))

    (def out-position
        (reduce
            (fn [position command-char]
                (position-from-command position command-char))
            in-position
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
