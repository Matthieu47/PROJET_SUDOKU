

(map #(Integer/parseInt %) (clojure.string/split "4 5 6 7" #" "))


(defn squares [s]
  (let [my-nb (clojure.string/split s #",")]
    (println my-nb)
    (apply str (butlast (mapcat  #(if (zero? (rem (Math/sqrt %) 1)) (str % ",") "") (map #(Integer/parseInt %) my-nb))))))

(str 4)

(str \4 \9)

(squares "4,9")
"4, 9"

(= (squares "4,5,6,7,8,9") "4,9")

(type (fn [a]))
(type '(* a b c))

(defn pb-177 [s]
  (loop [s s pile-p '() pile-c '() pile-a '()]
    (if-let [c (first s)]
      (cond
       (= \( c) (recur (rest s) (conj pile-p \() pile-c pile-a)
       (= \) c) (if (seq pile-p) (recur (rest s) (pop pile-p) pile-c pile-a) false)
       (= \[ c) (recur (rest s) pile-p (conj pile-c \[) pile-a)
       (= \] c) (if (seq pile-c) (recur (rest s) pile-p (pop pile-c) pile-a) false)
       (= \{ c) (recur (rest s) pile-p pile-c (conj pile-a \{))
       (= \} c) (if (seq pile-a) (recur (rest s) pile-p pile-c (pop pile-a)) false)
       :default (recur (rest s) pile-p pile-c pile-a))
      (not (or (seq pile-p) (seq pile-c) (seq pile-a))))))

(pb-177 "[ { ] }")
