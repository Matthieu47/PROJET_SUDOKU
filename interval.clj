(defn inter
  [s]
  (if (empty? s) []
      (let [sort-s (dedupe (sort s))]
        (loop [s (rest sort-s) prems (first sort-s) res [] prec (first sort-s)]
          (if-let [elm (first s)]
            (if (= (dec elm) prec)
              (recur (rest s) prems res elm)
              (recur (rest s) elm (conj res [prems prec]) elm))
            (conj res [prems prec]))))))

(inter [1 2 3])
(inter [])
(inter [10 9 8 1 2 3])
(inter [1 1 1 1 1])
