(defn infinit
  ([f]
   (map
    (fn [n]
      (map #(str n %) (range)))
    (range)))
  ([f m n]
   (drop m
         (map
          (fn [s]
            (map #(drop n %) s))
          (infinit f)))))

(drop 2 [1])



(defn debut [auto]
  (:start auto))

(defn accepted [etat auto]
  (contains? (:accepts auto) etat))

(defn transition [etat auto]
  (get (:transitions auto) etat))

(defn add_l [etat auto mot]
  (map #(list (second %2) (conj %1 (first %2))) (repeat mot) (transition etat auto)))

(apply str '(sss s))

(defn regex
  ([auto] (regex auto (list (list (debut auto) [""]))  0))
  ([auto lst-mots brk]
    (do
      (println lst-mots)
      (let [res
            (disj
             (set
              (map
               #(when (accepted (first %) auto)
                  (apply str (second %)))
               lst-mots)) nil)
            lst-mots
            (mapcat
             #(add_l (first %) auto (second %))
             lst-mots)]
        (if (= brk 7)
          res
          (lazy-seq (clojure.set/union res (regex auto lst-mots (inc brk)))))))))


(conj '(s s) 's)

(def auto '{:states #{q0 q1 q2} :alphabet #{a b c} :start q0 :accepts #{q1 q2 q3}
            :transitions {q0 {a q1} q1 {b q2} q2 {c q3}}})

(def auto1 '{:states #{q0 q1} :alphabet #{0 1} :start q0 :accepts #{q0}
           :transitions {q0 {0 q0, 1 q1} q1 {0 q1, 1 q0}}})


(accepted 'q1 auto)
(add_l 'q0 auto '())
(regex auto)
