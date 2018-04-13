(defn big-divide
  [n a b]
  (letfn [(my-sum [a q] (* a (/ (* (bigint q) (inc q)) 2)))]
    (let [a-q (quot (dec n) a)
          b-q (quot (dec n) b)
          c (* a b)
          c-q (quot (dec n) c)]
      (- (+ (my-sum a a-q) (my-sum b b-q)) (my-sum c c-q)))))

(let [a (* 10000 10000 10000)]
  (type a))


(str (big-divide (* 10000 10000 10000) 757 809))
