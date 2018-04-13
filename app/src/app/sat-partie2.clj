;;; # SAT Partie 2 : manipulations de formules propositionnelles

;;; Ne pas oublier la dépendance suivante dans project.clj
;;; [org.clojure/core.match "0.3.0-alpha5"]

(ns sat-partie2
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.set :as set]))


;; EXERCICE : ajouter  l'implication et l'équivalence dans tout ce qui suit

'(==> a b)
'(<=> a b)

;;; ## Simplifications des formules

(defn simplify-one [f]
  (match f
         ;; *** simplification du not ***
         ;; (not true) -> false
         (['not true] :seq) false
         ;; (not false) -> true
         (['not false] :seq) true
         ;; (not (not a)) -> a
         (['not (['not a] :seq)] :seq) a
         ;; *** simplification du or ***
         ;; (or true a) -> true
         (['or true a] :seq) true
         ;; (or a true) -> true
         (['or a true] :seq) true
         ;; (or false a) -> a
         (['or false a] :seq) a
         ;; (or a false) -> a
         (['or a false] :seq) a
         ;; *** simplification du and ***
         ;; (and true a) -> a
         (['and true a] :seq) a
         ;; (and a true) -> a
         (['and a true] :seq) a
         ;; (and false a) -> false
         (['and false a] :seq) false
         ;; (and a false) -> false
         (['and a false] :seq) false
         :else f)) 

(simplify-one '(not true))
(simplify-one '(not false))
(simplify-one '(not (not true)))
(simplify-one '(not (or true (not false))))

(defn simplify [f]
  (match f
         ([op a] :seq) (simplify-one (list op (simplify a)))
         ([op a b] :seq) (simplify-one (list op (simplify a) 
                                             (simplify b)))
         :else f))

(simplify '(or (not (or (not true)
                        (and (or (not x) false)
                             (or (not false) x))))
               (not (or y (and false z)))))

(simplify '(not (or true (not false))))

;;; ## Forme normale NNF

(defn nnf' [f]
  (match f
         ;; not .. and
         (['not (['and a b] :seq)] :seq)
         (list 'or (nnf' (list 'not  a)) (nnf' (list 'not b)))
         ;; not .. or
         (['not (['or a b] :seq)] :seq)
         (list 'and (nnf' (list 'not  a)) (nnf' (list 'not b)))
                                        ;: not .. not
         (['not (['not a] :seq)] :seq) (nnf' a)
         ;; and ..
         (['and a b] :seq) (list 'and (nnf' a) (nnf' b))
         ;; or ..
         (['or a b] :seq) (list 'or (nnf' a) (nnf' b))
         ;; TODO ==> et <=>
         :else f))

(nnf' '(not (or true (not false))))
(nnf' '(or (not (or (not true)
                    (and (or (not x) false)
                         (or (not false) x))))
           (not (or y (and false z)))))

(simplify '(or (not (or (not true)
                        (and (or (not x) false)
                             (or (not false) x))))
               (not (or y (and false z)))))

(simplify (nnf' '(or (not (or (not true)
                              (and (or (not x) false)
                                   (or (not false) x))))
                     (not (or y (and false z))))))

(defn nnf [f]
  (nnf' (simplify f)))

;;; ## Forme normale disjonctive DNF

(defn distrib [f]
  (match f
         (['and (['or a b] :seq) c] :seq)
         (list 'or (distrib (list 'and a c)) 
               (distrib (list 'and b c)))
         (['and a (['or b c] :seq)] :seq)
         (list 'or (distrib (list 'and a b)) 
               (distrib (list 'and a c)))
         :else f))

;; Remarque : f doit être en NNF
(defn dnf' [f]
  (match f
         (['and a b] :seq) (distrib (list 'and (dnf' a) 
                                          (dnf' b)))
         (['or a b] :seq) (list 'or (dnf' a) 
                                (dnf' b))
         :else f))

(defn dnf [f]
  (dnf' (nnf f)))

(dnf '(and (or a (and b c)) (or (not a) (not c))))

;;; Problème : c'est pas lisible et c'est simplifiable
;;; Solution : représentation sous forme d'ensemble (conjonctif) de clauses (disjonctives)

(defn setify-and [f]
  (match f
         (['and a b] :seq) 
         (set/union (setify-and a) (setify-and b))  
         :else #{f}))

(setify-and '(and a (and a (and (not b ) (not b)))))

(defn setify-dnf [f]
  (match f
         (['and a b] :seq) #{(setify-and f)}
         (['or a b] :seq) (set/union (setify-dnf a) (setify-dnf b))
         :else #{#{f}}))

(setify-dnf
 '(or (or (and a (not a)) (and a (not c))) (or (and (and b c) (not a)) (and (and b c) (not c)))))


(defn make-clause [bool clause x]
  (let [x (if bool x (list 'not x))
        not-x (simplify (list 'not x))]
    (cond
     (contains? clause x) true
     (contains? clause not-x) (disj clause not-x)
     :else clause)))

(defn make-true-false [bool phi x]
  (reduce
   (fn [phi' clause']
     (case clause'
       nil (reduced nil)
       true phi'
       ;;else
       (conj phi' clause')
       ))
   #{}
   (map #(make-clause bool % x) phi)))

(defn make-true [phi x]
  (make-true-false true phi x))

(defn make-false [phi x]
  (make-true-false false phi x))

(make-false '#{#{x y z} #{x z a} #{a z e}} 'x)

;;phi doit toujours être en dcnf => ensemble d'ensemble

(defn find-1-literal [f]
  (some #(when (= (count %) 1) (first %)) f))

(find-1-literal '#{#{x (not g)} #{(not z)} #{z (not x)}})

(defn rule-1-literal [phi]
  (if-let [litt (find-1-literal phi)]
    (if-let [phi' (if (symbol? litt)
                    (make-true phi litt)
                    (make-false phi (second litt)))]
      [phi', (if (symbol? litt) litt (second litt)), (symbol? litt)]
      ;;else formule insatisfiable
      [nil, nil, nil]) ;;insatisfiable
    ;;else on n'a pas trouvé
    nil))


(defn get-var [litt]
  (if (symbol? litt)
    litt
    (second litt)))

(defn varfreq1 [m clause]
  (reduce (fn [m litt]
            (let [x (get-var litt)]
              (if-let [xnb (get m x)]     
                (assoc m x (inc xnb))
                (assoc m x 1)))) m clause))

(defn varfreqs [phi]
  (reduce varfreq1 {} phi))

(defn max-val [m]
  (loop [m m, xval nil, maxval 0]   
    (if (seq m)
      (let [[y yval] (first m)]
        (if (> yval xval)
          (recur (rest m) y yval)
          (recur (rest m) xval maxval)))
      ;;à la fin du let
      maxval)))

(defn max-splitter [phi]
  (max-val (varfreqs phi)))

(defn find-neg-pos [phi]
  (loop [phi phi nm {}]
    (if-let [clause (first phi)]
      (let [nm (reduce
                #(let [[var kw] (cond
                                 (symbol? %2) [%2 :pos]
                                 :else [(second %2) :neg])]
                   (if (contains? %1 var)
                     (cond (= kw (get %1 var)) %1
                           :else (assoc %1 var :suppr))
                     (assoc %1 var kw)))
                nm
                clause)]
        (recur (rest phi) nm))
      (some #(when (not (= :suppr (second %))) %) nm))))

(find-neg-pos '#{#{(not a) (not x)} #{(not a) x}})

(defn rule-aff-neg [phi]
  (if-let [[x signe] (find-neg-pos phi)]
    (case signe 
      :positive (make-true phi x)
      :negative (make-false phi x))
    ;;Pas de pos ou de neg
    nil))

(defn dpll
  "prend une formule ph: et retourne la map des variables/valeurs (instanciation) ou no si non satisfiable"
  ([phi] (dpll phi {} max-splitter))
  ([phi splitter] (dpll phi {} splitter))
  ([phi sat splitter]
   (loop [phi phi, sat sat]
     (if (empty? phi)
       ;;sat => si le joueur a gagné
       (if-let [[phi', x, xval] (rule-1-literal phi)]
         ;;=> cherche dans une formule les clauses avec un seul littéral
         (if (nil? phi')
           nil
           (recur phi' (assoc sat x xval)))
         ;;Pour l'affirmative, la variable est tout le temps faux (resp. vrai)
         ;;On en cherche une, puisqu'on ne peut pas tout appliquer d'un coup
         (if-let [[phi', x, xval] (rule-aff-neg phi)]
           (recur phi' (assoc sat x xval))
           (let [x (splitter phi)]
             (or (let [phi-true (make-true phi x)]
                   (and phi-true (dpll phi-true (assoc sat x true) splitter)))
                 (let [phi-false (make-false phi x)]
                   (and phi-false (dpll phi-false (assoc sat x false) splitter)))
                 nil))))))))

(symbol? 'and)



(defn nenf' [f]
  (match f
         ;; not .. and
         (['not (['and a b] :seq)] :seq)
         (list 'or (nenf' (list 'not  a)) (nenf' (list 'not b)))
         ;; not .. or
         (['not (['or a b] :seq)] :seq)
         (list 'and (nenf' (list 'not  a)) (nenf' (list 'not b)))
         ;: not .. not
         (['not (['not a] :seq)] :seq) (nenf' a)
         ;; and ..
         (['and a b] :seq) (list 'and (nenf' a) (nenf' b))
         ;; or ..
         (['or a b] :seq) (list 'or (nenf' a) (nenf' b))
         ;; TODO ==>
         (['==> a b] :seq) (list '==> (nenf' a) (nenf' b))
         ;;et <=>
         (['<=> a b] :seq) (list '<=> (nenf' a) (nenf' b))
         ;; <=>
         (['not (['<=> a b] :seq)] :seq) ('<=> (nenf' a) (nenf' (list 'not b)))
         :else f))

(nenf' '(==> (or true (not false))))
(nenf' '(or (not (or (not true)
                     (and (or (not x) false)
                          (or (not false) x))))
            (not (or y (and false z)))))

(simplify '(or (<=> (or (not true)
                        (and (or (not x) false)
                             (or (not false) x))))
               (not (or y (and false z)))))

(simplify (nenf' '(or (not (or (not true)
                               (and (or (not x) false)
                                    (or (not false) x))))
                      (not (or y (and false z))))))

(defn nenf [f]
  (nenf' (simplify f)))

(nenf '(or (not (or (not true)
                    (and (or (not x) false)
                         (or (not false) x))))
           (not (or y (and false z)))))

(defn dcnf-aux [f equivs]
  (match f
         ([op a b] :seq)
         (let [[a', equivs1] (if (symbol? a) [a equivs] (dcnf-aux a equivs))
               [b', equivs2] (if (symbol? b) [b equivs1] (dcnf-aux b equivs1))
               f' (list op a' b')]
           (if-let [eq (get equivs2 f')]
             [eq equivs2]
             ;;si on a pas trouvé la formule
             (let [v (symbol (str "$" (inc (count equivs2))))]
               [v (assoc equivs2 f' v)])))))

(println "C IC PAULINE"(dcnf-aux '(and (and a b) (or a b)) {}))

;; EXERCICE : retirer les clauses qui contiennent un litéral et sa négation
;; fonction :  filter-trivial

;; EXERCICE : si on a une clause C1 incluse dans une clause C2
;; (par exemple: #{a (not b)}   et  #{a (not b) c})
;; alors on retire la plus grande C2 ..
;; fonction :  filter-subsume

;; EXERCICE : en déduire une fonction dnfs qui prend une
;; formule quelconque et retourne la formule DNF simplifiée représentée par des ensembles

;; EXERCICE :  comment passer d'une DNF sous forme d'ensemble d'ensembles à une CNF ?
;;             (indice : la CNF d'une formule f  est liée à la DNF de (not f) )

;; En déduire une fonction :  cnfs prend une 
;; formule quelconque et retourne la formule CNF simplifiée représentée par des ensembles
;; (en passant par la représentation DNF)
