(ns euler.p001)

(defn multiples-of
  "Returns a lazy sequence that generates a sequential ordering for
  the multiples of x, starting with x."
  [x]
  (iterate #(+ % x) x))

(defmacro if3
  [cmp less equal greater]
  `(let [cmp# ~cmp]
    (cond (neg? cmp#) ~less
          (zero? cmp#) ~equal
          :else ~greater)))

(defn combine-seq
  "Returns a lazy sequence that is a combination of the two sorted
  sequences provided, based on the compare function."
  [c1 c2]
  (lazy-seq (let [e1 (first c1)
                  e2 (first c2)]
              (if3 (compare e1 e2)
                   (cons e1 (combine-seq (rest c1) c2))
                   (cons e1 (cons e2 (combine-seq (rest c1) (rest c2))))
                   (cons e2 (combine-seq c1 (rest c2)))))))

(defn sum-unique-multiples
  [n & seqs]
  (->> (apply combine-seq seqs)
       (take-while #(< % n))
       (into #{})
       (apply +)))

(defn answer
  []
  (sum-unique-multiples 1000 (multiples-of 3) (multiples-of 5)))
