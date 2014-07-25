(ns looping-is-recursion)

(defn factorial [n]
  (apply * (range 1 (inc n))))

;(factorial 4)

(defn accumulating-factorial-helper [acc n]
  (if (zero? n)
    acc
    (accumulating-factorial-helper (* acc n) (dec n))))

(defn accumulating-factorial [n]
  (accumulating-factorial-helper 1 n))

;(accumulating-factorial 4)

(defn recur-factorial [n]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc n) (dec n))))]
    (helper 1 n)))

;(recur-factorial 4)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

;(power 2 4)
;(power 2 2)  ;=> 4
;(power 5 3)  ;=> 125
;(power 7 0)  ;=> 1
;(power 0 10) ;=> 0

(defn last-element [a-seq]
  (let [helper (fn [prev curr-seq]
                 (if (empty? curr-seq)
                   prev
                   (recur (first curr-seq) (rest curr-seq))))]
    (helper nil a-seq)))

;(last-element [])      ;=> nil
;(last-element [1 2 3]) ;=> 3
;(last-element [2 5])   ;=> 5

(defn seq= [seq1 seq2]
  (if-not (= (count seq1) (count seq2))
    false
    (let [helper (fn [a-seq b-seq]
                   (if (empty? a-seq)
                     true
                     (let [first-a (first a-seq)
                           first-b (first b-seq)]
                       (if-not (= first-a first-b)
                         false
                         (recur (rest a-seq) (rest b-seq))))))]
      (helper seq1 seq2))))

;(seq= [1 2 4] '(1 2 4))  ;=> true
;(seq= [1 2 3] [1 2 3 4]) ;=> false
;(seq= [1 3 5] [])        ;=> false

(defn find-first-index [pred a-seq]
  (loop [the-seq a-seq
         index 0]
    (if (empty? the-seq)
      nil
      (if (pred (first the-seq))
        index
        (recur (rest the-seq) (inc index))))))

;(find-first-index even? [1 3 2])
;(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
;(find-first-index zero? [1 1 3 7 2])                          ;=> nil
;(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
;(find-first-index nil? [])                                    ;=> nil

(defn avg [a-seq]
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

