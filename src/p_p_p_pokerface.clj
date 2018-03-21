(ns p-p-p-pokerface)


(defn
  rank
  [card]
  (let [[fst _] card
        card-values {\A 14, \K 13, \Q 12, \J 11, \T 10}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get card-values fst))))
(defn
  suit
  [card]
  (let [[_ snd] card]
  (str snd)))

(defn
  pair?
  [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 1)
    true
    false))


(defn
  three-of-a-kind?
  [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 2)
    true
    false))

(defn
  four-of-a-kind?
  [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 3)
    true
    false))

(defn
  flush?
  [hand]
  (apply = (map suit hand)))

(defn
  full-house?
  [hand]
  (= (vals (frequencies (map rank hand))) '(3 2)))


(defn
  two-pairs?
  [hand]
  (or (four-of-a-kind? hand)
      (= (vals (frequencies (map rank hand))) '(2 2 1))))

(defn
  straight?
  [hand]
  (let [card-values (sort (map rank hand))
        different-values? (apply < card-values)
        low-ace-card-values (sort (replace {14 1} (map rank hand)))
        adjanced-values? (or (= (- (last card-values) (first card-values)) 4)
                             (= (- (last low-ace-card-values) (first low-ace-card-values)) 4))]
  (and different-values?
       adjanced-values?)))

(defn
  straight-flush?
  [hand]
  (and (straight? hand)
       (flush? hand)))

(defn
  high-card?
  [hand]
  true)

(defn
  value
  [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        possible-values (map second (filter #((first %) hand) checkers))]
  (apply max possible-values)))


