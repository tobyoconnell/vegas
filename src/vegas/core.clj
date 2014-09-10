(ns vegas.core
  (:use clojure.pprint)
  (:use clojure.contrib.generic.functor)
  (:gen-class))

(def costs-2014-vegas
  "A sample data structure representing expenses. For each item, the :payer is
  the one who paid the amount :paid on behalf of all :users. Thus, all :users
  owe the :payer an equal share of :paid dollars."
  (let [everybody [:al :am :jd :kj :mr :rc :to]]
    [{:title "Marquee"  :paid 2333.00 :payer :kj :users everybody}
     {:title "Daylight" :paid 2309.00 :payer :to :users everybody}
     {:title "Drais"    :paid 1321.00 :payer :jd :users everybody}
     {:title "Pool bev" :paid   58.00 :payer :am :users [:al :to]}
     {:title "Room bev" :paid  108.00 :payer :rc :users everybody}
     {:title "Room A"   :paid  420.00 :payer :kj :users [:kj]}
     {:title "Room B"   :paid  420.00 :payer :rc :users [:am :rc]}
     {:title "Join B"   :paid   30.00 :payer :rc :users [:am :rc]}
     {:title "Room C"   :paid  420.00 :payer :am :users [:al :to]}
     {:title "Join C"   :paid   30.00 :payer :rc :users [:al :to]}
     {:title "Room D"   :paid  420.00 :payer :mr :users [:jd :mr]}
     {:title "Join D"   :paid   30.00 :payer :rc :users [:jd :mr]}]))

(def costs-2015-edc
  "Another sample data structure."
  (let [everybody [:ei :nw :to :zb]]
    [{:title "PH"  :paid 9999.99 :payer :zb :users everybody}
     {:title "PH"  :paid 9999.99 :payer :nw :users everybody}
     {:title "MGM" :paid 9999.99 :payer :to :users everybody}]))

(defn names' [costs]
  "Extract all people's names from the :payer and :users values to find out all
  involved people."
  (distinct (into (map :payer costs)
                  (flatten (map :users costs)))))

(defn names [costs]
  "Extract all people's names from the :payer and :users values to find out all
  involved people."
  (distinct (into (map :payer costs)
                  (flatten (map :users costs)))))

(defn format-money [n]
  "Round to two decimal places and add dollar sign. Parenthesize negatives."
  (if (< n 0)
    (format "($%.2f)" (Math/abs n))
    (format "$%.2f" n)))

(defn print-costs [costs]
  (doseq [c costs]
    (println (format "%s spent %s on %s for: %s"
                     (name (:payer c))
                     (format-money (:paid c))
                     (:title c)
                     (clojure.string/join \space (map name (:users c)))))))

(defn print-persons [persons]
  (doseq [[person {:keys [share paid balance]}] persons]
    (println (format "%s paid %s of his %s share, so has balance of %s"
                     (name person)
                     (format-money paid)
                     (format-money share)
                     (format-money balance)))))

; (defn map-map
;   "Returns new map with f applied to each k-v pair in m. f should be a function
;   that takes args k and v and returns a map-like value such as {new-k new-v}."
;   [f m]
;   (into (empty m) (map #(apply f %) m)) )

(defn total-cost [costs]
  (reduce + (map :paid costs)))

(defn init-persons [costs]
  (into {} (map #(vector % {:share 0.0 :paid 0.0 :balance 0.0})
                (names costs))))

(defn add-paid [persons costs]
  "Insert how much each person paid."
  (reduce (fn [persons cost-row]
            (let [payer (:payer cost-row)
                  paid (:paid cost-row)]
              (update-in persons [payer :paid] + paid)))
          persons
          costs))

(defn add-share [persons costs]
  "Insert how much each person's share of the total expenses."
  (reduce (fn [persons cost-row]
            (let [paid (:paid cost-row)
                  users (:users cost-row)
                  share (/ paid (count users))]
              (reduce (fn [persons user]
                        (update-in persons [user :share] + share))
                      persons
                      users)))
          persons
          costs))

(defn add-balance [persons]
  "Insert how much each person still owes or is owed."
  (reduce (fn [persons [name person]]
            (let [share (:share person)
                  paid (:paid person)
                  balance (- share paid)]
              (update-in persons [name :balance] + balance)))
          persons
          persons))

(defn- unsorted-persons [costs]
  (into {} (-> (init-persons costs)
               (add-paid costs)
               (add-share costs)
               add-balance)))

(defn build-persons [costs]
  "Transform cost-oriented data to person-oriented data.

  e.g. {:alice {:share 100.0 :paid 44.445 :balance 55.555} ...}"
  (let [unsorted (unsorted-persons costs)]
    (into (sorted-map-by (fn [a b]
                           (compare (-> unsorted b :balance)
                                    (-> unsorted a :balance))))
          unsorted)))

(defn format-payment [payer payee amount]
  (format "%s sends %s to %s"
          (name payer)
          (format-money amount)
          (name payee)))

(defn naive-payments [persons]
  "Naive impl that has (n-1) payments. Single payments flow person-to-person
  from person owing most to person owed most. Simple but large payments."
  (loop [remaining persons
         got-paid 0
         payments []]
    (let [[payer {balance :balance}] (first remaining)
          [payee _] (second remaining)
          must-pay (+ got-paid balance)]
      (if-not payee
        payments
        (recur (rest remaining)
               must-pay
               (conj payments (format-payment payer payee must-pay)))))))

(defn minimal-payments [persons]
  "Better impl that has few, smaller payments. Person owing most pays person
  owed most, recursively. Even better would first check if any ower owes same
  exact amount as an owee is owed and resolve that inequality first, or
  implement a knapsack problem solution"
  (loop [remaining persons
         payments []]
    (let [person-has-balance (fn [[_ {v :balance}]] (not (zero? v)))
          remaining (into {} (filter person-has-balance remaining))
          two-left (< 1 (count remaining))]
      (if-not two-left
              payments
              (let [[payer {payer-balance :balance}] (first remaining)
                    [payee {payee-balance :balance}] (last remaining)
                    amount (min payer-balance (* -1 payee-balance))
                    remaining (-> remaining
                                  (update-in [payee :balance]
                                             + amount)
                                  (update-in [payer :balance]
                                             - amount))]
                (recur remaining
                       (conj payments (format-payment payer payee amount))))))))

(defn calculate+print-results [costs]
  (let [persons (build-persons costs)
        total (format-money (total-cost costs))
        payments (minimal-payments persons)]
    (println "Total Cost: " total)
    (println)
    (println "Costs:") (print-costs costs)
    (println)
    (println "Persons:") (print-persons persons)
    (println)
    (println "Payments:") (doseq [x payments] (println x))))

(defn -main [& args]
  (calculate+print-results costs-2014-vegas))
