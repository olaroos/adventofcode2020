(use 'clojure.java.io)
(require ['clojure.string :as 'str])

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(defn get-lines-to-int [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(defn parse-int [s]
  (Integer. (re-find #"\d+" s)))

(defn day1!
  []
  (let [numbers (vec (map parse-int (get-lines (file "resources/day1"))))]
    (reduce (fn [accu val]
              (let [twin (first (filter #(= (+ val %) 2020) numbers))]
                (if (not (nil? twin))
                  (concat accu [val twin])
                  accu))) [] numbers)))

(defn day2!
  []
  (let [input (get-lines (file "resources/day2"))
        fours (map (fn [line]
                     (-> (str/replace line ":" "")
                         (str/replace "-" " ")
                         (str/split #" "))) input)
        parsed-fours (map (fn [line]
                            (let [lower (parse-int (first line))
                                  upper (parse-int (second line))
                                  third (line 2)
                                  fourth (line 3)]
                              [lower upper third fourth])) fours)
        bools (map (fn [line]
                     (let [lower (line 0)
                           upper (line 1)
                           char (line 2)
                           password (line 3)
                           char-list (filter #(= char (str %)) password)
                           number (count char-list)]
                       (and (<= lower number)
                            (>= upper number)))
                     ) parsed-fours)
        n-true (count (filter #(true? %) bools))]
          n-true))

(day2!)




