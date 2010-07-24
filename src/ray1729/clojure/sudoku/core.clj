(ns ray1729.clojure.sudoku.core
  (:use [clojure.contrib.string :only (join)])
  (:require [clojure.contrib.error-kit :as ekit]))

(ekit/deferror *inconsistent-grid* [] []
  {:msg "Inconsistent grid"
   :unhandled (ekit/throw-msg Exception)})
  
(def rows "ABCDEFGHI")
(def cols "123456789")
(def grid-keys (for [r rows c cols] (str r c)))
(def candidates (apply hash-set "123456789"))

(def empty-grid (apply sorted-map (interleave grid-keys (repeat candidates))))

(defn print-grid [grid]
  (letfn [(cell->str [cell] (join "" (map #(or (cell %) \.) candidates)))
	  (row->str  [row]  (join " | " (map #(join " " %) (partition 3 (map cell->str row)))))]
    (let [rows (map row->str (partition 9 (vals grid)))
	  separator "------------------------------+-------------------------------+------------------------------"]
      (doseq [r (apply concat (interpose [separator] (partition 3 rows)))] (println r)))))

(defn row-keys [[r c]] (map #(str r %) cols))

(defn col-keys [[r c]] (map #(str % c) rows))

(defn box-keys [[r c]]
  (let [row-map (into {} (for [ps (partition 3 rows) e ps] [e ps]))
	col-map (into {} (for [ps (partition 3 cols) e ps] [e ps]))]
    (for [row (row-map r) col (col-map c)] (str row col))))

(defn peers [s] (disj (into #{} (concat (row-keys s) (col-keys s) (box-keys s))) s))

(declare assign)
(declare eliminate)

(defn parse-grid [s]
  (let [values (filter #(not (= \0 (val %))) (zipmap grid-keys (map first (re-seq #"\d" s))))]
    (reduce #(assign %1 (key %2) (val %2)) empty-grid values)))

(defn assign [grid cell value]
  (if ((grid cell) value)
    (reduce #(eliminate %1 %2 value) (assoc grid cell #{value}) (peers cell))
    (ekit/raise *inconsistent-grid*)))

(defn eliminate [grid cell value]
  (if (not ((grid cell) value))
    grid
    (let [new-candidates (disj (grid cell) value)
	  num-candidates (count new-candidates)]
      (cond
       (= num-candidates 0) (ekit/raise *inconsistent-grid*)
       (= num-candidates 1) (assign grid cell (first new-candidates))
       :else (assoc grid cell new-candidates)))))

(defn solved? [grid]
  (every? #(= 1 (count %)) (vals grid)))

(defn try-assign [grid cell value]
  (ekit/with-handler
    (assign grid cell value)
    (ekit/handle *inconsistent-grid* [] nil)))

(defn try-eliminate [grid cell value]
  (ekit/with-handler
    (eliminate grid cell value)
    (ekit/handle *inconsistent-grid* [] nil)))

(defn solve [grid]
  (when grid
    (if (solved? grid)
      grid
      (let [cell (first (filter #(> (count (grid %)) 1) (keys grid)))
	    value (first (grid cell))]
	(or (solve (try-assign grid cell value)) (solve (try-eliminate grid cell value)))))))

(comment
  
  ;; This is a trivial puzzle: it is solved by the initial assignment
  (def g1 (parse-grid
	   "003020600
            900305001
            001806400
            008102900
            700000008
            006708200
            002609500
            800203009
            005010300"))

  (solved? g1)
  ;;=> true

  (def g2 (parse-grid "400000805030000000000700000020000060000080400000010000000603070500200000104000000"))

  (print-grid g2)
;; ...4..... 1....67.9 12...67.9 | 1.3.....9 .23..6..9 12...6..9 | .......8. 123.....9 ....5....
;; .2...6789 ..3...... 12..56789 | 1..45..89 .2.456..9 12.456.89 | 12...67.9 12.4....9 12.4.67.9
;; .2...6.89 1...56.89 12..56.89 | ......7.. .23456..9 12.456.89 | 123..6..9 1234....9 1234.6..9
;; ------------------------------+-------------------------------+------------------------------
;; ..3...789 .2....... 1.3.5.789 | ..345...9 ..345.7.9 ...45.7.9 | 1.3.5.7.9 .....6... 1.3...789
;; ..3..67.9 1...567.9 1.3.567.9 | ..3.5...9 .......8. .2..567.9 | ...4..... 123.5...9 123...7.9
;; ..3..6789 ...456789 ..3.56789 | ..345...9 1........ .2.4567.9 | .23.5.7.9 .23.5..89 .23...789
;; ------------------------------+-------------------------------+------------------------------
;; .2.....89 .......89 .2.....89 | .....6... ...45...9 ..3...... | 12..5...9 ......7.. 12.4...89
;; ....5.... .....6789 ..3..6789 | .2....... ...4..7.9 1..4..789 | 1.3..6..9 1.34...89 1.34.6.89
;; 1........ .....6789 ...4..... | ....5..89 ....5.7.9 ....5.789 | .23.56..9 .23.5..89 .23..6.89  
  
  ;; More tricky...
  (solved? g2)
  ;;=> false

  (def s (solve g2))
  (solved? s)
  ;;=> true

  (print-grid s)
;; ...4..... 1........ ......7.. | ..3...... .....6... ........9 | .......8. .2....... ....5....
;; .....6... ..3...... .2....... | 1........ ....5.... .......8. | ........9 ...4..... ......7..
;; ........9 ....5.... .......8. | ......7.. .2....... ...4..... | ..3...... 1........ .....6...
;; ------------------------------+-------------------------------+------------------------------
;; .......8. .2....... ....5.... | ...4..... ..3...... ......7.. | 1........ .....6... ........9
;; ......7.. ........9 1........ | ....5.... .......8. .....6... | ...4..... ..3...... .2.......
;; ..3...... ...4..... .....6... | ........9 1........ .2....... | ......7.. ....5.... .......8.
;; ------------------------------+-------------------------------+------------------------------
;; .2....... .......8. ........9 | .....6... ...4..... ..3...... | ....5.... ......7.. 1........
;; ....5.... ......7.. ..3...... | .2....... ........9 1........ | .....6... .......8. ...4.....
;; 1........ .....6... ...4..... | .......8. ......7.. ....5.... | .2....... ........9 ..3......
  
  (def g3 (parse-grid
	   "008601000
            600000003
            000048506
            040000600
            780020091
            001000030
            109870000
            200000007
            000209100"))

  (print-grid (solve g3))
;; ...4..... ..3...... .......8. | .....6... ....5.... 1........ | .2....... ......7.. ........9
;; .....6... 1........ ....5.... | ......7.. ........9 .2....... | .......8. ...4..... ..3......
;; ........9 .2....... ......7.. | ..3...... ...4..... .......8. | ....5.... 1........ .....6...
;; ------------------------------+-------------------------------+------------------------------
;; ..3...... ...4..... .2....... | ........9 1........ ......7.. | .....6... ....5.... .......8.
;; ......7.. .......8. .....6... | ....5.... .2....... ..3...... | ...4..... ........9 1........
;; ....5.... ........9 1........ | ...4..... .......8. .....6... | ......7.. ..3...... .2.......
;; ------------------------------+-------------------------------+------------------------------
;; 1........ .....6... ........9 | .......8. ......7.. ....5.... | ..3...... .2....... ...4.....
;; .2....... ....5.... ..3...... | 1........ .....6... ...4..... | ........9 .......8. ......7..
;; .......8. ......7.. ...4..... | .2....... ..3...... ........9 | 1........ .....6... ....5....
  
  )