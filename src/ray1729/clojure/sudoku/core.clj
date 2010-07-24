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

(defn units [s] (vector (row-keys s) (col-keys s) (box-keys s)))

(defn peers [s] (disj (into #{} (apply concat (units s))) s))

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

(defn solve [grid]
  (if (solved? grid)
    grid
    (let [cell (first (filter #(> (count (grid %)) 1) (keys grid)))
	  candidates (grid cell)
	  candidate (first candidates)]
      (ekit/with-handler
	(do
	  (println (str "Trying " cell "=" candidate))
	  (recur (assign grid cell candidate)))
	(ekit/handle *inconsistent-grid* []		     
		     (if (next candidates)
		       (do (println (str "Trying next candidate for " cell))
			   (recur (eliminate grid cell candidate)))
		       (ekit/do-not-handle)))))))

(comment
  
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

  (def g2 (parse-grid "400000805030000000000700000020000060000080400000010000000603070500200000104000000"))
  )