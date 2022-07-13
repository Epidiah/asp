(ns gc.sort-n-sorcery
  (:require [clojure.string :as s]))

(def by-year (fn [row] (first (:year+key row))))

(defn by-title
  [row]
  (let [title (s/lower-case (:title row))]
    (condp #(s/starts-with? %2 %1) title
      "the " (subs title 4)
      "a " (subs title 2)
      title)))


(def default-sort #(sort-by (juxt
                              by-year
                              by-title)
                            %))

(defn build-filters
  "Expects a set of keyword tuples like [:tag :k+key] where :k+key is the
  header+key underwhich the :tag can be found."
  [filter-set]
  (vec (for [[t k] filter-set] (filter (comp t k)))))
