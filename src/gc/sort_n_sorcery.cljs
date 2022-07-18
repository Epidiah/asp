(ns gc.sort-n-sorcery
  (:require [clojure.string :as s]
            [gc.utils.formats :as frmt]))

(def by-year (fn [row] (-> row
                           :year+key
                           frmt/year+key->int
                           -)))

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

