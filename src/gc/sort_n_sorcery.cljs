(ns gc.sort-n-sorcery
  (:require [clojure.string :as s]
            [gc.utils.formats :as frmt]))

(defn sort-by-year
  "Sorts entries by year with most recent year first."
  [row]
  (-> row
      :year+key
      frmt/year+key->int
      -))

(defn sort-by-title
  "Sorts entries in alphabetical order by title, ignoring the
  articles (a/the) at the front of those titles"
  [row]
  (let [title (s/lower-case (:title row))]
    (cond-> (:title row)
      (s/starts-with? title "\"") (subs 1)
      (s/starts-with? title "'") (subs 1)
      (s/starts-with? title "『") (subs 1)
      (s/starts-with? title "the ") (subs 4)
      (s/starts-with? title "a ") (subs 2))))

(defn default-sort 
  "Sorts by year, most recent first, and then alphabetically by title."
  [row]
  (sort-by (juxt
             sort-by-year
             sort-by-title)
           row))

