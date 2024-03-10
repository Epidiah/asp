(ns asp.utils
  (:require
   [clojure.string :as str]))

(defn url-str?
  "Returns true if `s`tring can represents an address. Otherwise false.
  Very forgiving at the moment."
  [s]
  (let [low-s (str/lower-case s)]
    (or (str/starts-with? low-s "http")
        (str/starts-with? low-s "www."))))

(defn sortable-title
  "Produces a title for the row in lower-case and without the articles
  (a/the)or extraneous punctuation at the front.
  Suitable for alphabetical sorting.
  I'm afraid you're going to have to manually add to this list
  as more titles come in."
  [base-title]
  (loop [title (str/lower-case (str base-title))]
    (cond
      (str/starts-with? title "#")    (recur (subs title 1))
      (str/starts-with? title "(")    (recur (subs title 1))
      (str/starts-with? title "[")    (recur (subs title 1))
      (str/starts-with? title "\"")   (recur (subs title 1))
      (str/starts-with? title "'")    (recur (subs title 1))
      (str/starts-with? title "『")   (recur (subs title 1))
      (str/starts-with? title "the ") (recur (subs title 4))
      (str/starts-with? title "a ")   (recur (subs title 2))
      :else                           title)))
