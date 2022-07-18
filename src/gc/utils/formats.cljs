(ns gc.utils.formats
  (:require [clojure.string :as s]))

(defn unescape [escaped-string]
  (-> escaped-string
      (s/replace #"&amp;" "&")))

(defn format-key [words]
  (-> words
      s/lower-case
      (s/replace #" " "-")
      (s/replace #"/" "-or-")
      (s/replace #"&amp;" "and")))

(defn de-tag [tag]
  (s/join " " (s/split (format-key tag) #"-")))

(defonce uncapped-words #{"and" "as" "if" "at" "but" "by" "for" "from" "in" "into" "like" "near" "nor" "of" "off" "on" "once" "onto" "or" "out" "over" "past" "so" "than" "that" "till" "to" "up" "upon" "with" "when" "yet"})

(defn ->title-case [words]
  (let [all-words (s/split words #"-")
        first-word (s/capitalize (first all-words))
        mid-words (map #(if (uncapped-words %)
                           %
                           (s/capitalize %))
                        (butlast (rest all-words)))
        last-word (s/capitalize (last all-words))]
    (s/join " " (list first-word (s/join " " mid-words) last-word))))

(defn year+key->int [yk]
  (-> yk
      ffirst
      name
      js/parseInt))
