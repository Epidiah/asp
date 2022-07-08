(ns gc.init.sort-n-sorcery
  (:require [clojure.string :as s]))

(def default-sort (comp #(s/lower-case (:title %)) :year))
