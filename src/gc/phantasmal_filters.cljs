(ns gc.phantasmal-filters
  (:require [gc.init.state :refer [app-state]]))


(defn add-filter! [t h]
  (swap! app-state #(update-in % [:filtering] conj [t h])))

(defn remove-filter! [t h]
  (swap! app-state #(update-in % [:filtering] disj [t h])))

(defn build-filters
  "Expects a set of keyword tuples like [:tag :k+key] where :k+key is the
  header+key underwhich the :tag can be found."
  [filter-set]
  (vec (for [[t k] filter-set] (filter (comp t k)))))
