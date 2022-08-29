(ns gc.phantasmal-filters
  (:require [gc.state.init :refer [app-state]]))

(defn get-years []
  (filter #(= (second %) :year+key) (:filtering @app-state)))

(defn last-year? []
  (when (= 1 (count (get-years)))
    true))

(defn all-years? []
  (when (= (count (get-years))
           (count (:year-range @app-state)))
    true))

(defn add-filter! [t h]
  (swap! app-state #(update-in % [:filtering] conj [t h])))

(defn remove-filter! [t h]
  (swap! app-state #(update-in % [:filtering] disj [t h])))

(defn build-tag-filters
  "Expects a set of keyword tuples like [:tag :header+key] where
  :header+key is the header under which the :tag can be found.
  Returns a vector of transducers that filters out everything that
  doesn't have ALL the tags."
  []
  (vec (for [[t k] (remove #(= (second %) :year+key) (:filtering @app-state))]
         (filter (comp t k)))))

(defn build-year-filters
  "Expects a set of keyword tuples like [:tag :header+key] where
  :header+key is the header under which the :tag can be found.
  Returns a vector of transducers that filters out everything that
  doesn't have AT LEAST ONE of the tags."
  []
  (as-> (get-years) $ 
    (map first $)
    (set $)
    (comp $ key first :year+key)))

