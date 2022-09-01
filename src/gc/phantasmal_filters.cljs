(ns gc.phantasmal-filters
  (:require [gc.state.init :refer [app-state]]))

(defn add-filter! [t h]
  (if (= h :year+key)
    (swap! app-state #(update-in % [:missing-years] conj (js/parseInt (name t))))
    (swap! app-state #(update-in % [:filtering] conj [t h]))))

(defn remove-filter! [t h]
  (if (= h :year+key)
   (swap! app-state #(update-in % [:missing-years] disj (js/parseInt (name t))))
    (swap! app-state #(update-in % [:filtering] disj [t h]))))

(defn clear-filters! [state]
  (doseq [filt [:filtering :missing-years]]
   (swap! state assoc filt #{})))

(defn filter-by-plus-key
  []
  (vec (for [[t k] (remove #(= (second %) :year+key) (:filtering @app-state))]
         (filter (comp t k)))))

(defn filter-by-year
  [omitted-years entries]
  (apply dissoc entries omitted-years))

