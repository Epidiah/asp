(ns ^:figwheel-hooks gc.asp
  (:require
   [gc.ui :refer [assembled-page]]
   [goog.dom :as gdom]
   [reagent.dom :as rdom]))

(defn get-asp-element []
  (gdom/getElement "asp"))

(defn mount [el]
  (rdom/render [assembled-page] el))

(defn mount-asp-element []
  (when-let [el (get-asp-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-asp-element)

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (mount-asp-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
