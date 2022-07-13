(ns ^:figwheel-hooks gc.asp
  (:require
    [gc.init.larps :refer [entries]]
    [gc.ui :refer [assembled-page]]
    [gc.sort-n-sorcery :as sns]
    [goog.dom :as gdom]
    [reagent.core :as reagent :refer [atom]]
    [reagent.dom :as rdom]))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:filtering #{}
                          :sorting sns/default-sort
                          :entries entries}))

(defn get-asp-element []
  (gdom/getElement "asp"))

(defn add-filter! [t h]
  (swap! app-state #(update-in % [:filtering] conj [t h])))

; (defn add-filter! [fltr]
;   (swap! app-state #(update-in % [:filtering] conj (filter fltr))))

; (add-filter! (comp :larp :styles-of-play+key))
; (add-filter! (comp :sci-fi :tags+key))

(defn mount [el]
  (rdom/render [assembled-page app-state] el))

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
