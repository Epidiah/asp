(ns gc.state.state
  (:require [reagent.core :as r]
            [reagent.ratom :as ratom]
            [gc.state.init :as init]
            [gc.state.load-larps :as load]
            [clojure.set :as set]
            [clojure.string :as str]))

(defonce app-state (r/atom {:state/entries         {} #_init/entries
                            :state/filters         #{}
                            :state/query           ""
                            :state/visible-years   (set (keys init/entries))}))

(load/initialize-entries! load/csv->map app-state [:state/entries])

;; Should only change while loading larps
(defonce entries (r/cursor app-state [:state/entries]))
(defonce query (r/cursor app-state [:state/query]))
(defonce filters (r/cursor app-state [:state/filters]))
(defonce visible-years (r/cursor app-state [:state/visible-years]))

(defonce all-years (ratom/reaction
                    (sort (keys @entries))))

(defonce all-headers (ratom/reaction
                      (disj (->> @entries
                                 vals
                                 flatten
                                 (mapcat keys)
                                 (into #{})) :headers)))

(defonce key-headers (ratom/reaction
                      (->> @all-headers
                           (filter #(str/ends-with? (name %) "+key"))
                           (into #{}))))

(defonce url-headers (ratom/reaction
                      (->> @all-headers
                           (filter #(str/ends-with? (name %) "+url"))
                           (into #{}))))

(defonce txt-headers (ratom/reaction
                      (set/difference @all-headers @key-headers @url-headers)))

#_(add-watch app-state :logger (fn [_ _ _ new-state]
                                 (println "New state: " new-state)))
