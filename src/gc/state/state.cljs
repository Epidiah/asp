(ns gc.state.state
  (:require [reagent.core :as r]
            [reagent.ratom :as ratom]
            [gc.state.init :as init]
            [clojure.set :as set]
            [clojure.string :as s]))

(defonce app-state (r/atom {:state/filtering       #{}
                            :state/query           ""
                            :state/no-match        false ; remove
                            :state/missing-years   #{} ; remove
                            :state/visible-years   (set (keys init/entries))
                            :state/entries         init/entries
                            :state/results         init/entries ;remove
                            :state/visible-entries #{}
                            }))

(defonce query (r/cursor app-state [:state/query]))
(defonce filtering (r/cursor app-state [:state/filtering]))
(defonce no-match (r/cursor app-state [:state/no-match])) ; remove
(defonce missing-years (r/cursor app-state [:state/missing-years])) ; remove
(defonce entries (r/cursor app-state [:state/entries]))
(defonce results (r/cursor app-state [:state/results])) ; remove
(defonce visible-years (r/cursor app-state [:state/visible-years]))

(defonce visible-entries
  (ratom/reaction
   (let [invisible (apply
                    comp
                    (for [[t k] (remove (comp #{:year+key} second) @filtering)]
                      (filter (comp t k))))]
     nil)))

;; Remove
(defonce flat-results (ratom/reaction
                       (flatten (vals @results))))

(defonce flat-entries (ratom/reaction
                       (flatten (vals @entries))))

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
                           (filter #(s/ends-with? (name %) "+key"))
                           (into #{}))))

(defonce url-headers (ratom/reaction
                      (->> @all-headers
                           (filter #(s/ends-with? (name %) "+url"))
                           (into #{}))))

(defonce txt-headers (ratom/reaction
                      (set/difference @all-headers @key-headers @url-headers)))
