(ns gc.ui
  (:require [clojure.string :as s]
            [gc.sort-n-sorcery :as sns]
            [reagent.core :as reagent]))

(defn table-of-contents [larps]
  (let [halfway (quot (count larps) 2)
        contents (for [row larps]
                   [:li {:key (str "toc" (:anchor+url row))}
                    [:a {:href (str "#" (:anchor+url row))}
                     (:title row) " by " (:designers row)]])]
     [:table
      [:tbody
       [:tr
       (into [:td] (take halfway contents))
       (into [:td] (drop halfway contents))]]]))

(defn key-buttons [key-type coll+key]
  (for [[k v] coll+key]
    [:button {:class (name key-type) :key (name k)} v]))

(defn display-info
  ([k row] (display-info k row (k (:headers row))))
  ([k row head]
  [:p [:strong (str head ": ")]
   (cond
    (s/ends-with? (name k) "+url") [:a {:href (k row)} (k row)] 
    (s/ends-with? (name k) "+key") (key-buttons k (k row))
    :else (k row))]))

(defn contents [larps]
  (into [:div] (for [row larps]
    [:div {:key (:anchor+url row)}
     [:h2 [:a {:name (:anchor+url row) :href ""} (:title row)]]
     [:h3 "by " (:designers row)]
     (when (seq (:designers-work+url row))
       (display-info :designers-work+url row "Other Work"))
     (when (seq (:styles-of-play+key row))
       (display-info :styles-of-play+key row))
     (display-info :description row)
     (when (seq (:tags+key row))
       (display-info :tags+key row))
     (let [accounted-for [:anchor+url :title :designers :designers-work+url
                          :tags+key :styles-of-play+key :description
                          :year+key]]
       (for [k (keys (apply dissoc (:headers row) accounted-for))]
         (display-info k row)))
     ])))

(defn header [larps]
  [:div
   [:h1 "Golden Cobra Submissions"]])

(defn assembled-page [state]
  (let [sorting (:sorting @state)
        filtering (sns/build-filters (:filtering @state))
        entries (:entries @state)
        larps (sorting
                (eduction
                  (apply comp filtering)
                  entries))]
    [:div
     [header larps]
     [table-of-contents larps]
     [contents larps]]))
