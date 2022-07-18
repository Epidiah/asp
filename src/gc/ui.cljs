(ns gc.ui
  (:require [clojure.string :as s]
            [gc.init.state :refer [app-state key-headers]]
            [gc.utils.formats :as frmt]
            [gc.search-for-spock :as stiii]
            [gc.phantasmal-filters :as pf]
            [goog.dom :as gdom]
            [reagent.core :as r]))

(def clr-dark "#1a1a1a")
(def clr-light "#ffffcc")

(defn toc-by-year [larps]
  (let [halfway (quot (count larps) 2)
        contents (for [row larps]
                   [:li {:key (str "toc" (:anchor+url row))}
                    [:a {:href (str "#" (:anchor+url row))}
                     (:title row) " by " (:designers row)]])]
    [:table
     [:tbody
      [:tr
       (if (> 5 halfway)
         (into [:td] contents)
         [:<> (into [:td] (take halfway contents))
             (into [:td] (drop halfway contents))])]]]))

(defn table-of-contents [larps]
  (let [most-recent (frmt/year+key->int (:year+key (first larps)))]
    (into [:<>] (for [year (partition-by :year+key larps)]
     (let [yr-int (frmt/year+key->int (:year+key (first year)))]
       [:details (if (= yr-int most-recent) {:open true :id (str yr-int "toc")} {:id (str yr-int "toc")})
        [:summary [:h2 (str  yr-int " — " (count year) " Entries")]]
      [toc-by-year year]])))))

(defn key-counts [larps]
  (zipmap key-headers (for [k-h key-headers]
    (frequencies (mapcat (comp keys k-h) larps)))))

(defn filtered? [k k-header]
  ((:filtering @app-state) [k k-header]))

(defn filter-toggle [k k-header]
  (fn [] (if (filtered? k k-header)
           (pf/remove-filter! k k-header)
           (pf/add-filter! k k-header))))

(defn key-buttons
  ([key-header coll+key]
   [key-buttons key-header coll+key nil])
  ([key-header coll+key c]
   (into [:<>] (for [[k v] coll+key]
                 [:input {:type "button"
                          :value (str v (when c (str "(" c ")")))
                          :style (if (filtered? k key-header)
                                   {:background-color clr-dark :color clr-light}
                                   {:background-color  clr-light :color clr-dark })
                          :class (name key-header)
                          :key (name k)
                          :on-click (filter-toggle k key-header)}]))))
 
(defn mark-text 
  ([text] (let [searching (s/trim (:searching @app-state))]
            (if (seq searching)
              (mark-text text (stiii/text-matches searching text))
              text)))
  ([text found-list]
   (if (or (empty? found-list)
           (empty? text))
     text    
     (let [found (first found-list)
           [head tail] (s/split text (stiii/plain-text->re found) 2)]
       [:<> head [:mark {:style {:padding "0"
                                 :background-color clr-dark
                                 :color clr-light}} found]
        (mark-text tail (rest found-list))]))))

(defn display-info
  ([k row] (display-info k row (k (:headers row))))
  ([k row head]
   [:p {:key (str (:anchor+url row) (name k))} [:strong (str head ": ")]
    (cond
      (s/ends-with? (name k) "+url")
        (into [:<>] (interpose " + " (for [url (k row)] [:a {:href url} url]))) 
      (s/ends-with? (name k) "+key")
        [:a {:href (str "#" (:anchor+url row))} (key-buttons k (k row))]
      :else (mark-text (k row)))]))

(defn contents [larps]
  (into [:div] (for [row larps]
    [:div {:key (:anchor+url row)}
     [:h2 [:a {:name (:anchor+url row) :href ""} (mark-text (:title row))]]
     [:h3 "by " (mark-text (:designers row))]
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
       (doall
         (for [k (keys (apply dissoc (:headers row) accounted-for))]
         (display-info k row))))
     ])))

(defn radio [label r-name value]
  [:<>
   [:label
    [:input {:type :radio
             :id (str "radio-" r-name value)
             :name r-name
             :value value
             :style {:margin-left "1em"}}]
    label]])

(defn search-bar [larps]
  (let [no-match (r/atom false)]
    (fn [larps]
      [:div [:label {:for "search"} "Search: "]
       [:input {:type "text" :id "search" :name "search"
                :on-input #(let [q (.. % -target -value)]
                             (if (stiii/fruitful-search? q larps)
                               (do 
                                 (stiii/add-search! q)
                                 (reset! no-match false))
                               (do
                                 (stiii/add-search! "")
                                 (reset! no-match true))))}]
       (when @no-match [:span {:style {:color "#a44"}} " No Matches Found"])
       (radio " Match any of the terms" "search-type" "any")
       (radio " Match all of the terms" "search-type" "all")])))

(defn header [larps]
  [:div
   [:h1 "Golden Cobra Submissions"]
   [search-bar larps]])

(defn assembled-page []
  (let [sorting (:sorting @app-state)
        filtering (pf/build-filters (:filtering @app-state))
        searching (stiii/search-fn (:searching @app-state))
        entries (:entries @app-state)
        larps (sorting
                (filter #(searching %) (eduction
                             (apply comp filtering)
                             entries)))]
    [:div
     [header larps]
     [table-of-contents larps]
     [contents larps]]))
