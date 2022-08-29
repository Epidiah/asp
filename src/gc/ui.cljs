(ns gc.ui
  (:require [clojure.string :as s]
            [gc.state.init :refer [app-state key-headers]]
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
    [:table {:style {:border-collapse "separate" :border-radius "13px" :border "1px solid #c0c0c0"}}
     [:tbody
      [:tr
       (if (> 5 halfway)
         [:td {:style {:border "none"}} (into [:ul] contents)]
         [:<> [:td  {:style {:border "none"}} (into [:ul] (take halfway contents))]
          [:td {:style {:border "none"}} (into [:ul] (drop halfway contents))]])]]]))

(defn table-of-contents [larps]
  (let [most-recent (frmt/year+key->int (:year+key (first larps)))]
    (into [:<>] (for [year (partition-by :year+key larps)]
                  (let [yr-int (frmt/year+key->int (:year+key (first year)))]
                    [:details (if (= yr-int most-recent) {:open true :id (str yr-int "toc") :style {:cursor "pointer"}} {:id (str yr-int "toc") :style {:cursor "pointer"}})
                     [:summary [:h2 (str  yr-int " — " (count year) " Games")]]
                     [toc-by-year year]])))))

(defn key-counts [larps]
  (zipmap key-headers (for [k-h key-headers]
                        (frequencies (mapcat (comp keys k-h) larps)))))

(defn filtered? [k k-header]
  ((:filtering @app-state) [k k-header]))

(defn filter-toggle [k k-header]
  (fn [] (if (filtered? k k-header)
           (when-not (and (= k-header :year+key)
                          (pf/last-year?))
             (pf/remove-filter! k k-header))
           (pf/add-filter! k k-header))))

(defn key-buttons
  ([key-header coll+key]
   [key-buttons key-header coll+key nil])
  ([key-header coll+key key-count]
   (into [:<>] (for [[k v] coll+key]
                 [:input {:type "button"
                          :value (if (= key-header :year+key)
                                   v
                                   (str v
                                        (when-let [quantity (k key-count)]
                                          (str " (" quantity ")"))))
                          :style (into {}
                                       (if (filtered? k key-header)
                                         {:background-color clr-dark :color clr-light}
                                         {:background-color  clr-light :color clr-dark}))
                          :class [(name key-header) "asp-btn"]
                          :key (name k)
                          :on-click (filter-toggle k key-header)}]))))

(defn filter-by-keys [larps keyword-header]
  (let [visible (r/atom 5)]
    (fn [larps keyword-header]
      (let [k-count (keyword-header (key-counts larps))
            sort-fn (if (= keyword-header :year+key)
                      #(- (js/parseInt (second %)))
                      #(- ((first %) k-count)))
            ks (sort-by sort-fn (apply merge (map keyword-header larps)))
            headers (apply merge (map :headers larps))]
        [:div [:hr]
         [:label (if-not (= :year+key keyword-header)
                   (str "Filter by " (keyword-header headers))
                   "Selected Years") ": "]
         (key-buttons keyword-header (if (= :year+key keyword-header)
                                       (sort-by
                                         (comp - #(js/parseInt %) second)
                                         (:year-range @app-state))
                                       (take @visible ks)) k-count)
         (if-not (= :year+key keyword-header)
           (if (< @visible (count ks))
             [:input {:type "button"
                      :value (str "+ " (count (drop @visible ks))
                                  " more "
                                  (keyword-header headers))
                      :style {:margin "0.1em 0.1em"
                              :background-color  clr-dark
                              :color clr-light}
                      :class "extend-filter"
                      :on-click #(reset! visible (count ks))}]
             (when (> (count ks) 5)
               [:input {:type "button"
                        :value (str "Less "
                                    (keyword-header headers))
                        :style {:margin "0.1em 0.1em"
                                :background-color  clr-dark
                                :color clr-light}
                        :class "extend-filter"
                        :on-click #(reset! visible 5)}]))
           (when-not (pf/all-years?)
             [:input {:type "button"
                      :value (str "Select All")
                      :style {:margin "0.1em 0.1em"
                              :background-color  clr-light
                              :color clr-dark}
                      :on-click #(doseq [[yr _] (:year-range @app-state)]
                                   (pf/add-filter! yr :year+key))}]))]))))

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
   (when (seq (k row)) 
     [:p {:key (str (:anchor+url row) (name k))} [:strong (str head ": ")]
    (cond
      (= k :year+key)
      [:a {:href (str "#" (val (first (k row))) "toc")} (val (first (k row)))]
      (s/ends-with? (name k) "+url")
      (into [:<>] (interpose " + " (for [url (k row)] [:a {:href url} url])))
      (s/ends-with? (name k) "+key")
      [:a {:href (str "#" (:anchor+url row))} (key-buttons k (k row))]
      :else (mark-text (k row)))])))

(defn larp-title [row]
  (if (seq (:link+url row))
    [:a {:href (:link+url row)} (mark-text (:title row))]
    (mark-text (:title row))))

(defn contents [larps]
  (into [:div] (for [row larps]
                 [:div {:key (:anchor+url row)}
                  [:h2 {:id (:anchor+url row)} (larp-title row)]
                  [:h3 "by " (mark-text (:designers row))]
                  (when (seq (:not-eligible row))
                    [:p "(Not Eligible for Awards)"])
                  (when (seq (:designers-work+url row))
                    (display-info :designers-work+url row "Other Work"))
                  (when (seq (:styles-of-play+key row))
                    (display-info :styles-of-play+key row))
                  (display-info :description row)
                  (when (seq (:tags+key row))
                    (display-info :tags+key row))
                  (when (seq (:year+key row))
                    (display-info :year+key row "Year Submitted"))
                  (let [accounted-for [:anchor+url :title :designers :designers-work+url
                                       :tags+key :styles-of-play+key :description
                                       :not-eligible :link+url :year+key]]
                    (doall
                     (for [k (keys (apply dissoc (:headers row) accounted-for))]
                       (display-info k row))))])))

(defn search-bar [larps]
  (let [no-match (r/atom false)]
    (fn [larps]
      [:div [:label {:for "search"
                     :style {:margin-right "0.3em"}} "Filter by Search: "]
       [:input {:type "text" :id "search" :name "search"
                :on-input #(let [q (.. % -target -value)]
                             (if (stiii/fruitful-search? q larps)
                               (do
                                 (stiii/add-search! q)
                                 (reset! no-match false))
                               (do
                                 (stiii/add-search! "")
                                 (reset! no-match true))))}]
       [:input {:type "button"
                :value "Clear Search"
                :style {:margin "0.1em 0.1em"
                        :background-color  clr-dark
                        :color clr-light}
                :class "search-clear"
                :on-click #(do
                             (stiii/add-search! "")
                             (reset! no-match false)
                             (set!
                              (.. (gdom/getElement "search") -value) ""))}]
       (when @no-match [:span {:style {:color "#a44"}} " No Matches Found"])])))

(defn showcase 
  ([larps] (showcase larps 5))
  ([larps quantity]
   [:details {:id "showcase" :style {:cursor "pointer"}}
    [:summary [:h2 "Showcase — " (min quantity (count larps)) " Random Games"]]
    (toc-by-year (take quantity (shuffle larps)))]))

(defn header [larps]
  [:div
   [:h1 "Interactive Golden Cobra Archive"]
   [search-bar larps]
   [filter-by-keys larps :styles-of-play+key]
   [filter-by-keys larps :tags+key]
   [filter-by-keys larps :year+key]])

(defn assembled-page []
  (let [sorting (:sorting @app-state)
        year-filtering (pf/build-year-filters)
        tag-filtering (pf/build-tag-filters)
        searching (stiii/search-fn (:searching @app-state))
        entries (:entries @app-state)
        larps (->> entries
                   (filter year-filtering)
                   (eduction (apply comp tag-filtering))
                   (filter #(searching %))
                   sorting)]
    (if (seq larps)
      [:div
       [header larps]
       (when (> (count larps) 5) [showcase larps])
       [table-of-contents larps]
       [contents larps]]
      (doseq [[yr _] (:year-range @app-state)]
                                   (pf/add-filter! yr :year+key)))
    ))
