(ns gc.ui
  (:require [clojure.string :as s]
            [gc.state.init :refer [app-state key-headers year-range]]
            [gc.utils.formats :as frmt]
            [gc.search-for-spock :as stiii]
            [gc.phantasmal-filters :as pf]
            [goog.dom :as gdom]
            [reagent.core :as r]))

(def clr-dark "#1a1a1a")
(def clr-light "#ffffcc")

(defn update-larps [f larps]
  (reduce #(update-in %1 [%2] f) larps (keys larps)))

(defn available-larps []
  (let [omit-years (:missing-years @app-state)
        tag-filtering (pf/filter-by-plus-key)
        search-query (:searching @app-state)
        searching (stiii/search-fn search-query)
        entries (:entries @app-state)]
    (cond->> entries
      (seq omit-years) (pf/filter-by-year omit-years)
      (seq tag-filtering) (update-larps
                            #(eduction (apply comp tag-filtering) %))
      (seq search-query) (update-larps #(filter searching %)))))

(defn last-year? [year]
  (-> (available-larps)
      (dissoc (js/parseInt (name year)))
      vals
      flatten
      empty?))

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
         [:td {:style {:border "none"}} (into [:ul] contents)]
         [:<>
          [:td  {:style {:border "none"}}
           (into [:ul] (take halfway contents))]
          [:td {:style {:border "none"}}
           (into [:ul] (drop halfway contents))]])]]]))

(defn table-of-contents [larps]
  (let [most-recent (frmt/year+key->int (:year+key (first larps)))]
    (into
     [:<>]
     (for [year (partition-by :year+key larps)]
       (let [yr-int (frmt/year+key->int (:year+key (first year)))]
         [:details
          (if (= yr-int most-recent)
            {:open true :id (str yr-int "toc")
             :key (str yr-int "toc") :style {:cursor "pointer"}}
            {:id (str yr-int "toc")
             :key (str yr-int "toc") :style {:cursor "pointer"}})
          [:summary
           [:h2 (str yr-int " — " (count year) " Game"
                     (when-not (= 1 (count year)) "s"))]]
          [toc-by-year year]])))))

(defn key-counts [larps]
  (zipmap (key-headers) (for [k-h (key-headers)]
                          (frequencies (mapcat (comp keys k-h) larps)))))

(defn filtered? [k k-header]
  (if (= :year+key k-header)
    ((:missing-years @app-state) (js/parseInt (name k)))
    ((:filtering @app-state) [k k-header])))

(defn filter-toggle [k k-header]
  (if (and (= k-header :year+key)
           (last-year? k))
      (fn []  nil)
      (fn [] (if (filtered? k k-header)
             (pf/remove-filter! k k-header)
             (pf/add-filter! k k-header)))
      ))

(defn key-buttons
  ([key-header coll+key]
   [key-buttons key-header coll+key nil])
  ([key-header coll+key key-count]
   (into
    [:<>]
    (for [[k v] coll+key]
      [:input
       {:type "button"
        :value (if (= key-header :year+key)
                 v
                 (str v (when-let [quantity (k key-count)]
                          (str " (" quantity ")"))))
        :style (into
                {}
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
                   "Filter out Years") ": "]
         (key-buttons keyword-header
                      (if (= :year+key keyword-header)
                        (map #(vector (keyword (str %)) (str %))
                             (year-range))
                        (take @visible ks)) k-count)
         (if-not (= :year+key keyword-header)
           (if (< @visible (count ks))
             [:input
              {:type "button"
               :value (str "+ " (count (drop @visible ks))
                           " more "
                           (keyword-header headers))
               :style {:margin "0.1em 0.1em"
                       :background-color  clr-dark
                       :color clr-light}
               :class "extend-filter"
               :on-click #(reset! visible (count ks))}]
             (when (> (count ks) 5)
               [:input
                {:type "button"
                 :value (str "Less "
                             (keyword-header headers))
                 :style {:margin "0.1em 0.1em"
                         :background-color  clr-dark
                         :color clr-light}
                 :class "extend-filter"
                 :on-click #(reset! visible 5)}]))
           (when (seq (:missing-years @app-state))
             [:input
              {:type "button"
               :value (str "Show All Years")
               :style {:margin "0.1em 0.1em"
                       :background-color  clr-dark
                       :color clr-light}
               :on-click #(doseq [yr (year-range)]
                            (pf/remove-filter! (str yr) :year+key))}]))]))))

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

(defn contents [year larps]
  (into
   [:div {:key (str year "listing") :id "contents"}]
   (for [row larps]
     [:div {:key (:anchor+url row)}
      [:h2 {:id (:anchor+url row) :key (str (:anchor+url row) "header")}
       (larp-title row)]
      [:h3 {:key (str (:anchor+url row) "byline")}
       "by " (mark-text (:designers row))]
      (when (seq (:not-eligible row))
        [:p {:key (str (:anchor+url row) (name :not-eligible))}
         "(Not Eligible for Awards)"])
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
  (fn [larps]
    [:div [:label {:for "search"
                   :style {:margin-right "0.3em"}} "Filter by Search: "]
     [:input {:type "text" :id "search" :name "search"
              :on-input #(let [q (.. % -target -value)]
                           (if (stiii/fruitful-search? q larps)
                             (do
                               (stiii/add-search! q)
                               (stiii/change-match! false))
                             (do
                               (stiii/add-search! "")
                               (stiii/change-match! true))))}]
     [:input {:type "button"
              :value "Clear Search"
              :style {:margin "0.1em 0.1em"
                      :background-color  clr-dark
                      :color clr-light}
              :class "search-clear"
              :on-click #(stiii/clear-search! (gdom/getElement "search"))}]
     (when (:no-match @app-state)
       [:span {:style {:color "#a44"}} " No Matches Found"])]))

(defn showcase
  ([larps] (showcase larps 5))
  ([larps quantity]
   [:details {:id "showcase" :style {:cursor "pointer"}}
    [:summary [:h2 "Showcase — " (min quantity (count larps)) " Random Games"]]
    (toc-by-year (take quantity (shuffle larps)))]))

(defn header [larps]
  [:div
   [:h1 "Interactive Golden Cobra Archive [BETA]"]
   [search-bar larps]
   [filter-by-keys larps :year+key]
   [filter-by-keys larps :styles-of-play+key]
   [filter-by-keys larps :tags+key]
   [:input {:type "button"
            :value "Clear All Filters"
            :style {:margin "1em 0em"
                    :background-color  clr-dark
                    :color clr-light}
            :on-click #(do
                         (pf/clear-filters! app-state)
                         (stiii/clear-search! (gdom/getElement "search")))}]])

(defn assembled-page []
  (let [larps (available-larps)
     flarps (flatten (vals larps))]
    [:div
     [header flarps]
     (when (> (count flarps) 5) [showcase flarps])
     [table-of-contents flarps]
     (into [:<>] (for [[yr llist] larps] (contents yr llist)))]))
