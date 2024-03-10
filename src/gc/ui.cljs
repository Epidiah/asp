(ns gc.ui
  (:require [clojure.string :as s]
            [gc.state.state :as state]
            [gc.utils.formats :as frmt]
            [gc.sieve :as sieve]
            [goog.dom :as gdom]
            [goog.functions :as gfun]
            [reagent.core :as r]))

(def clr-dark "#1a1a1a")
(def clr-light "#ffffcc")

(defn last-year? [year]
  (-> @state/results
      (dissoc (js/parseInt (name year)))
      vals
      flatten
      empty?))

(defn toc-by-year [larps]
  (let [halfway (quot (count larps) 2)
        contents (doall
                  (for [row larps]
                    [:li {:key (str "toc" (:anchor+url row))}
                     [:a {:href (str "#" (:anchor+url row))}
                      (:title row) " by " (:designers row)]]))]
    [:table [:tbody
             [:tr
              (if (> 5 halfway)
                [:td {:style {:border "none"}} (into [:ul] contents)]
                [:<> [:td  {:style {:border "none"}} (into [:ul] (take halfway contents))]
                 [:td {:style {:border "none"}} (into [:ul] (drop halfway contents))]])]]]))

(defn table-of-contents []
  (let [larps @state/flat-entries]
    (when (seq larps)
      (let [most-recent (frmt/year+key->int (:year+key (first larps)))]
        [:<>
         (doall
          (for [year (partition-by :year+key larps)]
            (let [yr-int (frmt/year+key->int (:year+key (first year)))]
              [:details
               {:id    (str yr-int "toc")
                :key   (str yr-int "toc")
                :open  (when (= yr-int most-recent) true)
                :hidden (not (@state/visible-years yr-int))
                :style {:cursor "pointer"}}
               [:summary
                [:h2 (str yr-int " — " (count year) " Game"
                          (when-not (= 1 (count year)) "s"))]]
               [toc-by-year year]])))]))))

(defn key-counts [larps]
  (zipmap @state/key-headers
          (for [k-h @state/key-headers]
            (frequencies (mapcat (comp keys k-h) larps)))))

(defn filtered? [k k-header]
  (if (= :year+key k-header)
    (not (@state/visible-years (js/parseInt (name k))))
    (@state/filtering [k k-header])))

(defn filter-toggle [k k-header]
  (if (and (= k-header :year+key)
           (last-year? k))
    (fn []  nil)
    (fn [] (if (filtered? k k-header)
             (sieve/remove-filter! k k-header)
             (sieve/add-filter! k k-header))
      (sieve/update-sieve!))
    ))

(defn key-buttons
  ([key-header coll+key]
   [key-buttons key-header coll+key nil])
  ([key-header coll+key key-count]
   [:<>
    (doall
     (for [[k v] coll+key]
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
                :on-click (filter-toggle k key-header)}]))]))

(defn filter-by-keys [keyword-header]
  (r/with-let [visible (r/atom 5)]
    (let [larps   @state/flat-results
          k-count (keyword-header (key-counts larps))
          sort-fn (if (= keyword-header :year+key)
                    #(- (js/parseInt (second %)))
                    #(- ((first %) k-count)))
          ks      (sort-by sort-fn (apply merge (map keyword-header larps)))
          headers (apply merge (map :headers larps))]
      [:div [:hr]
       [:label (if-not (= :year+key keyword-header)
                 (str "Filter by " (keyword-header headers))
                 "Filter out Years") ": "]
       [key-buttons keyword-header
        (if (= :year+key keyword-header)
          (map #(vector (keyword (str %)) (str %))
               @state/all-years)
          (take @visible ks)) k-count]
       (if-not (= :year+key keyword-header)
         (if (< @visible (count ks))
           [:input {:type     "button"
                    :value    (str "+ " (count (drop @visible ks))
                                   " more "
                                   (keyword-header headers))
                    :style    {:margin           "0.1em 0.1em"
                               :background-color clr-dark
                               :color            clr-light}
                    :class    "extend-filter"
                    :on-click #(reset! visible (count ks))}]
           (when (> (count ks) 5)
             [:input {:type     "button"
                      :value    (str "Less "
                                     (keyword-header headers))
                      :style    {:margin           "0.1em 0.1em"
                                 :background-color clr-dark
                                 :color            clr-light}
                      :class    "extend-filter"
                      :on-click #(reset! visible 5)}]))
         [:input {:type     "button"
                  :hidden   (every? @state/visible-years @state/all-years)
                  :value    "Show All Years"
                  :style    {:margin           "0.1em 0.1em"
                             :background-color clr-dark
                             :color            clr-light}
                  :on-click (fn []
                              (doseq [yr @state/all-years]
                                (sieve/remove-filter!
                                 (str yr) :year+key)))}])])))

(defn mark-text
  ([text] (let [searching (s/trim @state/query)]
            (if (seq searching)
              (mark-text text (sieve/text-matches searching text))
              text)))
  ([text found-list]
   (if (or (empty? found-list)
           (empty? text))
     text
     (let [found (first found-list)
           [head tail] (s/split text (sieve/plain-text->re found) 2)]
       [:<> head [:mark {:style {:padding "0"
                                 :background-color clr-dark
                                 :color clr-light}} found]
        (mark-text tail (rest found-list))]))))

(defn display-info
  ([k row] (display-info k row (k (:headers row))))
  ([k row head]
   [:<> {:key (str (:anchor+url row) (name k))}
    (when (seq (k row))
      [:p [:strong (str head ": ")]
       (cond
         (= k :year+key)
         [:a {:href (str "#" (val (first (k row))) "toc")} (val (first (k row)))]
         (s/ends-with? (name k) "+url")
         (into [:<>] (interpose " + " (for [url (k row)] [:a {:href url} url])))
         (s/ends-with? (name k) "+key")
         [:a {:href (str "#" (:anchor+url row))} (key-buttons k (k row))]
         :else (mark-text (k row)))])]))

(defn larp-title [row]
  (if (seq (:link+url row))
    [:a {:href (:link+url row)} (mark-text (:title row))]
    (mark-text (:title row))))

(defn larp-entry [row]
  [:div {:hidden false}
   [:h2 {:id (:anchor+url row) :key (str (:anchor+url row) "header")} (larp-title row)]
   [:h3 {:key (str (:anchor+url row) "byline")} "by " (mark-text (:designers row))]
   (when (seq (:not-eligible row))
     [:p {:key (str (:anchor+url row) (name :not-eligible))} "(Not Eligible for Awards)"])
   (when (seq (:designers-work+url row))
     [display-info :designers-work+url row "Other Work"])
   (when (seq (:styles-of-play+key row))
     [display-info :styles-of-play+key row])
   [display-info :description row]
   (when (seq (:tags+key row))
     [display-info :tags+key row])
   (when (seq (:year+key row))
     [display-info :year+key row "Year Submitted"])
   (let [accounted-for [:anchor+url :title :designers :designers-work+url
                        :tags+key :styles-of-play+key :description
                        :not-eligible :link+url :year+key]]
     (doall
      (for [k (keys (apply dissoc (:headers row) accounted-for))]
        ^{:key k}
        [display-info k row])))])

(defn contents [year larps]
  [:div {:id (str "content-" year)
         :hidden (not (@state/visible-years year))}
   (doall
    (for [row larps]
      ^{:key (:anchor+url row)} [larp-entry row] ))])

(def debounced-search-fn
  (gfun/debounce
   (fn [e] (let [q (.. e -target -value)]
             (if (> (count q) 2)
               (do (sieve/set-query! q)
                   (sieve/update-sieve!))
               (do (sieve/clear-query!)
                   (sieve/update-sieve!)))))
   300))

(defn search-bar []
  [:div [:label {:for "search"
                 :style {:margin-right "0.3em"}} "Filter by Search: "]
   [:input {:type "text" :id "search" :name "search"
            :on-input debounced-search-fn}]
   [:input {:type "button"
            :value "Clear Search"
            :style {:margin "0.1em 0.1em"
                    :background-color  clr-dark
                    :color clr-light}
            :class "search-clear"
            :on-click (fn []
                        (sieve/clear-query!)
                        (set! (.. (gdom/getElement "search") -value) "")
                        (sieve/update-sieve!))}]
   (when @state/no-match
     [:span {:style {:color "#a44"}} " No Matches Found"])])

(defn rand-showcase []
  (let [larps           @state/flat-results
        quantity        5
        showcased-larps (r/atom (take quantity (shuffle larps)))
        button-texts    ["Re-roll!" "Deal me another hand!"
                         "Shuffle!" "Spin again!"
                         "Acquire another 5 entries stochastically!"
                         "Another set of entries, please!"]
        text            (r/atom (rand-nth button-texts))]
    (when (> (count larps) quantity)
      [:details {:id "showcase" :style {:cursor "pointer"}}
       [:summary [:h2 "Random Showcase"]]
       [:input {:type     "button"
                :key      "random-button"
                :value    @text
                :style    {:background-color clr-dark
                           :color            clr-light
                           :display          "block"
                           :margin           "auto"}
                :on-click (fn []
                            (reset! showcased-larps
                                    (take quantity (shuffle larps)))
                            (reset! text (rand-nth button-texts)))}]
       (toc-by-year @showcased-larps)])))

(defn header []
  [:div
   [:h1 "Interactive Golden Cobra Archive [BETA]"]
   [search-bar]
   [filter-by-keys :year+key]
   [filter-by-keys :styles-of-play+key]
   [filter-by-keys :tags+key]
   [:input {:type "button"
            :value "Clear All Filters"
            :style {:margin "1em 0em"
                    :background-color  clr-dark
                    :color clr-light}
            :on-click (fn []
                        (sieve/clear-filters!)
                        (sieve/clear-query!)
                        (set! (.. (gdom/getElement "search") -value) "")
                        (sieve/update-sieve!))}]])

(defn result-body []
  (let [larps @state/entries]
    [:<> (doall (for [[yr llist] larps]
                  ^{:key yr} [contents yr llist]))]))

(defn assembled-page []
  [:div
   [header]
   [rand-showcase]
   [table-of-contents]
   [result-body]])
