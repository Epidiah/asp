(ns gc.ui
  (:require [clojure.string :as str]
            [gc.state.state :as state]
            [gc.sieve :as sieve]
            [goog.dom :as gdom]
            [goog.functions :as gfun]
            [reagent.core :as r]))

(def clr-dark "#1a1a1a")
(def clr-light "#ffffcc")

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
                [:td {:style {:border "none"}}
                 (into [:ul] contents)]
                [:<> [:td  {:style {:border "none"}}
                      (into [:ul] (take halfway contents))]
                 [:td {:style {:border "none"}}
                  (into [:ul] (drop halfway contents))]])]]]))

(defn table-of-contents []
  (let [visible-larps (into (sorted-map-by >) @sieve/filtered-entries)
        most-recent (apply max (keys visible-larps))]
    [:<>
     (doall
      (for [[year larps] visible-larps
            :let [yr-count (count larps)]]
        [:details
         {:id     (str "toc" year)
          :key    (str "toc" year)
          :open   (when (= year most-recent) true)
          :hidden (zero? yr-count)
          :style  {:cursor "pointer"}}
         [:summary
          [:h2 (str year " — " yr-count " Game"
                    (when-not (= 1 yr-count) "s"))]]
         [toc-by-year larps]]))]))

(defn key-counts [larps]
  (zipmap @state/key-headers
          (for [k-h @state/key-headers]
            (frequencies (mapcat (comp keys k-h) larps)))))

(defn filtered? [k k-header]
  (@state/filters [k k-header]))

(defn year-toggle [year]
  (fn [] (when-not (= 1 (count @state/visible-years))
           (if (@state/visible-years year)
             (sieve/add-year! year)
             (sieve/remove-year! year)))))

(defn filter-toggle [k k-header]
  (fn [] (if (filtered? k k-header)
           (sieve/remove-filter! k k-header)
           (sieve/add-filter! k k-header))))

(defn year-buttons []
  [:<>
   (doall
    (for [year @state/all-years]
      ^{:key year}
      [:input {:type     "button"
               :value    year
               :style
               (if (@state/visible-years year)
                 {:background-color clr-light :color clr-dark}
                 {:background-color clr-dark :color clr-light})
               :class    "asp-btn"
               :on-click (year-toggle year)}]
      #_[:button {:style
                  (if (@state/visible-years year)
                    {:background-color clr-light :color clr-dark}
                    {:background-color clr-dark :color clr-light})
                  :class    "asp-btn"
                  :on-click (year-toggle year)}
         year]))])

(defn key-buttons
  ([key-header coll+key]
   [key-buttons key-header coll+key nil])
  ([key-header coll+key key-count]
   [:<>
    (doall
     (for [[k v] coll+key]
       ^{:key k}
       [:input {:type     "button"
                :value    (str v
                               (when-let [quantity (k key-count)]
                                 (str " (" quantity ")")))
                :style
                (if (filtered? k key-header)
                  {:background-color clr-dark :color clr-light}
                  {:background-color clr-light :color clr-dark})
                :class    "asp-btn"
                :on-click (filter-toggle k key-header)}
        #_[:button {:style
                    (if (filtered? k key-header)
                      {:background-color clr-dark :color clr-light}
                      {:background-color clr-light :color clr-dark})
                    :class    "asp-btn"
                    :on-click (filter-toggle k key-header)}
           (str v
                (when-let [quantity (k key-count)]
                  (str " (" quantity ")")))]]))]))

(defn filter-by-year []
  [:div [:hr]
   [:label "Filter out Years: "]
   [year-buttons]
   [:input {:type     "button"
            :hidden   (every? @state/visible-years @state/all-years)
            :value    "Show All Years"
            :style    {:margin           "0.1em 0.1em"
                       :background-color clr-dark
                       :color            clr-light}
            :on-click (fn []
                        (doseq [yr @state/all-years]
                          (sieve/remove-year! yr)))}]])

(defn filter-by-keys [keyword-header]
  (r/with-let [visible (r/atom 5)]
    (let [larps   @sieve/result-list
          k-count (keyword-header (key-counts larps))
          sort-fn #(- ((first %) k-count))
          ks      (sort-by sort-fn (apply merge (map keyword-header larps)))
          headers (apply merge (map :headers larps))]
      [:div [:hr]
       [:label (str "Filter by " (keyword-header headers)) ": "]
       [key-buttons keyword-header (take @visible ks) k-count]
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
                    :on-click #(reset! visible 5)}]))])))

(defn mark-text
  ([text] (let [q (str/trim @state/query)]
            (if (seq q)
              (mark-text text (sieve/text-matches q text))
              text)))
  ([text found-list]
   (if (or (empty? found-list)
           (empty? text))
     text
     (let [found (first found-list)
           [head tail] (str/split text (sieve/plain-text->re found) 2)]
       [:<> head [:mark {:style {:padding "0"
                                 :background-color clr-dark
                                 :color clr-light}} found]
        (mark-text tail (rest found-list))]))))

(defn display-link [k row]
  (let [str->links (fn [text]
                     [:a {:href (if (str/starts-with? (str/lower-case text) "http")
                                  text
                                  (str "http://" text))}
                      text])
        vec->hiccup (fn [chunk]
                      (if (sequential? chunk)
                        (into [:<>]
                              (interpose " + "
                                         (map str->links chunk)))
                        (str " " chunk " ")))]
    [:<> (map vec->hiccup (k row))]))

(defn display-info
  ([k row] (display-info k row (k (:headers row))))
  ([k row head]
   [:<>
    (when (seq (k row))
      [:p [:strong (str head ": ")]
       (cond
         (str/ends-with? (name k) "+url")
         (display-link k row)
         #_(into [:<>] (interpose " + " (for [url (k row)] [:a {:href url} url])))

         (str/ends-with? (name k) "+key")
         [:a {:href (str "#" (:anchor+url row))} [key-buttons k (k row)]]

         :else [mark-text (k row)])])]))

(defn larp-title [row]
  (if (seq (:link+url row))
    [:a {:href (:link+url row)} (mark-text (:title row))]
    (mark-text (:title row))))

(defn larp-entry [year row]
  [:<>
   [:h2 {:id (:anchor+url row)} (larp-title row)]
   [:h3  "by " (mark-text (:designers row))]
   #_[:p {:style {:color "red"}}(rand)]
   (when (seq (:not-eligible row))
     [:p  "(Not Eligible for Awards)"])
   (when (seq (:designers-work+url row))
     [display-info :designers-work+url row "Other Work"])
   (when (seq (:styles-of-play+key row))
     [display-info :styles-of-play+key row])
   [display-info :description row]
   (when (seq (:tags+key row))
     [display-info :tags+key row])
   [:a {:href (str "#" "toc" year)} year]
   (let [accounted-for [:anchor+url :title :designers :designers-work+url
                        :tags+key :styles-of-play+key :description
                        :not-eligible :link+url :year]]
     (doall
      (for [k (keys (apply dissoc (:headers row) accounted-for))]
        ^{:key k}
        [display-info k row])))])

(defn larp-entry-wrapper [year row]
  [:div {:hidden (not (@sieve/visible-entries row))}
   ^{:key (:anchor+url row)}
   [larp-entry year row]])

(defn contents [year larps]
  [:div {:id (str "content-" year)
         :hidden (not (@state/visible-years year))}
   (doall
    (for [row larps]
      ^{:key (:anchor+url row)} [larp-entry-wrapper year row] ))])

(def debounced-search-fn
  (gfun/debounce
   (fn [e] (let [q (.. e -target -value)]
             (if (> (count q) 2)
               (sieve/set-query! q)
               (sieve/clear-query!))))
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
                        (set! (.. (gdom/getElement "search") -value) ""))}]])

(defn rand-showcase []
  (let [larps           @sieve/result-list
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
   [filter-by-year]
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
                        (set! (.. (gdom/getElement "search") -value) ""))}]])

(defn larp-list []
  (let [larps (into (sorted-map-by >) @state/entries)]
    [:<> (doall (for [[yr larp-list] larps]
                  ^{:key yr} [contents yr larp-list]))]))

(defn assembled-page []
  [:div
   [header]
   [rand-showcase]
   [table-of-contents]
   [larp-list]])
