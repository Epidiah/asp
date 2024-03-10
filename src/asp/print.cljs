(ns asp.print
  (:require
   [asp.state :as state :refer [asp-state]]
   [clojure.string :as str]))

(defn panic [el hiccup]
  (.setAttribute el "style" "color: red; font-weight: bold;")
  (.setAttribute el "data-hiccup" (pr-str hiccup))
  (set! (.-innerText el) "There has been a grave error. Call Eppy!"))

;; (def clr-dark "#1a1a1a")
;; (def clr-light "#ffffcc")

(def debug? ^boolean js/goog.DEBUG)

(defn get-by-id [id]
  (.getElementById js/document id))

(defn get-by-class [class]
  (.getElementsByClassName js/document class))

(def asp (get-by-id "asp"))

(defn add-attrs! [el attrs]
  (doseq [class (:class attrs)]
    (.. el -classList (add class)))
  (doseq [[k v] (dissoc attrs :class :events)]
    (.setAttribute el (name k) (str v)))
  el)

(defn hiccup->element [hiccup]
  (if-not (sequential? hiccup)
    (str hiccup)
    (let [tag            (if (= :<> (first hiccup))
                           "div"
                           (name (first hiccup)))
          [attrs hiccup] (if (map? (second hiccup))
                           [(second hiccup) (nthrest hiccup 2)]
                           [{} (rest hiccup)])
          x-fn!          (fn x-fn! [el<- hiccup]
                           (if (and (coll? hiccup) (= :<> (first hiccup)))
                             (reduce x-fn! el<- (rest hiccup))
                             (.append el<- (hiccup->element hiccup)))
                           el<-)
          base-el        (add-attrs!
                          (.createElement js/document (name tag))
                          attrs)]
      (reduce x-fn! base-el hiccup))))

(defn render [hiccup] (.-outerHTML (hiccup->element hiccup)))

(defn id [k entry]
  (str (:id+url entry) "-" (name k)))

(defn btn-classes [header k]
  [(name header)
   (str (name header) "-" (name k))
   "asp-btn"])

(defn key-button
  ([header kn]
   (key-button header nil kn false))
  ([header key-count [k n] hidden?]
   [:button
    (merge {:class (btn-classes header k)}
           (when hidden? {:hidden true}))
    (if (= header :year)
      n
      (str n (when-let [quantity (k key-count)]
               (str " (" quantity ")"))))]))

#_(defn mark-text
    ([text] (let [searching (str/trim (:search @asp-state))]
              (if (seq searching)
                (mark-text text (stiii/text-matches searching text))
                text)))
    ([text found-list]
     (if (or (empty? found-list)
             (empty? text))
       text
       (let [found (first found-list)
             [head tail] (str/split text (stiii/plain-text->re found) 2)]
         [:<> head [:mark {:style {:padding "0"
                                   :background-color clr-dark
                                   :color clr-light}} found]
          (mark-text tail (rest found-list))]))))

(defn display-url [entry k]
  (let [str->links (fn [s]
                     [:a {:href
                          (if (str/starts-with? (str/lower-case s) "www.")
                            (str "http://" s) s)} s])
        vec->hiccup (fn [chunk]
                      (if (sequential? chunk)
                        (into [:<>]
                              (interpose " + "
                                         (map str->links chunk)))
                        (str " " chunk " ")))]
    (into [:<>] (map vec->hiccup (k entry)))))

(defn display-key [entry k]
  (into [:a {:href (str "#" (:id+url entry))
             :style "color: unset;"}]
        (map (partial key-button k) (k entry))))

(defn display-year [entry k]
  [:a {:href (str "#" (k entry) "-toc")}  (k entry)])

(defn display-info
  ([entry k] (display-info entry k (k (:headers entry))))
  ([entry k head]
   (when (or (= k :year) (seq (k entry)))
     [:p {:id (id k entry)} [:strong (str head ": ")]
      (cond
        (= k :year)                      (display-year entry k)
        (str/ends-with? (name k) "+url") (display-url entry k)
        (str/ends-with? (name k) "+key") (display-key entry k)
        :else                            [:<> (k entry)])])))

(defn pdf-link [entry] (ffirst (:link+url entry)))

(defn entry->elt-str
  [{:keys [id+url title designers designers-work+url
           styles-of-play+key tags+key]
    :as   entry}]
  (let [display (partial display-info entry)]
    (render
     [:div {:id id+url}
      [:h2 [:a {:href (pdf-link entry)
                :id (id :link+url entry)} title]]
      (when designers
        [:h3 {:id (id :designers entry)} "by " designers])
      (when (not-empty designers-work+url)
        (display :designers-work+url "Other Work"))
      (when (not-empty styles-of-play+key)
        (display :styles-of-play+key))
      (display :description)
      (when (not-empty tags+key)
        (display :tags+key))
      (display :year "Year Submitted")
      (let [remains (keys
                     (apply dissoc (:headers entry)
                            [:id+url :title :designers :designers-work+url
                             :tags+key :styles-of-play+key :description
                             :not-eligible :link+url :year]))]
        (into [:div] (map display remains)))])))

(defn year-div [year]
  (render [:div {:id (str "year-" year)
                 :class ["year"] :data-year year}]))

(defn situate-year [year]
  (let [divs      (.getElementsByClassName asp "year")
        div-range (for [d divs] (.getAttribute d "data-year"))
        next-yr   (->> div-range
                       sort
                       (some #(when (> % year) %)))]
    (if next-yr ["afterend" (str "year-" next-yr)] ["afterbegin" "entries"])))

(defn load-page []
  (set! (.-innerHTML asp)
        (render
         [:div
          [:div {:style (str "background-color:#1a1a1a;"
                             "color: #ffffcc;"
                             "margin: 2em auto;"
                             "border-radius: 1em;"
                             "text-align: center;")}
           [:h1 "Archive of Strange Play"]]
          [:div {:id "search"}]
          [:div {:id "years"}]
          [:div {:id "filters"}]
          [:div {:id "toc"}]
          [:div {:id "entries"}
           [:div {:id "loading"}
            "Caution: Larp Loading…"]]])))

(defn load-entries
  [entry-map]
  (let [yr (first (keys entry-map))
        entries (first (vals entry-map))
        [loc id] (situate-year yr)]
    (when-let [loading-msg (get-by-id "loading")]
      (.remove loading-msg))
    (when-not (get-by-id (str "year-" yr))
      (.insertAdjacentHTML (get-by-id id) loc (year-div yr)))
    (run!
     (fn [entry]
       (if-let [home (get-by-id (:id+url entry))]
         (set! (.-outerHTML home) (entry->elt-str entry))
         (.insertAdjacentHTML
          (get-by-id (str "year-" yr))
          "beforeend" (entry->elt-str entry))))
     entries)
    yr))


(defn toc-entry [entry]
  [:li {:id (str "toc-" (:id+url entry))}
   [:a {:href (str "#" (:id+url entry))}
    (:title entry)
    (when-let [designers (:designers entry)]
      (str " by " designers))]])

(defn toc-by-year [entries]
  (let [split-on (quot (count entries) 2)
        contents (map toc-entry entries)]
    [:table
     [:tbody
      [:tr
       (if (> 5 split-on)
         [:td {:style "border: none;"} (into [:ul] contents)]
         [:div
          [:td  {:style "border: none;"}
           (into [:ul] (take split-on contents))]
          [:td {:style "border: none;"}
           (into [:ul] (drop split-on contents))]])]]]))

(defn table-of-contents [entries]
  (let [most-recent (:year (first entries))]
    (into
     [:div]
     (for [yr-group (partition-by :year entries)
           :let [yr (:year (first yr-group))
                 total (count yr-group)]]
       [:details
        (merge {:id (str yr "-toc")
                :style "cursor: pointer;"}
               (when (= yr most-recent)
                 {:open true}))
        [:summary
         [:h2
          yr
          [:span {:class ["when-open"]} " — " total " Game"
           (when-not (= 1 total) "s")]]
         (into [:ul {:class ["when-closed"]}] (map toc-entry (take 5 (shuffle entries))))
         [:span {:class ["when-closed"]} "And " (- total 5) " more…"]]
        (toc-by-year yr-group)]))))

(defn load-toc []
  (set!
   (.-innerHTML (get-by-id "toc"))
   (render (table-of-contents (state/entries)))))

(defn key-total-map [entries]
  (let [key-headers (state/key-headers)]
    (zipmap key-headers (for [kh key-headers]
                          (frequencies (mapcat (comp keys kh) entries))))))

(defn filters-by-keys
  ([entries header] (filters-by-keys entries header 5))
  ([entries header show]
   (let [totals     (header (key-total-map entries))
         sort-fn    #(- ((first %) totals))
         ks         (sort-by sort-fn (apply merge (map header entries)))]
     [:div [:hr]
      [:label [:strong (str "Filter by " (header (state/headers-map)))] ": "]
      (into [:span]
            (map-indexed
             (fn [idx k] (key-button header totals k (<= show idx)))
             ks))
      [:input
       {:type "button"
        :value (str "+" (- (count ks) show) " more")
        :id (str "toggle-" (name header))
        :class (btn-classes header :more)}]])))

(defn select-by-year
  [years]
  (let [min-year (or (:min-year @asp-state) (apply min years))
        max-year (or (:max-year @asp-state) (apply max years))]
    [:div
     [:label [:strong "Selected Years from "]
      (into [:select {:name "min-year" :id "min-year"}]
            (map (fn [yr] [:option
                           (merge {:value yr
                                   :id (str "min-" yr)}
                                  (when (= yr min-year)
                                    {:selected true})
                                  (when (> yr max-year)
                                    {:hidden true}))
                           yr])
                 (sort < years)))]
     [:label [:strong " to " ]
      (into [:select {:name "max-year" :id "max-year"}]
            (map (fn [yr] [:option
                           (merge {:value yr
                                   :id (str "max-" yr)}
                                  (when (= yr max-year)
                                    {:selected true})
                                  (when (< yr min-year)
                                    {:hidden true}))
                           yr])
                 (sort < years)))]]))

(defn load-filters []
  (set! (.-innerHTML (get-by-id "years"))
        (render (select-by-year (state/all-years))))
  (doseq [k (into
             (remove #{:styles-of-play+key :tags+key} (state/key-headers))
             [:tags+key :styles-of-play+key])]
    (.insertAdjacentHTML (get-by-id "filters") "beforeend"
                         (render (filters-by-keys (state/entries) k)))))

(comment

  (.addEventListener (get-by-id "toggle-styles-of-play+key") "click"
                     (fn [_]
                       (doseq [el (.getElementsByClassName
                                   js/document "styles-of-play+key")]
                         (when (not= "toggle-styles-of-play+key" (.-id el))
                           (.toggleAttribute el "hidden"))
                         #_(set! (.-hidden el) false))))

  )
