(ns asp.loop
  (:require
   [cljs.core.async :as async :refer [put! go-loop <!]]
   [asp.print :as ap]
   [asp.read :as ar]
   [asp.input :as ai]
   [asp.state :as as :refer [asp-state]]
   [clojure.set :as set])
  (:import [goog.net XhrIo]))

(defn fetch-entries!
  "Takes in a `url` to fetch the data from,
  the `xform` function to transform the data with,
  the `channel` to send the data to,
  and the `year` associated with the data."
  [xform channel year]
  (let [url (str "csv/" year ".csv")
        cb  (fn [e]
              (let [target   (.-target e)
                    status   (.getStatus target)
                    response (.getResponseText target)]
                (if (and (<= 200 status) (> 400 status))
                  (do
                    (println (str "Loading entries for " year "."))
                    (put! channel (xform year response)))
                  (do
                    (println "Entries for" year "not found.")
                    (put! channel {year nil})))))]
    (.send XhrIo url cb)))

(defn hide [el]
  (set! (.-hidden el) true))

(defn unhide [el]
  (set! (.-hidden el) false))

(defn watch-years
  [_ _ old {:keys [max-year min-year]}]
  (when (or (not= max-year (:max-year old))
            (not= min-year (:min-year old)))
    (let [all-years (as/all-years)
          min-options (set (range (apply min all-years) (inc max-year)))
          max-options (set (range min-year (inc (apply max all-years))))
          visible? (set (range min-year (inc max-year)))]
      (doseq [yr all-years]
        (if (min-options yr)
          (unhide (ap/get-by-id (str "min-" yr)))
          (hide (ap/get-by-id (str "min-" yr))))
        (if (max-options yr)
          (unhide (ap/get-by-id (str "max-" yr)))
          (hide (ap/get-by-id (str "max-" yr))))
        (if (visible? yr)
          (do (unhide (ap/get-by-id (str yr "-toc")))
              (unhide (ap/get-by-id (str "year-" yr))))
          (do (hide (ap/get-by-id (str yr "-toc")))
              (hide (ap/get-by-id (str "year-" yr)))))))))

(defn filter-class [[k h]]
  (str (name h) "-" (name k)))

(defn watch-filter
  [_ _ old {:keys [filter-set]}]
  (when (not= (:filter-set old) filter-set)
    (let [#_         #_ available (as/available)
          selected   (map filter-class filter-set)
          deselected (map filter-class
                          (set/difference (:filter-set old) filter-set))]
      (println "Selected " selected "\nDeselected " deselected)
      (doseq [s selected]
        (run! (fn [el]
                (set! (.-style el) (str "background-color:"
                                        " rgb(26, 26, 26);"
                                        " color:"
                                        " rgb(255, 255, 204);")))
              (ap/get-by-class s)))
      (doseq [d deselected]
        (run! (fn [el]
                (set! (.-style el) (str "background-color:"
                                        " rgb(255, 255, 204);"
                                        " color:"
                                        " rgb(26, 26, 26);")))
              (ap/get-by-class d)))
      )
    ))


(defn initialize-years! []
  (let [years (as/all-years)
        max-yr (apply max years)
        min-yr (apply min years)]
    (swap! asp-state assoc :max-year max-yr)
    (swap! asp-state assoc :min-year min-yr)
    (add-watch asp-state ::years watch-years)
    (add-watch asp-state ::filter watch-filter)
    ))

(defn add-filter-listeners []
  (let [k-heads  (as/key-headers)
        k-maps   (update-vals
                  (apply merge-with merge
                         (map (fn [e] (select-keys e k-heads))
                              (as/entries))) keys)
        kh-pairs (reduce-kv
                  (fn [i k vs] (into i (map (fn [v] [v k]) vs))) [] k-maps)]
    (doseq [[k h :as pair] kh-pairs
            :let
            [click-fn (fn [e]
                        (.stopPropagation e)
                        (if ((:filter-set @asp-state) pair)
                          (swap! asp-state update :filter-set disj pair)
                          (swap! asp-state update :filter-set conj pair)))]]
      (doseq [el (ap/get-by-class (filter-class pair))]
        (.addEventListener el "click" click-fn)
        ))))

(add-filter-listeners)
(:filter-set (swap! asp-state
                    update :filter-set conj [:freeform-larp :styles-of-play+key]))

(def listeners
  [["min-year" "change"
    (fn [e]
      (.stopPropagation e)
      (swap! asp-state assoc :min-year (parse-long (.. e -target -value))))]
   ["max-year" "change"
    (fn [e]
      (.stopPropagation e)
      (swap! asp-state assoc :max-year (parse-long (.. e -target -value))))]
   ])

(defn add-listeners! [listeners]
  (doseq [[id event listen-fn] listeners]
    (.addEventListener (ap/get-by-id id) event listen-fn)))

(defonce incoming-entries (async/chan))

(defn initialize-entries! [in store]
  (go-loop [entries (<! in)
            years (set (keys entries))]
    (when (first (vals entries))
      (swap! store update :entries merge entries)
      (ap/load-entries entries))
    ;; When it's done, we've got work to do.
    (if (empty? (remove years ar/archive-search-range))
      (do
        (println "Finished loading.")
        (initialize-years!)
        (ap/load-toc)
        (ap/load-filters)
        (add-listeners! listeners)
        #_(async/close! in))
      (let [entries (<! in)
            yrs (into years (keys entries))]
        (recur entries yrs)))))

(defn init []
  (ap/load-page)
  (initialize-entries! incoming-entries asp-state)
  (doseq [yr ar/archive-search-range]
    (fetch-entries! ar/csv->map incoming-entries yr)))

#_(init)
