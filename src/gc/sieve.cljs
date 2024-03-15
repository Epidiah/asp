(ns gc.sieve
  (:require [gc.state.state :as state]
            [reagent.ratom :as ratom]
            [clojure.string :as str]))

(defn add-year! [yr]
  (swap! state/app-state
         update :state/visible-years disj yr))

(defn add-filter! [tag header]
  (swap! state/app-state
         update :state/filters conj [tag header]))

(defn remove-year! [year]
  (swap! state/app-state
         update :state/visible-years conj year))

(defn remove-filter! [tag header]
  (swap! state/app-state
         update :state/filters disj [tag header]))

(defn clear-filters! []
  (swap! state/app-state assoc :state/filters #{})
  (swap! state/app-state assoc :state/visible-years (set @state/all-years)))

(defn set-query! [user-input]
  (swap! state/app-state assoc :state/query user-input))

(defn clear-query! []
  (set-query! ""))

(defn searchable-text [row]
  (let [get-tag-text (map (fn [k-h] (comp vals k-h)) @state/key-headers)
        get-all-text (apply juxt (into @state/txt-headers get-tag-text))]
    (->> row
         get-all-text
         ;; a little clean up
         (remove nil?)
         flatten
         (map str))))

(defn quoted-input->queries [quoted-input]
  (map #(str/replace % #"\"" "") quoted-input))

(defn unquoted-input->queries [unquoted-input]
  (str/split (str/trim unquoted-input) #"\s"))

(defn quoting [text]
  (let [escape-set #{\\ \^ \$ \* \+ \? \. \| \( \) \{ \} \[ \]}
        escape (fn [c] (str (when (escape-set c) \\) c))]
    (->> text
         (map escape)
         str/join)))

(defn plain-text->re [text]
  (when (not-empty (str/trim text))
    (->> text
         quoting
         (str "(?i)") ; case insensitive, won't appear in printed literal
         re-pattern)))

#_(defn input->queries [user-input]
    (let [queries (unquoted-input->queries user-input)
          quoted (->> user-input
                      (re-seq #"\".*?\"" )
                      (map quoted-input->query)
                      #_(take-nth 2))]
      (if (empty? quoted)
        queries
        (into quoted (remove
                      #(str/includes?
                        (str/join " " (into quoted "\"")) %)
                      queries)))))

(defn input->queries [user-input]
  (let [quoted        (->> user-input
                           (re-seq #"\".*?\"" ))
        remove-quoted (apply comp
                             (map (fn [m]
                                    (fn [s]
                                      (str/replace s m "")))
                                  quoted))
        unquoted      (remove-quoted user-input)]
    (->> (unquoted-input->queries unquoted)
         (concat (quoted-input->queries quoted))
         (remove empty?))))


(defn text-matches [user-input text]
  (remove nil?
          (flatten
           (for [q (map plain-text->re (input->queries user-input))]
             (re-seq q text)))))

(defn row-match? [user-input row]
  (let [queries (map plain-text->re (input->queries user-input))
        zones   (searchable-text row)]
    (some identity (for [q queries z zones] (re-seq q z)))))

(defn search-fn [user-input]
  (if (empty? user-input)
    identity
    (partial row-match? user-input)))

(defonce filter-xf
  (ratom/reaction
   (apply comp
          (filter (search-fn @state/query))
          (map (fn [[ky hdr]] (filter (comp ky hdr))) @state/filters))))

(defonce filtered-entries
  (ratom/reaction
   (-> @state/entries
       (select-keys @state/visible-years)
       (update-vals (fn [v] (into [] @filter-xf v))))))

(defonce visible-entries
  (ratom/reaction
   (-> @filtered-entries
       vals
       flatten
       set)))

(defonce result-list
  (ratom/reaction
   (-> @filtered-entries
       vals
       flatten)))
