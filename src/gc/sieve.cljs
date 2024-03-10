(ns gc.sieve
  (:require [gc.state.state :as state]
            [clojure.string :as s]))

(defn add-filter! [tag header]
  (if (= header :year+key)
    (swap! state/app-state
           update :state/visible-years disj (js/parseInt (name tag)))
    (swap! state/app-state
           update :state/filtering conj [tag header])))

(defn remove-filter! [tag header]
  (if (= header :year+key)
    (swap! state/app-state
           update :state/visible-years conj (js/parseInt (name tag)))
    (swap! state/app-state
           update :state/filtering disj [tag header])))

(defn clear-filters! []
  (swap! state/app-state assoc :state/filtering #{})
  (swap! state/app-state :state/visible-years (set @state/all-years)))

(defn filter-by-plus-key []
  (vec (for [[t k] (remove (comp #{:year+key} second) @state/filtering)]
         (filter (comp t k)))))

(defn update-larps [f larps]
  (reduce #(update-in %1 [%2] f) larps (keys larps)))

(def searchable-keys
  (map (fn [k-h] #(s/join " " (vals (k-h %)))) @state/key-headers))

(def searchable-text
  (into (vec @state/txt-headers) searchable-keys))

(def escape-these #{\\ \^ \$ \* \+ \? \. \| \( \) \{ \} \[ \]})

(defn quoted-input->query [quoted-input]
  (s/replace quoted-input #"\"" ""))

(defn unquoted-input->queries [unquoted-input]
  (s/split (s/trim unquoted-input) #"\s"))

(defn quoting [text]
  (s/join
    (for [chr text]
      (str (when (contains? escape-these chr) "\\") chr))))

(defn plain-text->re [text]
  (when (seq (s/trim text))
    (->> text
         quoting
         (str "(?i)")
         re-pattern)))

(defn input->queries [user-input]
  (let [queries (unquoted-input->queries user-input)
        quoted (->> user-input
                    (re-seq #"\".*?\"" )
                    (map quoted-input->query)
                    (take-nth 2))]
    (if (empty? quoted)
      queries
      (into quoted (remove
                    #(s/includes?
                      (s/join " " (into quoted "\"")) %)
                    queries)))))

(defn text-matches [user-input text]
  (remove nil?
          (flatten
           (for [q (map plain-text->re (input->queries user-input))]
             (re-seq q text)))))

(defn row-match? [user-input row]
  (let [queries (map plain-text->re (input->queries user-input))
        zones (remove nil? ((apply juxt searchable-text) row))]
    (some identity (for [q queries z zones] (re-seq q z)))))

(defn search-fn [user-input]
  (if (empty? user-input)
    identity
    (partial row-match? user-input)))

(defn change-match! [match?]
  (swap! state/app-state assoc :state/no-match match?))

(defn set-query! [user-input]
  (swap! state/app-state assoc :state/query user-input))

(defn clear-query! []
  (println "The state of the query: " @state/query)
  (set-query! "")
  (change-match! false))

(defn available-larps []
  (let [tag-filtering (filter-by-plus-key)
        query         @state/query
        searching     (search-fn query)
        entries       @state/entries]
    (cond->> (select-keys entries @state/visible-years)
      (seq tag-filtering) (update-larps
                           #(eduction (apply comp tag-filtering) %))
      (seq query)         (update-larps #(filter searching %)))))

(defn update-sieve! []
  (swap! state/app-state assoc :state/results (available-larps)))
