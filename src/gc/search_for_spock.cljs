(ns gc.search-for-spock
  (:require [gc.state.init :refer [app-state key-headers txt-headers]]
            [clojure.string :as s]))

(def searchable-keys 
    (map (fn [k-h] #(s/join " " (vals (k-h %)))) key-headers))

(def searchable-text
  (into (vec txt-headers) searchable-keys))

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
          (flatten (for [q (map plain-text->re (input->queries user-input))]
                     (re-seq q text)))))

(defn row-match? [user-input row]
  (let [queries (map plain-text->re (input->queries user-input))
        zones (remove nil? ((apply juxt searchable-text) row))]
    (some identity (for [q queries z zones] (re-seq q z)))))

(defn search-fn [user-input]
  (if (empty? user-input)
    identity
    (partial row-match? user-input)))

(defn fruitful-search? [user-input larps]
    (seq (filter (search-fn user-input) larps)))

(defn add-search! [user-input]
      (swap! app-state #(assoc-in % [:searching] user-input)))
