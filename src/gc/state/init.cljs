(ns gc.state.init
  (:require [clojure.string :as s]
            [clojure.set :refer [difference]]
            [gc.sort-n-sorcery :as sns]
            [gc.utils.formats :as frmt]
            [goog.dom :as gdom]
            [goog.labs.format.csv :as csv]
            [reagent.core :as r]))

(defonce collected-larp-els (array-seq (gdom/getElementsByClass "larps")))

(defn id->years [coll]
  (map #(js/parseInt (.getAttribute % "id")) coll))

(def year-range
  (->> collected-larp-els
       id->years 
       ((juxt #(apply min %) #(inc (apply max %))))
       (apply range)
       reverse))

(defn csv-div->seq-table 
  "Takes a div containing csv data and returns a seq of rows of table data
  where the first row contains the headers."
  [csv-div]
  (let [rows (->> csv-div
                  .-innerHTML
                  s/trim
                  frmt/unescape
                  csv/parse
                  js->clj
                  (remove empty?))]
    (map (fn [row] (map #(s/trim %) row)) rows)))

(defn format-keys 
  "Takes a collect of strings and returns a collection of keywords"
  [coll]
  (->> coll
       (map
         #(-> %
              (frmt/format-key)
              keyword))))

(defn format-key-map 
  "Takes a string representing a csv of headers and returns a map
  that associates the keyword for each header with the plain string."
  [key-csv]
  (let [keyring (->> (s/replace key-csv \; \,)
                     csv/parse
                     js->clj
                     flatten
                     (map #(s/trim %)))]
    (zipmap (format-keys keyring) keyring)))

(defn format-url-vector
  "Takes a string representing one or more urls and returns a vector
  of those urls as individual strings."
  [url-csv]
  (->> url-csv
      csv/parse
      js->clj
      flatten
      (map #(s/trim %))
      vec))

(defn create-key-maps 
  "Takes a map representing a row of table data and returns a new map
  where all columns tagged with `+key` at the end now hold maps of the
  their contents in the format of {:key-word \"Key word\"}"
  [row]
  (into {} (map (fn [[k v]]
         (if (s/ends-with? (name k) "+key")
           [k (format-key-map v)]
           [k v]))
       row)))

(defn create-url-vectors
  [row]
  (map (fn [[k v]]
         (if (s/ends-with? (name k) "+url")
           [k (format-url-vector v)]
           [k v]))
       row))

(defn add-anchors
  "Adds a {:anchor+url URL} map to each row of data in a map.
  URL is generated using k and first 7 letters of the title."
  [rows k]
  (map (fn [row] (-> row
                     (assoc :anchor+url (str k (:title row)))
                     (assoc-in [:headers :anchor+url] "Anchor")))
       rows))

(defn clean-header 
  "Cleans the +url and +key tags off of the string representation of 
  a column header."
  [header]
  (let [tester (s/lower-case header)]
    (if (or (s/ends-with? tester "+url")
          (s/ends-with? tester "+key"))
    (first (s/split header #"\+"))
    header)))

(defn seq-table->map 
  "Takes a seq of seqs representing a table where the first seq
  is the column headers and each seq after that is a row of data.
  Returns a seq of maps associating a keyword header to its data cell.
  Adds {:headers {map of keyword-to-string associations of header data}}
  to each row.
  Constant data can be added to each row. Should be presents in pairs
  of string representing header followed by string representing data."
  [table & constant-data]
  (let [more-headers (reverse (take-nth 2 constant-data))
        more-cells (reverse (take-nth 2 (rest constant-data)))
        columns (into (first table) more-headers)
        headers (format-keys columns)
        header-map (zipmap headers (map clean-header columns))
        rows (map #(zipmap headers (into % more-cells)) (rest table))]
    (map (fn [row] (-> row
                       create-url-vectors
                       create-key-maps
                       (assoc :headers header-map)))
         rows)))

(defn div->map
  "Takes a div of csv larp data and transforms it into a map of that data."
  [div]
  (let [year (.-id div)
        entries (-> div
                    csv-div->seq-table
                    (seq-table->map "Year+key" year)
                    (add-anchors year))]
    {(js/parseInt year) (sort-by sns/sort-by-title entries)}))

(defonce entries (reduce into (sorted-map-by >) (map div->map collected-larp-els)))

(defonce app-state (r/atom {:filtering #{}
                            :searching ""
                            :no-match false
                            :missing-years #{}
                            :year-range year-range
                            :entries entries}))

(defonce all-headers (disj (->> (:entries @app-state)
                                vals
                                flatten
                                (mapcat keys)
                                (into #{})) :headers))

(defonce key-headers (->> all-headers
                          (filter #(s/ends-with? (name %) "+key"))
                          (into #{})))

(defonce url-headers (->> all-headers
                          (filter #(s/ends-with? (name %) "+url"))
                          (into #{})))

(defonce txt-headers (difference all-headers key-headers url-headers))
