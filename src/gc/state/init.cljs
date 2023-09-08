(ns gc.state.init
  (:require [clojure.string :as s]
            [clojure.set :refer [difference]]
            [gc.sort-n-sorcery :as sns]
            [gc.utils.formats :as frmt]
            #_[goog.dom :as gdom]
            [goog.labs.format.csv :as csv]
            [reagent.core :as r])
  (:import [goog.net XhrIo]))

;; Contest Dates
(def debut-year 2014)

(def current-year
  (let [now   (js/Date.)
        month (.getMonth now)
        year  (.getFullYear now)]
    (if (> 7 month) (dec year) year)))

(def years-running (range debut-year (inc current-year)))

;; Google Nonsense
(defn g-csv-url [doc-id year]
  (str "https://docs.google.com/spreadsheets/d/"
       doc-id
       "/gviz/tq?tqx=out:csv&sheet="
       year))

(def doc-id "11_Vzo-uEPHFAwb4c-c7xJp5H8QYVuc7Ifour3EQZKZE")

#_(defonce collected-larp-els (array-seq (gdom/getElementsByClass "larps")))

#_(defn id->years [coll]
(map #(js/parseInt (.getAttribute % "id")) coll))

#_(def year-range
(->> collected-larp-els
     id->years
     ((juxt #(apply min %) #(inc (apply max %))))
     (apply range)
     reverse))

#_(defn csv-div->seq-table
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

(defn csv->seq-table [csv]
  (let [rows (->> csv
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

#_(defn div->map
    "Takes a div of csv larp data and transforms it into a map of that data."
    [div]
    (let [year (.-id div)
          entries (-> div
                      csv-div->seq-table
                      (seq-table->map "Year+key" year)
                      (add-anchors year))]
      {(js/parseInt year) (sort-by sns/sort-by-title entries)}))

(defn csv->map
  [year-int csv]
  (let  [year-str (str year-int)
         entries (-> csv
                     csv->seq-table
                     (seq-table->map "Year+key" year-str)
                     (add-anchors year-str))]
    {year-int (sort-by sns/sort-by-title entries)}))

#_(defonce entries (#_reduce
                    into (sorted-map-by >)
                    (reduce-kv csv->map {}
                               @contest-map)
                    #_(map div->map collected-larp-els)))

(defonce app-state (r/atom {:filtering #{}
                            :searching ""
                            :no-match false
                            :missing-years #{}
                            :entries (sorted-map-by >)
                            #_#_:entries entries}))

#_(defn ensure-unique-years [store xform]
    (fn [yr data]
      (let  [incoming (xform yr data)
             previous (:entries store)
             matches (filter (comp #{(incoming yr)} second) previous)
             match-yrs (map first matches)
             unique? (empty? matches)
             earliest? (< yr (apply min match-yrs))]
        (.log js/console (str "Checking: " incoming))
        (cond
          unique? (do (.log js/console (str "Matches: " matches)) incoming)
          earliest? (do (doseq [m-yr match-yrs]
                          (.log js/console (str "Removing " m-yr))
                          (swap! store update :entries dissoc m-yr))
                        incoming)
          :else nil))))

(defn fetch-data [url store xform yr]
  (let [callback (fn [_]
                   (this-as ^js bad-idea
                     (let [status (.getStatus bad-idea)
                           response (.getResponseText bad-idea)]
                       (when (and (<= 200 status) (> 400 status))
                         (swap! store
                                update :entries
                                merge (xform yr response))))))]
    (.send XhrIo url callback "GET" nil nil 1000)))

(defn init-data [store xform]
  #_(doseq [yr years-running :when (not= 2015 yr)]
      (fetch-data (g-csv-url doc-id yr) store xform yr))
  (doseq [yr years-running]
    (fetch-data (str "csv/" yr ".csv") store xform yr)))

(init-data app-state csv->map)

(defn all-headers []
  (disj (->> (:entries @app-state)
             vals
             flatten
             (mapcat keys)
             (into #{})) :headers))

(defn key-headers []
  (->> (all-headers)
       (filter #(s/ends-with? (name %) "+key"))
       (into #{})))

(defn url-headers []
  (->> (all-headers)
       (filter #(s/ends-with? (name %) "+url"))
       (into #{})))

(defn txt-headers []
  (difference (all-headers) (key-headers) (url-headers)))

(defn year-range []
  (sort > (keys (:entries @app-state))))
