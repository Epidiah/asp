(ns asp.read
  (:require
   [asp.utils :as utils]
   [clojure.string :as str]
   [goog.labs.format.csv :as csv])
  (:import [goog.net XhrIo]))

;; Retrieve CSVs

(def archive-search-range
  "A reasonable range of years to search for based on the current year."
  (let [debut-year   2014
        current-year (.getFullYear (js/Date.))]
    (range debut-year (inc current-year))))

(defn fetch-entries!
  "Takes in a `url` to fetch the data from,
  the `xform` function to transform the data with,
  the atom to `store` the transformed data in,
  and the `year` associated with the data."
  [url xform store year]
  (let [cb (fn [_]
             (this-as ^js this-is-a-bad-idea
               (let [status   (.getStatus this-is-a-bad-idea)
                     response (.getResponseText this-is-a-bad-idea)]
                 (when (and (<= 200 status) (> 400 status))
                   (swap! store update :entries
                          merge (xform year response))))))]
    (.send XhrIo url cb "GET" nil nil #_1000)))

(defn initialize-entries!
  "Called to initially fetch the data for the entries from the server.
  Assumes the CSVs are in a pulbic folder labeled 'csv' and are themselves
  labeled by year with the '.csv' extentsion. For example: 2014.csv
  `xform` should be a function that transforms the raw data into a
  usable format. `store` should be an atom in which you will store
  the data as it is received."
  [xform store]
  (doseq [yr archive-search-range]
    (fetch-entries! (str "csv/" yr ".csv") xform store yr)))

(def csv->clj
  "Accepts a string representation of a csv and returns a vector of
  vectors of strings, where each internal vector represents a row
  of the csv table. Each string within that vector represents the
  data for that row associated with the column in that position."
  (comp js->clj csv/parse str/trim))

(defn unescape
  "Returns any safe characters that may have been escaped in the
  processing back into their original form. Please add as necessary."
  [escaped-string]
  (-> escaped-string
      (str/replace #"&amp;" "&")
      (str/replace #"&mdash;" "—")))

(defn safe-format
  "Takes a string of `words` and returns a string that should be
  safe to use as an unqualified keyword or a url."
  [words]
  (-> words
      str
      str/lower-case
      (str/replace #"\"" "'")
      (str/replace #"\s+" "-")
      (str/replace #"/" "-or-")
      (str/replace #"&" "and")))

(defn format-keys
  "Takes a `coll` of strings and returns a seq of keywords"
  [coll]
  (map (comp keyword safe-format) coll))

(defn de-tag-header
  "Cleans the +url and +key tags off of the string representation of
  a column `header`. Returns the cleaned string."
  [header]
  (let [tester (str/lower-case (str header))]
    (if-let [end (or (str/last-index-of tester "+url")
                     (str/last-index-of tester "+key"))]
      (subs header 0 end)
      header)))

(defn suffix-key?
  "Returns true if `k` ends with `suffix`."
  [suffix k]
  (-> k
      name
      str/lower-case
      (str/ends-with? (str/lower-case suffix))))

;; Why did you do this this way, Past Eppy? Drunk? Or some wisdom
;; no longer accessible to me?

(defn update-suffixed
  "Given a `suffix` and an `update-fn` creates a function that
  accepts a `m`ap and applies the `update-fn` to all vals in
  the map that have the `suffix`."
  [suffix update-fn]
  (fn [m]
    (let [suffix-keys (filter (partial suffix-key? suffix) (keys m))
          suffix-map (update-vals (select-keys m suffix-keys) update-fn)]
      (merge m suffix-map))))

(def update-url-vectors
  "Takes a map representing row data and transforms the vals for
  all keys ending in '+url' into a vector with the urls seperated out."
  (update-suffixed
   "+url"
   (fn [s]
     (if (str/blank? s)
       []
       (let [chunks      (partition-by utils/url-str? (str/split s #"\s+"))
             text-chunk? (fn [f] ((complement utils/url-str?) f))
             prep-text   (fn [c] (str/join " " c))
             prep-url    (fn [c] (mapv #(str/replace % #"[,;]$" "") c))]
         (map (fn [c]
                (if (text-chunk? (first c))
                  (prep-text c)
                  (prep-url c))) chunks))))))

(def update-key-maps
  "Takes a map representing a row of table data and returns a new map
  where all columns tagged with '+key' at the end now hold maps of the
  their contents in the format of {:key-word \"Key word\"}"
  (update-suffixed
   "+key"
   (fn [key-csv]
     (let [tags (map #(str/trim %)
                     (-> key-csv
                         (str/replace \; \,)
                         (str/replace \" \')
                         csv->clj
                         flatten))]
       (zipmap (format-keys tags) tags)))))

(defn add-ids
  "Given a sequence of maps representing `rows` of table data, adds
  an entry to the row that represents a hopefully unique id attribute
  for the inevitable element. `prefix` is probably the year of the
  entry."
  [rows prefix]
  (map (fn [row]
         (-> row
             (assoc :id+url (str prefix
                                 (safe-format (:title row))
                                 "-by-"
                                 (safe-format (:designers row))))
             (assoc-in [:headers :id+url] "Id")))
       rows))

(defn csv-str->seq-table
  "Given a string representation of a csv table, returns a
  seq of seqs in which `first` is the headers, in order,
  and `rest` is a seq contain a seq for each row of the
  table, element corresponding to the data for the header of
  the same position in the headers seq."
  [csv-str]
  (let [rows (->> csv-str
                  unescape
                  csv->clj
                  (remove empty?))]
    (map (fn [row] (map #(str/trim %) row)) rows)))

(defn seq-table->map
  "Takes a seq of seqs representing a table where the first seq
  is the column headers and each seq after that is a row of data.
  Returns a seq of maps associating a keyword header to its data cell.
  Adds {:headers {map of keyword-to-string associations of header data}}
  to each row.

  Constant data can be added to each row. Should be presents in pairs
  of string representing header followed by string representing data."
  [table & constant-data]
  (let [headers            (first table)
        data               (rest table)
        additional-headers (take-nth 2 constant-data)
        additional-data    (take-nth 2 (rest constant-data))
        full-headers       (into headers additional-headers)
        header-keys        (format-keys full-headers)
        header-map         (zipmap header-keys (map de-tag-header full-headers))
        rows               (map #(zipmap header-keys (into % additional-data)) data)]
    (map (fn [row] (-> row
                       update-url-vectors
                       update-key-maps
                       (assoc :headers header-map)))
         rows)))

(defn csv->map
  [csv-title csv-str]
  (let  [anchor-prefix (str "asp-" csv-title "-")
         entries (-> csv-str
                     csv-str->seq-table
                     (seq-table->map "Year" csv-title)
                     (add-ids anchor-prefix)
                     )]
    {csv-title (sort-by (comp utils/sortable-title :title) entries)}))
