(ns gc.state.init
  (:require [clojure.string :as str]
            [gc.utils.formats :as frmt]
            [goog.dom :as gdom]
            [goog.labs.format.csv :as csv]))

(defonce collected-larp-els (array-seq (gdom/getElementsByClass "larps")))

(defn id->years [coll]
  (map #(js/parseInt (.getAttribute % "id")) coll))

(defn csv-div->seq-table
  "Takes a div containing csv data and returns a seq of rows of table data
  where the first row contains the headers."
  [csv-div]
  (let [rows (->> csv-div
                  .-innerHTML
                  str/trim
                  frmt/unescape
                  csv/parse
                  js->clj
                  (remove empty?))]
    (map (fn [row] (map #(str/trim %) row)) rows)))

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
  (let [keyring (->> (str/replace key-csv \; \,)
                     csv/parse
                     js->clj
                     flatten
                     (map #(str/trim %)))]
    (zipmap (format-keys keyring) keyring)))

(defn format-url-vector
  "Takes a string representing one or more urls and returns a vector
  of those urls as individual strings."
  [url-csv]
  (->> url-csv
       csv/parse
       js->clj
       flatten
       (map #(str/trim %))
       vec))

(defn create-key-maps
  "Takes a map representing a row of table data and returns a new map
  where all columns tagged with `+key` at the end now hold maps of the
  their contents in the format of {:key-word \"Key word\"}"
  [row]
  (into {} (map (fn [[k v]]
                  (if (str/ends-with? (name k) "+key")
                    [k (format-key-map v)]
                    [k v]))
                row)))

(defn create-url-vectors
  [row]
  (map (fn [[k v]]
         (if (str/ends-with? (name k) "+url")
           [k (format-url-vector v)]
           [k v]))
       row))

(defn add-anchors
  "Adds a unique url fragment to the row map under the key `:anchor+url`."
  [rows]
  (map (fn [row] (-> row
                     (assoc :anchor+url (str "anchor" (hash row)))
                     (assoc-in [:headers :anchor+url] "Anchor")))
       rows))

(defn clean-header
  "Cleans the +url and +key tags off of the string representation of
  a column header."
  [header]
  (let [tester (str/lower-case header)]
    (if (or (str/ends-with? tester "+url")
            (str/ends-with? tester "+key"))
      (first (str/split header #"\+"))
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

(defn sortable-title
  "Produces a title for the row in lower-case and without the articles
  (a/the)or extraneous punctuation at the front.
  Suitable for alphabetical sorting.
  I'm afraid you're going to have to manually add to this list
  as more titles come in."
  [base-title]
  (loop [title (str/lower-case (str base-title))]
    (cond
      (str/starts-with? title "#")    (recur (subs title 1))
      (str/starts-with? title "(")    (recur (subs title 1))
      (str/starts-with? title "[")    (recur (subs title 1))
      (str/starts-with? title "\"")   (recur (subs title 1))
      (str/starts-with? title "'")    (recur (subs title 1))
      (str/starts-with? title "『")   (recur (subs title 1))
      (str/starts-with? title "the ") (recur (subs title 4))
      (str/starts-with? title "a ")   (recur (subs title 2))
      :else                           title)))

(defn div->map
  "Takes a div of csv larp data and transforms it into a map of that data."
  [div]
  (let [year (parse-long (.-id div))
        entries (-> div
                    csv-div->seq-table
                    (seq-table->map "Year" year)
                    add-anchors)]
    {year (sort-by (comp sortable-title :title) entries)}))

(defonce entries (reduce into (sorted-map-by >) (map div->map collected-larp-els)))
