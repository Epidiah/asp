(ns asp.state
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

(defonce asp-state (atom {:entries      (sorted-map-by >)
                          :search-terms #{}
                          :min-year     nil
                          :max-year     nil
                          :filter-set   #{}}))

;; `:entries` will be a `sorted-map` and the keys will be the years.
;; {year ({map of entry with headers as keys})}
;; Each year's val is a seq of entries.
;; Each entry is a map with headers as keys.
;; SPECIAL KEY/VALS
;; `:headers` map of the entry's header keys to their string representations
;; `:id+url` string representing the id and anchor link sans "#"
;; `:*+url` vector of links
;; `:link+url` vector of links to the larp pdfs
;; `:*+key` map of filterable keys to their string representations
;; `:year` map of year as a key to year as a number

;; `:filter-set` will be a `set` of `(key header)` tuples such that
;; the key is the key to filter for the header is the header under which
;; that key can be found. Example: `[:freeform-larp :styles-of-play+key]`

(def search identity)

(defn filter-years [entries]
  (let [min-year (:min-year @asp-state)
        max-year (:max-year @asp-state)]
    (select-keys entries (range min-year (inc max-year)))))

(def key-filter
  (let [filter-fn (apply juxt (map (fn [k h] (comp k h))) (:filter-set @asp-state))]
    (fn [e] (some filter-fn e))))

(def flatten-entries (comp flatten vals))

(defn available []
  (let [{:keys [entries min-year max-year filter-set search-terms]} @asp-state]
    (cond->> (merge (sorted-map-by >) entries)
      (or min-year max-year)   filter-years
      :always                  flatten-entries
      (not-empty filter-set)   (filter key-filter)
      (not-empty search-terms) search)))

(defn not-available []
  (let [{:keys [entries min-year max-year filter-set search-terms]} @asp-state]
    (cond->> (merge (sorted-map-by >) entries)
      (or min-year max-year)   filter-years
      :always                  flatten-entries
      (not-empty filter-set)   (remove key-filter)
      (not-empty search-terms) search)))

(count (available))
(count (not-available))

(defn headers-map []
  (apply merge
         (->> (:entries @asp-state)
              vals
              flatten
              (map :headers))))

(defn headers-set []
  (into #{} (keys (headers-map))))

(defn key-headers []
  (->> (headers-set)
       (filter #(str/ends-with? (name %) "+key"))
       (into #{})))

(defn url-headers []
  (->> (headers-set)
       (filter #(str/ends-with? (name %) "+url"))
       (into #{})))

(defn txt-headers []
  (set/difference (headers-set) (key-headers) (url-headers)))

(defn all-years []
  (sort > (keys (:entries @asp-state))))

(defn entries []
  (->> @asp-state
       :entries
       (merge (sorted-map-by >))
       vals
       flatten))
