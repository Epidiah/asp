(ns gc.utils.larp-scrapper
  (:require [clojure-csv.core :as csv]
            [clojure.string :as s]))


(defn grab-between [text left right]
  (let [left-width (if (string? left) (count left) 1)
        start (+ (s/index-of text left) left-width)
        end (s/index-of text right start)]
    [(subs text start end) end]))

(defn grab-multiple
  ([text left right] (grab-multiple text left right []))
  ([text left right coll]
   (if (s/includes? text left)
     (let [[grab pointer] (grab-between text left right)]
       (grab-multiple (subs text pointer) left right (conj coll grab)))
     coll)))

(def archive-url "http://www.goldencobra.org/archives.html")

(def archive (slurp archive-url))

(def submission-links 
  (->> ((grab-multiple archive "href=\"" \")
       (filter #(s/includes? % "submissions"))
       (map #(str "http://www.goldencobra.org/" %)))))

(def sub-pages (->> submission-links
                    (map slurp)
                    (map (juxt #(first (grab-between % "<hr>" "</div>"))
                               #(first (grab-between % "<h1>" " Submissions"))))))

(defn get-larp-link [l]
  (first (grab-between l "href=\"" \")))

(defn get-title [l]
  (first (grab-between l "<h2>" "</h2>")))

(defn get-designers [l]
  (first (grab-between l "<h3>by " "</h3>")))

(defn get-designers-work [l]
  (s/join ", " (grab-multiple l "<p><u><a href=\"" \")))

(defn get-styles [l]
  (grab-between l "<p>Styles of Play: " "</p>"))

(defn get-description [l start]
  (first (grab-between (subs l start) "<p>" "</p>")))

(defn get-tags [l]
  (first (grab-between l "<p>Tags: " "</p>")))

(defn web-dump->rows [web-string]
  (s/split web-string #"<hr>"))

(defn row-maker [row-string]
  (let [title (get-title row-string)
        pdf-link (get-larp-link row-string)
        designers (get-designers row-string)
        designers-work (get-designers-work row-string)
        [styles-of-play pointer] (get-styles row-string)
        description (get-description row-string pointer)
        tags (get-tags row-string)]
    [title pdf-link designers designers-work styles-of-play description tags]))

(defn larps->csv [sub-page]
  (let [table [["Title" "PDF Link+url" "Designers" "Designers Work+url" "Styles of Play+key" "Description" "Tags+key"]]]
    (->> sub-page
         web-dump->rows
         (map row-maker)
         (reduce conj table)
         csv/write-csv)))

(defn csv->div [csv-string year]
  (str "<div id=\"" year "\" class=\"larps\" hidden=\"true\">" csv-string "</div>"))

(defn larps->div [[sub-page year]]
  (csv->div (larps->csv sub-page) year))

(defn old-archive->csv-div [sub-pages]
  (let [divs (map larps->div sub-pages)]
    (dorun (map #(spit "old_archives.html" % :append true) divs))))

; (ns gc.utils.larp-scrapper
;   (:require [net.cgrand.enlive-html :as html]
;             [clojure.string :as s]
;             [clojure-csv.core :as csv]))

; (defn html->hiccup
;   [html-code]
;   (if-not (string? html-code)
;     (->> (map html->hiccup (:content html-code))
;          (concat [(:tag html-code) (:attrs html-code)])
;          (keep identity)
;          vec)
;     html-code))

; (def archive-url "http://www.goldencobra.org/archives.html")

; (defn scrape [url] (html/html-resource (java.net.URL. url)))

; (def archive (scrape archive-url))

; (def submission-links 
;   (->> (html/select archive [:p [:a]])
;        (filter #(= (first (:content %)) "Submissions"))
;        (map (comp :href :attrs))
;        (map #(str "http://www.goldencobra.org/" %))))

; (def sub-pages (map scrape submission-links))

; (defn titles [sub-page]
;   (->> (html/select sub-page [:h2])
;        (map (comp first :content))))

; (defn pdf-links [sub-page]
;   (->> (html/select sub-page [:a])
;        (map (comp :href :attrs))
;        (filter #(s/starts-with?  % "http://www.goldencobra.org/pdf"))))

; (defn designers [sub-page]
;   (->> (html/select sub-page [:h3])
;        (map :content)
;        (map #(subs (first %) 3))))

; (defn designers-work [sub-page]
;   (->> (html/select sub-page [:u [:a]])
;        (map (comp :href :attrs))))

; (defn text-and-tags [sub-page]
;   (->> (html/select sub-page [:p])
;        (map :content)
;        flatten
;        (filter string?)
;        (map s/trim)
;        (drop-while #(not (s/starts-with? % "Styles of Play:")))))

; (defn get-year [sub-page]
;   (if-let [date (->> (html/select sub-page [:h1])
;                      (map :content)
;                      flatten
;                      (filter #(s/ends-with? % "Submissions"))
;                      first)]
;     (subs date 0 4)
;     (str rand)))

; (defn missing-values [col rows]
;   (into col (take (- rows (count col)) (repeat "MISSING_VALUE"))))

; (defn larps->csv [submissions]
;   (let [table [["Title" "PDF Link+url" "Designers" "Designers Work+url" "Styles of Play+key" "Description" "Tags+key"]]
;         title-col (titles submissions)
;         pdf-link-col (pdf-links submissions)
;         designers-col (designers submissions)
;         designers-work-col (designers-work submissions)
;         t-and-t (text-and-tags submissions)
;         styles-col (map #(subs % 16) (filter #(s/starts-with? % "Styles of Play:") t-and-t))
;         description-col (remove #(or (s/starts-with? % "Styles of Play:") (s/starts-with? % "Tags:")) t-and-t)
;         tags-col (map #(subs % 6) (filter #(s/starts-with? % "Tags:") t-and-t))
;         the-lot [title-col pdf-link-col designers-col designers-work-col styles-col description-col tags-col]
;         total-rows (apply max (map count the-lot))]
;     (->> the-lot
;          (map #(missing-values % total-rows))
;          (apply interleave)
;          (partition 7)
;          (map vec)
;          (reduce conj table)
;          csv/write-csv)))

; (defn csv->div [csv-string year]
;   (str "<div id=\"" year "\" class=\"larps\" hidden=\"true\">" csv-string "</div>"))

; (defn larps->div [submissions]
;   (csv->div (larps->csv submissions) (get-year submissions)))

; (defn make-csv-files [submissions]
;   (doseq [sub-page submissions]
;     (let [year (get-year sub-page)
;           filename (str #_"./gc/resources/csv/" year "-submissions.csv")
;           csv-string (larps->csv sub-page)]
;       (spit filename csv-string)
;       (println "Saving" filename))))
