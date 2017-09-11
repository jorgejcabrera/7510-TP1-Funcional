(ns logical-interpreter)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn replace-value
  [roule old-value new-value]
  (str/replace roule (re-pattern (str old-value)) new-value))

(defn build-pattern
  "Returns a regex pattern from input string."
  [my-string]
  (re-pattern (str "(?i)" my-string)))

(defn get-parameters
  [my-string]
  (let [predicative (re-find #"^[a-zA-Z]+\([^)]+\)" my-string)]
    (cond
      (= predicative nil ) nil
      :else (re-seq #"[a-zA-Z]+" (str (re-seq #"\([^)]+\)" predicative))))
    ))

(defn get-base-roule
  [query]
  (first (re-seq #"[a-zA-Z]+[^\(\),]*" query)))

(defn get-facts
  "Returns all the facts from the rows"
  [database]
  (re-seq #"[a-z]+\([^()]+\)" database))

(defn get-roules
  "Returns all the roules from the rows"
  [database]
  (re-seq #"[a-z]+\([^()]+\)[ ]*:-.*\." database))

(defn get-roule
  [roules query]
  ( let [base-roule (get-base-roule query)]
    (re-find (re-pattern (str base-roule "[^.]+")) (str roules))))

(defn get-facts-for-roule
  "Return all the facts to find from a query."
  [roules query ]
  (def new-roule (get-roule roules query))
  (let [roule-parameters (get-parameters new-roule)
        query-parameters (get-parameters query)]
    (doseq [parameter roule-parameters]
      (def position (.indexOf roule-parameters parameter))
      (def new-roule (replace-value new-roule parameter (nth query-parameters position))))))

(defn invalid-query?
  "Returns true if query has invalid format."
  [query]
  (str/blank? (re-find #"[a-zA-Z]+\([^()]+\)" query)))

(defn invalid-database?
  "Returns true if database has invalid format."
  [database]
  (if (str/blank? (re-find #".*[^\.]$" (str/trim database))) false true))

(defn exist-base-rule?
  "Return true if base rule does not exist."
  [roules base-rule]
  (if (re-find (build-pattern base-rule) (str roules)) true false))

(defn evaluate-roule
  [roules facts query]
  (get-facts-for-roule roules query)
  (let [new-facts (re-seq #"[a-zA-Z]+\([^)]+\)" (str (re-seq #"[^:-]+$" new-roule)))]
    (set/subset? (set new-facts) (set facts))))

(defn evaluate
  "Return nil if base rule has invalid format, or if it does not exist. "
  [database query]
  (let [facts (set (get-facts database))
        roules (set (get-roules database))
        base_roule (get-base-roule query)]
    (cond
      (= base_roule nil) nil
      (= (exist-base-rule? roules base_roule ) false ) nil
      :else (evaluate-roule roules facts query)
      )))

(defn execute-query
  [database query]
  (let [facts (set (get-facts database))]
    (cond
      (contains? facts query) true
      :else (evaluate database query)
      )))

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (cond
    (or (invalid-query? query) (invalid-database? database)) nil
    :else (execute-query database query)
    ))