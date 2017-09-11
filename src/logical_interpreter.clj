(ns logical-interpreter)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn replace-value
  "Replace new-value for old-value in intput string.
  Example: [old-value x ; new-value juan ; hijo(x,y) :- varon(x), padre(y,x)] -> hijo(juan,y) :- varon(juan), padre(y,juan)."
  [rule old-value new-value]
  (str/replace rule (re-pattern (str old-value)) new-value))

(defn build-pattern
  "Returns a regex pattern from input string."
  [my-string]
  (re-pattern (str "(?i)" my-string)))

(defn get-parameters
  "Return the parameter from input string.
  Example: padre(x, y) -> ('x' 'y')."
  [my-string]
  (let [predicative (re-find #"^[a-zA-Z]+\([^)]+\)" my-string)]
    (cond
      (= predicative nil ) nil
      :else (re-seq #"[a-zA-Z]+" (str (re-seq #"\([^)]+\)" predicative))))
    ))

(defn get-base-rule
  "Return the relation or base rule from query.
  Example: padre(jorge, juan) -> padre."
  [query]
  (first (re-seq #"[a-zA-Z]+[^(),]*" query)))

(defn get-facts
  "Returns all the facts from the database. All the valid facts must be in lowercase.
  Example: varon(juan) -> valid
           varon(JUAN) -> invalid."
  [database]
  (re-seq #"[a-z]+\([a-z, ]+\)" database))

(defn get-rules
  "Returns all the rules from the database."
  [database]
  (re-seq #"[a-z]+\([^()]+\)[ ]*:-.*\." database))

(defn get-rule
  "Return the reoule that match with my query.
  Example: hijo(jorge,juan) -> hijo(X, Y) :- varon(X), padre(Y, X)"
  [rules query]
  ( let [base-rule (get-base-rule query)]
    (re-find (re-pattern (str base-rule "[^.]+")) (str rules))))

(defn load-facts-for-rule
  "Return all the facts to find from a query."
  [rules query ]
  (def new-rule (get-rule rules query))
  (let [rule-parameters (get-parameters new-rule)
        query-parameters (get-parameters query)]
    (doseq [parameter rule-parameters]
      (def position (.indexOf rule-parameters parameter))
      (def new-rule (replace-value new-rule parameter (nth query-parameters position))))))

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
  [rules base-rule]
  (if (re-find (build-pattern base-rule) (str rules)) true false))

(defn evaluate-rule
  [rules facts query]
  (load-facts-for-rule rules query)
  (let [new-facts (re-seq #"[a-zA-Z]+\([^)]+\)" (str (re-seq #"[^:-]+$" new-rule)))]
    (set/subset? (set new-facts) (set facts))))

(defn evaluate
  "Return nil if base rule has invalid format, or if it does not exist."
  [database query]
  (let [facts (set (get-facts database))
        rules (set (get-rules database))
        base_rule (get-base-rule query)]
    (cond
      (= base_rule nil) nil
      (= (exist-base-rule? rules base_rule ) false ) nil
      :else (evaluate-rule rules facts query)
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