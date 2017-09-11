(ns helpers-function-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))
(require '[clojure.set :as set])

(def database "
  varon(juan).
  varon(pepe).
  varon(JUAN)
  padre(roberto, cecilia).
  trillizas(ana, maria, lisa).
  hijo(X, Y) :- varon(X), padre(Y, X).
  hija(X, Y) :- mujer(X), padre(Y, X).
")

(deftest helpers-function-replace-test
  (testing "reemplanzar valor deberia funcionar bien."
    (is (= (replace-value "hija(X, Y) :- mujer(X), padre(Y, X)" "X" "jorge")
          "hija(jorge, Y) :- mujer(jorge), padre(Y, jorge)"))))

(deftest helpers-function-parameters-test
  (testing "obtener primer parametro deberia ser x."
    (is (= (nth (get-parameters "padre(x,y)") 0)
      "x")))
  (testing "obtener segundo parametro deberia ser y."
    (is (= (nth (get-parameters "padre(x,y)") 1)
      "y")))
  (testing "obtener parametro de entrada invalida deberia ser nil."
    (is (= (get-parameters "padrex,y)")
      nil))))

(deftest helpers-function-base-rule-test
  (testing "obtener regla base de entrada valida deberia funcionar bien."
    (is (= (get-base-rule "padre(jorge, juan)")
          "padre"))))

(deftest helpers-function-get-rules-test
  (testing "obtener reglas de la db deberia funcionar bien."
    (is (= (set/subset? (set '("hijo(X, Y) :- varon(X), padre(Y, X)." "hija(X, Y) :- mujer(X), padre(Y, X).")) (set (get-rules database)))
          true))))

(deftest helpers-function-get-facts-test
  (testing "obtener facts de la db deberia funcionar bien."
    (is (= (set/subset? (set '("varon(juan)" "varon(pepe)" "padre(roberto, cecilia)" "trillizas(ana, maria, lisa)")) (set (get-facts database)))
          true))))

(deftest helpers-function-get-rule-test
  (testing "obtener regla correspondiente a query hijo(jorge,juan)."
    (is (= (get-rule (get-rules database) "hijo(jorge,juan)")
          "hijo(X, Y) :- varon(X), padre(Y, X)")))
  (testing "obtener regla correspondiente a query hija(maria,juan)."
    (is (= (get-rule (get-rules database) "hija(jorge,juan)")
          "hija(X, Y) :- mujer(X), padre(Y, X)")))
  (testing "obtener regla inexistente correspondiente a query padrastro(maria,juan) deberia devolver nil."
    (is (= (get-rule (get-rules database) "padrastro(maria,juan)")
          nil))))

(deftest helpers-function-invalid-query-test
  (testing "invalid query deberia devolver true para query malaquery."
    (is (= (invalid-query? "malaquery" )
          true)))
  (testing "invalid query deberia devolver false para query buena(query)."
    (is (= (invalid-query? "buena(query)" )
          false)))
  (testing "invalid query deberia devolver false para query buena(query,bonita)."
    (is (= (invalid-query? "buena(query,bonita)" )
          false))))

(deftest helpers-function-exist-base-rule-test
  (testing "exist base rule deberia devolver true para base-rule padre."
    (is (= (exist-base-rule? (get-rules database) "padre")
          true)))
  (testing "exist base rule deberia devolver false para base-rule fruta."
    (is (= (exist-base-rule? (get-rules database) "fruta")
          false))))