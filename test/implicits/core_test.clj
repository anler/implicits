(ns implicits.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [implicits.core :as implicits]))

(deftest defimplicit-test
  (testing "defimplicit should accept a docstring"
    (let [syntax    (macroexpand-1 '(implicits/defimplicit :a "A docstring."))
          docstring (:doc (meta (second syntax)))]
      (is (s/includes? docstring "A docstring."))))

  (testing "defimplicit should accept an initial value"
    (let [syntax (macroexpand-1 '(implicits/defimplicit :a 3))
          value  (last syntax)]
      (is (= value 3))))

  (testing "defimplicit should  define a var in the current ns and register that implicit"
    (implicits/defimplicit ::my-implicit)
    (is (contains? (implicits/implicits)
                   ::my-implicit))
    (is (nil? my-implicit))))

(deftest defn-test
  (testing "defn should accept a docstring"
    (let [syntax    (macroexpand-1 '(implicits/defn f "A docstring." [] []))
          docstring (:doc (meta (second syntax)))]
      (is (s/includes? docstring "A docstring.")))))
