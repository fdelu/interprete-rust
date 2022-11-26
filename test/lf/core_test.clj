(ns lf.core-test
  (:require [clojure.test :refer :all]
            [lf.core :refer :all]))

(deftest test-listar
    (let 
      [
        nl (with-out-str (prn))
        actual (with-out-str (listar (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol "}"))))
        expected (str "fn main ( )" nl "{" nl "  println! ( \"Hola, mundo!\" )" nl "}" nl)
      ]
      (is (= actual expected))
    )
)

