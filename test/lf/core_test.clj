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

(deftest test-agregar-ptocoma
    (let 
      [
        actual (agregar-ptocoma (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}")))
        expected (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") (symbol ";") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}"))
      ]
      (is (= actual expected))
    )
)

(deftest test-palabra-reservada
  (is (palabra-reservada? 'while)) 
  (is (not (palabra-reservada? 'until)))  
  (is (not (palabra-reservada? 13)))
)

(deftest test-identificador?
  (is (identificador? 'boolean))
  (is (not (identificador? 'bool)))
  (is (identificador? 'e120))
  (is (not (identificador? '12e0)))
)

(deftest test-dump
    (let 
      [
        nl (with-out-str (prn))
      ]
      (is (= 
           (with-out-str (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG]))
           (str "0 [POPREF 2]" nl "1 [PUSHFI 2]" nl "2 MUL" nl "3 [PUSHFI 1]" nl "4 ADD" nl "5 NEG" nl)
          )
      )
      (is (= 
           (with-out-str (dump '[HLT]))
           (str "0 HLT" nl)
          )
      )
      (is (= 
           (with-out-str (dump nil))
           (str "0 nil" nl)
          )
      )
    )
)

(deftest test-ya-declarado-localmente?
  (is (ya-declarado-localmente? 'Write [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]]))
  (is (not (ya-declarado-localmente? 'Read [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])))
  (is (ya-declarado-localmente? 'Write [[0 1] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]]))
  (is (not (ya-declarado-localmente? 'Write [[0 2] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])))
)

(deftest test-cargar-const-en-tabla
  (let 
    [con-errores [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []]]
    (is (= (cargar-const-en-tabla con-errores) con-errores))
  )
  (let
    [
      sin-errores [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []]
      expected [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0] ['TRES ['const 'i64] 3]]] 0 [['CAL 0] 'HLT] []]
    ]
    (is (= (cargar-const-en-tabla sin-errores) expected))
  )
)