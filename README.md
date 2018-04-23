# implicits

**This is highly experimental and in a big WIP state**.

A Clojure library for defining functions with implicit parameters.

## Usage

``` clojure
(require '[implicits.core :as i])


(i/defimplicit subject "My implicit value" "World")

(clojure.repl/doc subject)
;; -------------------------
;; implicits.core/subject
;;   My implicit value
;; => nil

(i/defn say-hello
  [subject]
  [& [message]]
  (format (or message "Hello, %s!") subject))

(say-hello)
;; => "Hello, World!"

(say-hello (i/implicitly subject "Planet"))
;; => "Hello, Planet!"

(say-hello "Hi, %s!")
;; => "Hi, World!"

(say-hello (i/implicitly subject "Planet") "Hi, %s!")
;; => "Hi, Planet!"
```

## License

Copyright © 2018 Anler Hernández Peral

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
