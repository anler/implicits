(ns implicits.core
  (:refer-clojure :exclude [defn]))

(defonce ^:private ^:dynamic *registry* (atom {}))

(def implicits
  "Returns a map of currently-defined implicits."
  (fn []
    @*registry*))

(defn- normalize [sym]
  (symbol (or (namespace sym)
              (str *ns*))
          (name sym)))

(defmacro defimplicit
  "Define an implicit value named by the provided symbol."
  [sym & [docstring init]]
  (let [[docstring value] (if (and (string? docstring)
                                   (< (count &form) 4))
                            ["" docstring]
                            [docstring init])]
    (swap! *registry* assoc (normalize sym) value)
    `(def ~(with-meta sym
             {:doc (str docstring "\n" "Implicit value.")
              ::implicit true})
       ~value)))

(defn- with-implicit-params
  [implicits f]
  (fn [& args]
    (with-bindings* implicits
      (fn []
        (apply f args)))))

(defmacro defn
  "Similar to clojure.core/defn but it uses two parameter lists where
  the first is used to define which implicit parameters this function
  will use. When calling this function, normal parameters will be
  bound to the second parameter list, if the first parameter though is
  of the form (implicitly param...), those params will be bound to the
  first parameter (implicit parameters) list."
  [& defn-args]
  (let [[fname & fdecl]         fdecl
        [docstring fdecl]       (if (string? (first fdecl))
                                  [(first fdecl) (rest fdecl)]
                                  ["" fdecl])
        [attr-map fdecl]        (if (map? (first fdecl))
                                  [(first fdecl) (rest fdecl)]
                                  [{} fdecl])
        [implicit-params fdecl] [(first fdecl) (rest fdecl)]
        evidences               (implicits)
        implicit-param-values   (reduce conj (map #(if (contains? evidences %)
                                                     [(-> % name symbol) (% evidences)]
                                                     (throw (ex-info "Implicit param not found."
                                                                     {:cause :implicit-param-not-found
                                                                      :param %})))
                                                  implicit-params))]
    ;; {:arglists (map #(cons '(implicitly implicit-params)?))}
    `(with-implicits
       ~implicit-params-values
       (fn ~(with-meta fname
             (assoc attr-map
                    :doc (str docstring "\n Implicits: " implicit-params)))
         ~@fdecl))))

(comment
  (defn hello [::x] []
    x)
  (hello (list implicitly ::x "John"))
  (macroexpand-1 '(hello (implicitly ::x "John")))
  (macroexpand-1
    '(defn hello [::x] []
       x))
  (defn x [f & [implicits args]]
    (if (= 'implicits (first implicits))
      (binding ) ;; call f with implicits
      ))

  (macroexpand-1
    '(defimplicit ::x ""))

  (do (defimplicit ::x "John")
      (defn hello-msg [::x] []
        (str "Hello there, " x))
      (hello-msg))
  )
