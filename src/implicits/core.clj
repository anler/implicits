(ns implicits.core
  (:refer-clojure :exclude [defn]))

(defmacro defimplicit
  "Like def, but defines an implicit value."
  [sym & rest]
  `(def ~(with-meta sym {:dynamic true, :implicit true})
     ~@rest))

(defn- with-implicits
  [implicits f]
  (fn [& args]    
    (with-bindings* (if (and (coll? (first args))
                             (= 'implicitly (first (first args))))
                      (merge implicits (into {} (map (fn [[sym val]]
                                                       [(resolve sym) val])
                                                     (partition 2 (rest (first args))))))
                      implicits)
      (fn [args]
        (apply f args))
      (if (and (coll? (first args))
               (= 'implicitly (first (first args))))
        (rest args)
        args))))

(defmacro defn
  "Similar to clojure.core/defn but it uses two parameter lists where
  the first is used to define which implicit parameters this function
  will use. When calling this function, normal parameters will be
  bound to the second parameter list, if the first parameter though is
  of the form (implicitly param...), those params will be bound to the
  first parameter (implicit parameters) list."
  [& fdecl]
  (let [[fname & fdecl]         fdecl
        [docstring fdecl]       (if (string? (first fdecl))
                                  [(first fdecl) (rest fdecl)]
                                  ["" fdecl])
        [attr-map fdecl]        (if (map? (first fdecl))
                                  [(first fdecl) (rest fdecl)]
                                  [{} fdecl])
        [implicit-params fdecl] [(first fdecl) (rest fdecl)]
        implicits#              (into {} (map #(if-let [v (resolve %)]
                                                 [v (deref v)]
                                                 (throw (ex-info "Implicit param not found."
                                                                 {:cause :implicit-param-not-found
                                                                  :param %})))
                                              implicit-params))]
    `(def ~(with-meta fname
             (assoc attr-map
                    :doc (str docstring "\n Implicits: " implicit-params)
                    ;; TODO: arglists metadata
                    ))
       (with-implicits
         ~implicits#
         (fn ~@fdecl)))))

(comment
  (defimplicit x 3)
  (macroexpand-1
    '(defn hello [x] []
       x))

  (defn hello [x] []
    x)

  (hello '(implicitly x 5))
  )
