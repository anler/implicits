(ns implicits.core
  (:refer-clojure :exclude [defn])
  (:require [implicits.core :as implicits]))

(defmacro defimplicit
  "Like def, but defines an implicit value."
  [sym & rest]
  `(def ~(with-meta sym {:dynamic true, :implicit true})
     ~@rest))

(defmacro implicitly
  [& bindings]
  `(list ::implicitly ~(into {} (map (fn [[sym value]] [(resolve sym) value]) (partition 2 bindings)))))

(defn- with-implicits
  [implicits f]
  (fn [& args]
    (let [implicit-params? (and (coll? (first args))
                                (= ::implicitly (first (first args))))
          bindings         (if implicit-params?
                             (merge implicits (second (first args)))
                             implicits)
          fargs            (if implicit-params?
                             (rest args)
                             args)]
      (with-bindings* bindings #(apply f %) fargs))))

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
