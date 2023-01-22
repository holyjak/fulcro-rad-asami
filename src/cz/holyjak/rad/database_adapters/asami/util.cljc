(ns cz.holyjak.rad.database-adapters.asami.util
  "INTERNAL NS. Subject to change without warning."
  (:require
    [asami.core :as d]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [com.fulcrologic.guardrails.core :refer [>defn =>]]
    [com.fulcrologic.rad.attributes :as attr]
    [taoensso.timbre :as log]))

(>defn env->asami
       "Find Asami from the Pathom env, optionally given a schema and whether required from connections or databases"
       ([env schema asami-entry]
        [map? keyword? keyword? => any?]
        (or (cond
              (= asami-entry aso/connections) (some-> (get-in env [asami-entry schema]))
              (= asami-entry aso/databases) (some-> (get-in env [asami-entry schema]) deref))
            (log/error (str "No "
                            (if (= asami-entry aso/databases)
                              "database atom"
                              "connection")
                            " for schema: " schema))))
       ([env]
        [map? => any?]
        (env->asami env :production))
       ([env schema]
        [map? keyword? => any?]
        (env->asami env schema aso/databases)))

(defn ensure!
  ([x]
   (assert x)
   x)
  ([x msg]
   (assert x msg)
   x)
  ([x pred ^String msg]
   (assert (pred x) msg)
   x))

(defn to-one? [{::attr/keys [key->attribute]} k]            ; copied from datomic-common
  (when key->attribute (not (boolean (some-> (get key->attribute k) (attr/to-many?))))))

(def to-many? (complement to-one?))

(defn ref? [{::attr/keys [key->attribute]} k]               ; copied from datomic-common
  (when key->attribute (= :ref (some-> k key->attribute ::attr/type))))

(defn id? [{::attr/keys [key->attribute]} k]
  (when key->attribute (some-> k key->attribute ::attr/identity?)))

(defn map-over-many-or-one [many? f v]
  (if many?
    (mapv f v) ; BEWARE: Pathom3 requires that attribute values are not lazy sequences; breaks tests otherwise
    (f v)))

(def q-ident->node-id
  "Query turning an ident into Asami node id
  Usage: `(d/q q-ident->node-id db [:person/id #uuid \"...\")`"
  '[:find ?e . :where [?e :id ?ident] :in $ ?ident])

(defn ident->node-id [db ident]
  (d/q q-ident->node-id db ident))

(defn deep-merge
  "Merges nested maps without overwriting existing keys."
  [& xs]
  (if (every? map? xs)
    (apply merge-with deep-merge xs)
    (last xs)))