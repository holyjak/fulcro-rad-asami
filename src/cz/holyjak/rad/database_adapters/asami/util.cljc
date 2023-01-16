(ns cz.holyjak.rad.database-adapters.asami.util
  "INTERNAL NS. Subject to change without warning."
  (:require
    [asami.core :as d]
    [com.fulcrologic.rad.attributes :as attr]))

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
    (map f v)
    (f v)))

(def q-ident->node-id
  "Query turning an ident into Asami node id
  Usage: `(d/q q-ident->node-id db [:person/id #uuid \"...\")`"
  '[:find ?e . :where [?e :id ?ident] :in $ ?ident])

(defn ident->node-id [db ident]
  (d/q q-ident->node-id db ident))