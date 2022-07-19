(ns cz.holyjak.rad.database-adapters.asami.util
  (:require
    [com.fulcrologic.rad.attributes :as attr]))

(defn ensure! [x]
  (assert x)
  x)

(defn to-one? [{::attr/keys [key->attribute]} k]            ; copied from datomic-common
  (when key->attribute (not (boolean (some-> (get key->attribute k) (attr/to-many?))))))

(defn ref? [{::attr/keys [key->attribute]} k]               ; copied from datomic-common
  (when key->attribute (= :ref (some-> k key->attribute ::attr/type))))

(defn id? [{::attr/keys [key->attribute]} k]
  (when key->attribute (some-> k key->attribute ::attr/identity?)))

(defn update-attr-val
  "Apply the fn `f` to each value of the attribute `k` based on whether it is singular or multi-value"
  [{::attr/keys [key->attribute] :as env} k f v]
  {:pre [(get key->attribute k)]}
  (if (to-one? k env)
    (f v)
    (apply f v)))