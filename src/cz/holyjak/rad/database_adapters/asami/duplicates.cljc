(ns cz.holyjak.rad.database-adapters.asami.duplicates
  "Duplicated from the Datomic adapter and kvstore. So the idea would be to put these functions in a library both libraries
  depend on, which would be the Fulcro RAD library itself
  See https://github.com/fulcrologic/fulcro-rad-kvstore/blob/d0dff827ee2200090d70768a9e581ba91f84f937/src/main/com/fulcrologic/rad/database_adapters/key_value/duplicates.cljc"
  (:require
    [com.fulcrologic.rad.attributes :as attr]
    [taoensso.encore :as enc]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [taoensso.timbre :as log]))

(defn keys-in-delta
  "Copied from or very similar to datomic function of same name"
  [delta]
  (let [id-keys  (into #{}
                       (map first)
                       (keys delta))
        all-keys (into id-keys
                       (mapcat keys)
                       (vals delta))]
    all-keys))

(defn schemas-for-delta
  "Copied from or very similar to datomic function of same name"
  [{::attr/keys [key->attribute]} delta]
  (let [all-keys (keys-in-delta delta)
        schemas  (into #{}
                       (keep #(-> % key->attribute ::attr/schema))
                       all-keys)]
    schemas))

(defn tempids->generated-ids
  "Copied from or very similar to datomic function of same name"
  [unwrap-id {_ ::attr/key->attribute :as env} delta]
  (let [idents                      (keys delta)
        fulcro-tempid->generated-id (into {} (keep (fn [[k id :as ident]]
                                                     (when (tempid/tempid? id)
                                                       [id (unwrap-id env k id)])) idents))]
    fulcro-tempid->generated-id))

(defn generate-resolvers
  "Generate all of the resolvers that make sense for the given database config. This should be passed
  to your Pathom parser to register resolvers for each of your schemas. Just a copy from the datomic adapter.
  Although `id-resolver` isn't"
  [id-resolver attributes schema]
  (let [attributes            (filter #(= schema (::attr/schema %)) attributes)
        key->attribute        (attr/attribute-map attributes)
        entity-id->attributes (group-by ::k (mapcat (fn [attribute]
                                                      (map
                                                        (fn [id-key] (assoc attribute ::k id-key))
                                                        (get attribute ::attr/identities)))
                                                    attributes))
        entity-resolvers      (reduce-kv
                                (fn [result k v]
                                  (enc/if-let [attr (key->attribute k)
                                               resolver (id-resolver attributes attr v)]
                                              (conj result resolver)
                                              (do
                                                (log/error "Internal error generating resolver for ID key" k)
                                                result)))
                                []
                                entity-id->attributes)]
    entity-resolvers))
