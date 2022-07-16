(ns cz.holyjak.rad.database-adapters.asami.duplicates
  "Duplicated from the Datomic adaptor and kvstore. So the idea would be to put these functions in a library both libraries
  depend on, which would be the Fulcro RAD library itself
  See https://github.com/fulcrologic/fulcro-rad-kvstore/blob/d0dff827ee2200090d70768a9e581ba91f84f937/src/main/com/fulcrologic/rad/database_adapters/key_value/duplicates.cljc"
  (:require
    [cz.holyjak.rad.database-adapters.asami-options :as aso] ; FIXME rename
    [com.fulcrologic.rad.attributes :as attr]
    [com.wsscode.pathom.core :as p]
    [taoensso.encore :as enc]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [clojure.spec.alpha :as s]
    #?(:clj [clojure.core.async :as async :refer [<!!]])
    #?(:cljs [cljs.core.async :as async :refer [<! go]])
    [taoensso.timbre :as log]))

;; cz.holyjak.rad.database-adapters.datomic/datomic-cloud
(defn pathom-plugin
  "A pathom plugin that adds the necessary ::aso connections/databases to the pathom env for
  a given request. Requires a database-mapper, which is a
  `(fn [pathom-env] {schema-name connection})` for a given request.

  The resulting pathom-env available to all resolvers will then have:

  - `aso/connections`: The result of database-mapper
  - `aso/databases`: A map from schema name to atoms holding a database. The atom is present so that
  a mutation that modifies the database can choose to update the snapshot of the db being used for the remaining
  resolvers.

  This plugin should run before (be listed after) most other plugins in the plugin chain since
  it adds connection details to the parsing env.

  This code is similar to the datomic-plugin's."
  [database-mapper]
  (p/env-wrap-plugin
    (fn [env]
      (let [database-connection-map (database-mapper env)
            databases               (into {}
                                          (map (fn [[k v]]
                                                 [k (atom v)]))
                                          database-connection-map)]
        (assoc env
          aso/connections database-connection-map
          aso/databases databases)))))

;; cz.holyjak.rad.database-adapters.datomic-common
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

;; cz.holyjak.rad.database-adapters.datomic-common
(defn schemas-for-delta
  "Copied from or very similar to datomic function of same name"
  [{::attr/keys [key->attribute]} delta]
  (let [all-keys (keys-in-delta delta)
        schemas  (into #{}
                       (keep #(-> % key->attribute ::attr/schema))
                       all-keys)]
    schemas))

;; cz.holyjak.rad.database-adapters.datomic-common
(defn tempids->generated-ids
  "Copied from or very similar to datomic function of same name"
  [unwrap-id {::attr/keys [key->attribute] :as env} delta]
  (let [idents                      (keys delta)
        fulcro-tempid->generated-id (into {} (keep (fn [[k id :as ident]]
                                                     (when (tempid/tempid? id)
                                                       [id (unwrap-id env k id)])) idents))]
    fulcro-tempid->generated-id))

;; cz.holyjak.rad.database-adapters.datomic-common
(defn tempid->intermediate-id
  "Copied from or very similar to datomic function of same name, except rid of specter use"
  [tempids-in-delta delta]
  (let [tempids                        (tempids-in-delta delta)
        fulcro-tempid->intermediate-id (into {} (map (fn [t] [t (str (:id t))]) tempids))]
    fulcro-tempid->intermediate-id))

;; No duplicate found, perhaps been refactored out of existence
(>defn delta->tempid-maps
  "Copied from or very similar to datomic function of same name"
  [tempids-in-delta unwrap-id env delta]
  [fn? fn? map? map? => map?]
  (let [tempid->txid         (tempid->intermediate-id tempids-in-delta delta)
        tempid->generated-id (tempids->generated-ids unwrap-id env delta)]
    {:tempid->string       tempid->txid
     :tempid->generated-id tempid->generated-id}))

;; cz.holyjak.rad.database-adapters.datomic/datomic-cloud
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
