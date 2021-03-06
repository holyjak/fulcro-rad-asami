(ns cz.holyjak.rad.database-adapters.asami.pathom
  "Pathom integration for Asami"
  (:require
    [asami.core :as d]
    [edn-query-language.core :as eql]
    [clojure.pprint :refer [pprint]]
    [com.fulcrologic.guardrails.core :refer [>defn =>]]
    [cz.holyjak.rad.database-adapters.asami.duplicates :as dups]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.form :as form]
    [cz.holyjak.rad.database-adapters.asami :as-alias asami]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [cz.holyjak.rad.database-adapters.asami.connect :as asami-core]
    [cz.holyjak.rad.database-adapters.asami.read :as query]
    [cz.holyjak.rad.database-adapters.asami.util :as util :refer [ref?]]
    [cz.holyjak.rad.database-adapters.asami.write :as write]
    [com.fulcrologic.rad.authorization :as auth]
    [com.wsscode.pathom.connect :as pc]
    [com.wsscode.pathom.core :as p]
    [taoensso.encore :as enc]
    [taoensso.timbre :as log]))

(>defn ^:private env->asami
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

(defn delete-entity!
    "Delete the given entity, if possible."
    [{;:asami/keys [transact]
      ::attr/keys [key->attribute] :as env} params]
    (enc/if-let [pk (ffirst params)  ; params = e.g. {:order/id 1} ?
                 id (get params pk)
                 ident [pk id]
                 {::attr/keys [schema]} (key->attribute pk)
                 connection (env->asami env schema aso/connections)]
                (do
                  (log/info "Deleting" ident)
                  (let [database-atom (get-in env [aso/databases schema])
                        {:keys [db-after]} @(write/retract-entity connection ident)] ; we assume the entity has {:id <ident>}
                    (some-> database-atom (reset! db-after))
                    {})) ; I guess we return {} instead of nil to make Pathom understand it succeeded (=> no "not found")
                (log/warn "Datomic adapter failed to delete " params)))

(defn save-form!
  "Do all of the possible operations for the given form delta (save to the database involved)"
  [env {::form/keys [delta] :as _save-params}]
  (let [schemas (dups/schemas-for-delta env delta)
        result (volatile! {:tempids {}})]
    (log/debug "Saving form across " schemas)
    (doseq [schema schemas
            :let [connection (env->asami env schema aso/connections)
                  ;; NOTE: tempid = Fulcro tempid;
                  ;;       generated-id = the (uu)id that we generated as the ID of the new entity
                  {:keys [tempid->generated-id txn]} (write/delta->txn env schema delta)]]

      (log/debug "Saving form delta" (with-out-str (pprint delta)) "on schema" schema)
      (if (and connection (seq txn))
        (try
          (log/debug "Running txn\n" (with-out-str (pprint txn)))
          (let [database-atom (get-in env [aso/databases schema])]
            @(d/transact connection txn)
            (some-> database-atom (reset! (d/db connection)))
            (vswap! result update :tempids merge tempid->generated-id))
          (catch #?(:clj Exception :cljs :default) e
            (log/error e "Transaction failed!")
            {}))
        (log/error "Unable to save form:"
                   (cond
                     (not connection) (str "connection is missing in env for schema " schema "; has: " (keys (get env aso/connections)))
                     (empty? txn) "the transaction is empty"))))
    @result))

(defn- deep-merge
  "Merges nested maps without overwriting existing keys."
  [& xs]
  (if (every? map? xs)
    (apply merge-with deep-merge xs)
    (last xs)))

(defn wrap-save
  "Form save middleware to accomplish saves."
  ([]
   (fn [{::form/keys [params] :as pathom-env}]
     (save-form! pathom-env params)))
  ([handler]
   (fn [{::form/keys [params] :as pathom-env}]
     (let [save-result    (save-form! pathom-env params)
           handler-result (handler pathom-env)]
       (deep-merge save-result handler-result)))))

(defn wrap-delete
  "Form delete middleware to accomplish deletes."
  ([]
   (fn [{::form/keys [params] :as pathom-env}]
     (delete-entity! pathom-env params)))
  ([handler]
   (fn [{::form/keys [params] :as pathom-env}]
     (let [delete-result   (delete-entity! pathom-env params)
           handler-result (handler pathom-env)]
       (deep-merge handler-result delete-result)))))

(defn id-resolver
  "Generates a resolver from `id-attribute` to the `output-attributes`."
  [_all-attributes
   {::attr/keys [qualified-key] :keys [::attr/schema ::asami/wrap-resolve] :as id-attribute}
   output-attributes]
  [::attr/attributes ::attr/attribute ::attr/attributes => ::pc/resolver]
  (let [outputs          (attr/attributes->eql output-attributes)
        resolve-sym      (symbol
                           (str (namespace qualified-key))
                           (str (name qualified-key) "-resolver"))
        with-resolve-sym (fn [r]
                           (fn [env input]
                             (r (assoc env ::pc/sym resolve-sym) input)))]
    (log/info "Building ID resolver for" qualified-key "outputs" outputs)
    {::pc/sym resolve-sym
     ::pc/input #{qualified-key}
     ::pc/output outputs
     ::pc/batch? true
     ::pc/resolve (cond-> (fn [{_ ::attr/key->attribute :as env} input]
                            (let [batch? (sequential? input)
                                  db (env->asami env schema aso/databases)]
                              (log/debug "In resolver:" qualified-key "inputs:" (cond-> input (instance? clojure.lang.LazySeq input) vec) "db ver:" (d/as-of-t db))
                              (->> (query/entities
                                     (assoc env ::asami/id-attribute id-attribute)
                                     input
                                     db)
                                   (util/map-over-many-or-one batch? #(auth/redact env %)))))
                          wrap-resolve (wrap-resolve)
                          :always (with-resolve-sym))}))

(defn mock-resolver-env
  "Returns a mock env that has the aso/connections and aso/databases keys that would be present in
  a properly-set-up pathom resolver `env` for a given single schema. This should be called *after*
  you have seeded data against a `connection` that goes with the given schema.
  * `schema` - A schema name
  * `connection` - A database connection that is connected to a database with that schema."
  [schema connection]
  {aso/connections {schema connection}
   aso/databases   {schema (atom (d/db connection))}})

(defn generate-resolvers [attributes schema]
  (dups/generate-resolvers id-resolver attributes schema))

(defn pathom-plugin
  "Instantiate a pathom plugin for Asami connections and DB.
  - `database-mapper` - a `(fn [env]) -> map from a schema to an Asami connection"
  [database-mapper]
  (p/env-wrap-plugin
    (fn [env]
      (let [database-connection-map (database-mapper env)
            databases               (update-vals database-connection-map (comp atom d/db util/ensure!))]
        (assoc env
          aso/connections database-connection-map
          aso/databases databases)))))

(comment
  (def *env (mock-resolver-env :production (d/connect (asami-core/config->url {:asami/driver :local, :asami/database "playground3"}))))

  (query/entities
    (assoc *env ::asami/id-attribute {::attr/qualified-key :order/id})
    {:order/id 1}
    (env->asami *env))

  (delete-entity! (assoc *env
                    ::attr/key->attribute
                    {:order/id {::attr/schema :production}})
                  {:order/id 1})
  ,)