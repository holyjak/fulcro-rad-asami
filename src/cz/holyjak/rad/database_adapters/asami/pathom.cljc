(ns cz.holyjak.rad.database-adapters.asami.pathom
  (:require
    #?(:cljs [cz.holyjak.rad.database-adapters.asami.tmp-update-vals :refer [update-vals]])
    [asami.core :as d]
    [clojure.pprint :refer [pprint]]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [cz.holyjak.rad.database-adapters.asami.duplicates :as dups]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.form :as form]
    [cz.holyjak.rad.database-adapters.asami :as-alias asami]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [cz.holyjak.rad.database-adapters.asami.core :as asami-core]
    [cz.holyjak.rad.database-adapters.asami.query :as query]
    [cz.holyjak.rad.database-adapters.asami.write :as write]
    [com.fulcrologic.rad.authorization :as auth]
    [com.wsscode.pathom.connect :as pc]
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
                 {:keys [::attr/schema]} (key->attribute pk)
                 connection (env->asami env schema aso/connections)]
                (do
                  (log/info "Deleting" ident)
                  (let [database-atom (get-in env [aso/databases schema])]
                    @(write/retract-entity connection ident) ; we assume the entity has {:id <ident>}
                    (when database-atom
                      (reset! database-atom (d/db connection)))
                    {}))
                (log/warn "Datomic adapter failed to delete " params)))

(defn save-form!                                            ; FIXME check impl
  "Do all of the possible operations for the given form delta (save to the database involved)"
  [env {::form/keys [delta] :as save-params}]
  (let [schemas (dups/schemas-for-delta env delta)
        result (atom {:tempids {}})
        tx-with-ref2new (fn [[_op eid _prop value :as triplet-or-map-tx]]
                          (or (-> eid meta ::write/new?)
                              (-> value meta ::write/new?)))]
    (log/debug "Saving form across " schemas)
    (doseq [schema schemas
            :let [connection (env->asami env schema aso/connections)
                  ;; NOTE: tempid = Fulcro tempid;
                  ;;       generated-id = the (uu)id that we generated as the ID of the new entity
                  {:keys [tempid->generated-id txn]} (write/delta->txn env schema delta)]]

      (log/debug "Saving form delta" (with-out-str (pprint delta)) "on schema" schema)
      (let [{txs1 false, txs2 true}
            (group-by
              (every-pred vector? tx-with-ref2new)
              txn)]
        (doseq [txn' [txs1 txs2]]
          (log/debug "Running txn\n" (with-out-str (pprint txn')))
          (if (and connection (seq txn'))
            (try
              (let [database-atom (get-in env [aso/databases schema])
                    ;; Mapping from Asami tempid (e.g. -1) to the assigned :db/id (e.g. #a/n [33])
                    #_#_{txid->db-id :tempids} @(d/transact connection txn')]
                @(d/transact connection txn')
                (when database-atom
                  (reset! database-atom (d/db connection)))
                (swap! result update :tempids merge tempid->generated-id))
              (catch #?(:clj Exception :cljs :default) e
                (log/error e "Transaction failed!")
                {}))
            (log/error "Unable to save form. Either connection was missing in env, or txn was empty.")))))
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
     (let [save-result (save-form! pathom-env params)]
       save-result)))
  ([handler]
   (fn [{::form/keys [params] :as pathom-env}]
     (let [save-result    (save-form! pathom-env params)
           handler-result (handler pathom-env)]
       (deep-merge save-result handler-result)))))

(defn wrap-delete
  "Form delete middleware to accomplish deletes."
  ([handler]
   (fn [{::form/keys [params] :as pathom-env}]
     (let [local-result   (delete-entity! pathom-env params)
           handler-result (handler pathom-env)]
       (deep-merge handler-result local-result))))
  ([]
   (fn [{::form/keys [params] :as pathom-env}]
     (delete-entity! pathom-env params))))

(defn id-resolver
  "Generates a resolver from `id-attribute` to the `output-attributes`."
  [all-attributes
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
     ::pc/resolve (cond-> (fn [{::attr/keys [key->attribute] :as env} input]
                            (log/debug "In resolver:" qualified-key "inputs:" (vec input))
                            (let [db (env->asami env schema aso/databases)]
                              (->> (query/entity-query
                                     (assoc env ::asami/id-attribute id-attribute)
                                     input
                                     db)
                                   (auth/redact env))))
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

(defn pathom-plugin [database-mapper]
  (dups/pathom-plugin (fn [env] (update-vals (database-mapper env) d/db))))

(comment
  (def *env (mock-resolver-env :production (d/connect (asami-core/config->url {:asami/driver :local, :asami/database "playground3"}))))

  (query/entity-query
    (assoc *env ::asami/id-attribute {::attr/qualified-key :order/id})
    {:order/id 1}
    (env->asami *env))

  (delete-entity! (assoc *env
                    ::attr/key->attribute
                    {:order/id {::attr/schema :production}})
                  {:order/id 1})
  ,)