(ns cz.holyjak.rad.database-adapters.asami.pathom-common
  "High-level RAD <> Asami integration functions for changing the DB,
   independent of Pathom"
  (:require
    [asami.core :as d]
    [clojure.pprint :refer [pprint]]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.authorization :as auth]
    [com.fulcrologic.rad.form :as form]
    [cz.holyjak.rad.database-adapters.asami :as-alias asami]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [cz.holyjak.rad.database-adapters.asami.duplicates :as dups]
    [cz.holyjak.rad.database-adapters.asami.read :as query]
    [cz.holyjak.rad.database-adapters.asami.util :as util]
    [cz.holyjak.rad.database-adapters.asami.write :as write]
    [com.wsscode.misc.coll :as misc.coll]
    [taoensso.encore :as enc]
    [taoensso.timbre :as log]))

(defn delete-entity!
  "Delete the given entity, if possible."
  [{;:asami/keys [transact]
    ::attr/keys [key->attribute] :as env} params]
  (enc/if-let [pk (ffirst params)  ; params = e.g. {:order/id 1} ?
               id (get params pk)
               ident [pk id]
               {::attr/keys [schema]} (key->attribute pk)
               connection (util/env->asami env schema aso/connections)]
    (do
      (log/info "Deleting" ident)
      (let [database-atom (get-in env [aso/databases schema])
            {:keys [db-after]} @(write/retract-entity connection ident)] ; we assume the entity has {:id <ident>}
        (some-> database-atom (reset! db-after))
        {})) ; I guess we return {} instead of nil to make Pathom understand it succeeded (=> no "not found")
    (log/warn "Asami adapter failed to delete " params)))

(defn save-form!
  "Do all the possible operations for the given form delta (save to the database involved)"
  [env {::form/keys [delta] :as _save-params}]
  (let [schemas (dups/schemas-for-delta env delta)]
    (when (next schemas) (log/debug "Saving form across " schemas))
    {:tempids
     (->>
       (for [schema schemas
             :let [connection (or (util/env->asami env schema aso/connections)
                                  (do (log/error "Unable to save form: connection is missing in env for schema "
                                                 schema "; has: " (keys (get env aso/connections)))
                                      nil))]
             :when connection]
         (try
           (log/debug "Saving form delta" (with-out-str (pprint delta)) "on schema" schema)
           (let [database-atom (get-in env [aso/databases schema])

                 ;; NOTE: tempid = Fulcro tempid;
                 ;;       generated-id = the (uu)id that we generated as the ID of the new entity
                 vtempid->generated-id (volatile! {})

                 ;; here tempids are [:id <ident>] -> node-id but we want tempid->ident
                 {:keys [db-after #_tempids]}
                 (write/transact-generated
                   connection
                   (fn [graph]
                     (let [{:keys [tempid->generated-id txn]}
                           (write/delta->txn-map-with-retractions env graph schema delta)]
                       (if (seq txn)
                         (log/debug "Running txn\n" (with-out-str (pprint txn)))
                         (log/error "Unable to save form: the transaction is empty for delta =" delta))
                       (vreset! vtempid->generated-id tempid->generated-id)
                       txn)))]
             (some-> database-atom (reset! db-after))
             @vtempid->generated-id)
           (catch #?(:clj Exception :cljs :default) e
             (log/error e "Transaction failed!")
             nil)))
       (apply merge))}))

(defn wrap-save
  "Form save middleware to accomplish saves."
  ([]
   (fn [{::form/keys [params] :as pathom-env}]
     (save-form! pathom-env params)))
  ([handler]
   (fn [{::form/keys [params] :as pathom-env}]
     (let [save-result    (save-form! pathom-env params)
           handler-result (handler pathom-env)]
       (util/deep-merge save-result handler-result)))))

(defn wrap-delete
  "Form delete middleware to accomplish deletes."
  ([]
   (fn [{::form/keys [params] :as pathom-env}]
     (delete-entity! pathom-env params)))
  ([handler]
   (fn [{::form/keys [params] :as pathom-env}]
     (let [delete-result   (delete-entity! pathom-env params)
           handler-result (handler pathom-env)]
       (util/deep-merge handler-result delete-result)))))

(defn wrap-env
  "Build a (fn [env] env') that adds RAD datomic support to an env.
  - `database-mapper` - a `(fn [env]) -> map from a schema to an Asami connection"
  [base-wrapper database-mapper]
  (fn [env]
    (cond-> (let [database-connection-map (database-mapper env)
                  databases               (update-vals database-connection-map (comp atom d/db #(util/ensure! % "nil value in db-connection-map")))]
              (assoc env
                aso/connections database-connection-map
                aso/databases databases))
            base-wrapper (base-wrapper))))

(defn id-resolver*
  "Common implementation of id-resolver.
  Takes in resolver-maker-fn, which knows how to make a Pathom resolver for either Pathom2 or Pathom3.
  Its signature must be [resolve-sym qualified-key outputs resolver-fn transform]"
  [resolver-maker-fn
   _attributes
   {::attr/keys [qualified-key schema] ::asami/keys [_fetch-nested? wrap-resolve] :as id-attribute}
   output-attributes]
  ;[::attr/attributes ::attr/attribute ::attr/attributes => ::pc/resolver]
  (let [transform nil ; not implemented (yet?)
        outputs   (attr/attributes->eql output-attributes)
        resolve-sym      (symbol
                           (str (namespace qualified-key))
                           (str (name qualified-key) "-resolver"))
        resolver-fn (cond-> (fn [{_ ::attr/key->attribute :as env} input]
                              (let [batch? (sequential? input)
                                    db     (util/env->asami env schema aso/databases)]
                                (log/debug "In resolver:" qualified-key "inputs:" (cond-> input (instance? #?(:clj clojure.lang.LazySeq :cljs cljs.core/LazySeq) input) vec) "db ver:" (d/as-of-t db))
                                (cond->> (->> (query/entities
                                                (assoc env ::asami/id-attribute id-attribute)
                                                input db)
                                              (util/map-over-many-or-one batch? (partial auth/redact env)))
                                         ;; Make sure len of inputs = len outputs despite not found entities,
                                         ;; just as Pathom3 requires
                                         batch? (misc.coll/restore-order input qualified-key))))
                            wrap-resolve (wrap-resolve))]
    (log/info "Building ID resolver for" qualified-key "outputs" outputs)
    (resolver-maker-fn resolve-sym qualified-key outputs resolver-fn transform)))
