(ns cz.holyjak.rad.database-adapters.asami.write
  (:require
    [asami.core :as d]
    [clojure.set :as set]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [cz.holyjak.rad.database-adapters.asami.duplicates :as dups]
    [cz.holyjak.rad.database-adapters.asami.util :refer [id? ref? to-one?]]))

(defn retract-entity [conn id]
  (d/transact
    conn
    (d/q '[:find :db/retract ?e ?a ?v :where [?e ?a ?v] [?e :id ?id] :in $ ?id]
         (d/db conn) id)))

(defn- uuid-ident?                                          ; copied from datomic-common
  "Returns true if the ID in the given ident uses UUIDs for ids."
  [{::attr/keys [key->attribute] :as env} ident]
  (= :uuid (some-> ident first key->attribute ::attr/type)))

(defn failsafe-id                                           ; copied & modified from datomic-common
  "Returns a fail-safe id for the given ident in a transaction. A fail-safe ID will be one of the following:
  - A string that stands for a temporary :db/id within the transaction if the id of the ident is temporary.
  - A lookup ref (the ident itself) if the ID uses a non-native ID, and it is not a tempid.
  "
  [{::attr/keys [key->attribute] :as env} ident]
  (let [[_ id] ident]
    (cond
      (tempid/tempid? id) (str (:id id))
      ;(and (native-ident? env ident) (pos-int? id)) id
      :otherwise ident)))

(defn non-id-schema-value?                                         ; copied from datomic-common (+ add docs)
  "The attribute belongs to the current schema (= DB) and is a value, i.e. not the ID"
  [{::attr/keys [key->attribute]} target-schema k]
  (let [{:keys [::attr/schema]
         ::attr/keys [identity?]} (key->attribute k)]
    (and (= schema target-schema) (not identity?))))

(defn generate-id                                           ; copied from cz.holyjak.rad.database-adapters.key-value.pathom/unwrap-id
  "Generate an id for a new entity being  saved. You need to pass a `suggested-id` as a UUID or a tempid.
  If it is a tempid and the ID column is a UUID, then the UUID *from* the tempid will be used."
  [{::attr/keys [key->attribute] :as env} k suggested-id]
  (let [{::attr/keys [type]} (key->attribute k)]
    (cond
      (= :uuid type) (cond
                       (tempid/tempid? suggested-id) (:id suggested-id)
                       (uuid? suggested-id) suggested-id    ; does this ever happen?
                       :else (throw (ex-info "Only unwrapping of tempid/uuid is supported" {:id suggested-id})))
      :otherwise (throw (ex-info "Cannot generate an ID for non-uuid ID attribute" {:attribute k})))))

;; TODO: How to determine an ID for a new entity? Options:
;; - If tempid provided and type is uuid -> use that
;; - X If type is long then perhaps use the Asami-generated node id? But I only get it after creating the entity? Also, in-mem it would be a kwd!
;; - X Otherwise support & require that the user suggests a value (string, ...)?
;; => store as both :id and :<entity>/id, i.e. the identity? attribute
(defn create-tempid->generated-id [env delta]
  (dups/tempids->generated-ids generate-id env delta))

(defn asami-lookup-ref->ident
  "The opposite of [[ident->asami-lookup-ref]]"
  [lookup-ref]
  {:pre [(vector? lookup-ref) (= :id (first lookup-ref))]}
  (second lookup-ref))

(defn ident->asami-lookup-ref
  "Turn a Fulcro ident into something Asami interprets as an entity id / lookup reference
  NOTE: If this is a new entity then the metadata `::new?` is added to it.
  See [[asami-lookup-ref->ident]] for the opposite."
  [tempid->real-id [key id :as ident]]
  (if-let [id' (when tempid->real-id (tempid->real-id id))]
    (with-meta [:id [key id']] {::new? true})
    [:id ident]))

(defn new-entity-ident->tx-data [[id-prop id-val :as ident]]
  ;; NOTE: We assume tempids have been resolved and replaced with real ones by now
  (let [eid (ident->asami-lookup-ref nil ident)]
    [[:db/add eid :id ident] ; add the internal Asami :id prop
     [:db/add eid id-prop id-val]   ; add the external id itself
     [:db/add eid :a/entity true]])) ; mark it as standalone entity => only ref-ed, not inlined when fetching a parent one

(defn singular-prop->tx-data
  "Create tx quadruplets: `[:db/add|remove <entity id/lookup> <property> <value>]`, one the value `v`, with
  encoding references in the way Asami expects and marking replacements of an existing value as such."
  [{:keys [tempid->real-id] :as env+} operation eid k v]
  (let [ref?? (ref? env+ k)]
    [[operation
      eid
      k
      (cond->> v
               ref?? (ident->asami-lookup-ref tempid->real-id))]]))

(defn multivalued-prop->tx-data
  "Create tx quadruplets: `[:db/add|remove <entity id/lookup> <property> <value>]`, one for each of `vals`, with
  encoding references in the way Asami expects."
  [{:keys [tempid->real-id] :as env+} operation eid k vals]
  (let [ref (ref? env+ k)]
    (mapv (fn [v] [operation
                   eid
                   k
                   (cond->> v
                            ref (ident->asami-lookup-ref tempid->real-id))])
          vals)))

(defn prop-delta->tx-data
  "Turns a single delta for a single entity and property into transaction(s) (multiple if cardinality = many)"
  [env+ eid k {:keys [before after] :as delta}]
  ;; NOTE: `delta` is typically map {:before <val>, :after <val>} but can also be a single value, for
  ;; the ID attribute or an enum attribute
  ;(def *args [env+ eid k {:before before :after after}])
  (let [singular? (to-one? env+ k)]
    (cond
      (and singular? (nil? after))
      (singular-prop->tx-data env+ :db/retract eid k before)

      (and singular? (nil? before))
      (singular-prop->tx-data env+ :db/add eid k after)

      (and singular? before)
      (concat (singular-prop->tx-data env+ :db/retract eid k before)
              (singular-prop->tx-data env+ :db/add eid k after))

      :multi-valued
      (let [before (set before)
            after (set after)]
        (concat
          (multivalued-prop->tx-data env+ :db/add eid k (set/difference after before))
          (multivalued-prop->tx-data env+ :db/retract eid k (set/difference before after)))))))

(defn delta->value-txn
  "Delta for non-id attributes into Asami tx-data"
  [{:keys [tempid->real-id] :as env+} schema delta]         ; copied & modified from datomic-common
  (into []
        (mapcat
          (fn entity-delta->tx-data [[[_ id :as ident] entity-delta]]
            (let [eid (ident->asami-lookup-ref tempid->real-id ident)
                  relevant-entity-deltas (->> entity-delta
                                              (filterv #(non-id-schema-value? env+ schema (key %))))]
              (->> relevant-entity-deltas
                   (mapcat (fn prop->tx-data [[k delta]] (prop-delta->tx-data env+ eid k delta)))
                   doall))))
        delta))
; new-id? (and (id? env+ k) (-> eid meta ::new?))

(>defn delta->txn
  "Turn Fulcro form delta into an Asami update transaction. Example delta (only one entry):

  {[:account/id #uuid \"ffffffff-ffff-ffff-ffff-000000000100\"]
   {:account/active? {:before true, :after false}}}

   Returns a map with three keys:
    - `txn` - the Asami transactions themselves
    - `tempid->generated-id` - mapping from Fulcro's tempids in delta to real, external IDs generated for the new entities"
  [env schema delta]
  [map? keyword? map? => map?]
  ;; 1. Map tempids (which contain a uuid) to Asami tempids (neg ints), replace wherever used in the tx
  (let [tempid->generated-id (create-tempid->generated-id env delta)
        ;;; For new entities being created, add the identifier attributes - the :<entity>/id <val> and :id <ident> ones:
        new-ids-txn (into []
                          (comp
                            ;; There are likely better ways to do this, e.g. checking (and (tempid/tempid? id) (uuid-ident? env ident))
                            (map (partial ident->asami-lookup-ref tempid->generated-id))
                            (filter #(-> % meta ::new?))
                            (mapcat (comp new-entity-ident->tx-data asami-lookup-ref->ident)))
                          (keys delta))]
    {:tempid->generated-id tempid->generated-id
     :txn (into []
                (concat
                  ;non-native-id-attributes-txn
                  new-ids-txn
                  (delta->value-txn
                    (assoc env :tempid->real-id tempid->generated-id)
                    schema delta)))}))
