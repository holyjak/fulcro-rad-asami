(ns cz.holyjak.rad.database-adapters.asami.write
  "Support for turning form deltas into Asami transactions etc."
  (:require
    [asami.core :as d]
    [asami.graph :as graph]
    [clojure.set :as set]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [cz.holyjak.rad.database-adapters.asami.duplicates :as dups]
    [cz.holyjak.rad.database-adapters.asami.util :refer [ensure! ref? to-one?]]
    [taoensso.timbre :as log]
    [cz.holyjak.rad.database-adapters.asami.util :as util]))

(defn non-id-schema-prop?                                         ; copied from datomic-common + add docs, renamed
  "The attribute belongs to the current schema (= DB) and is a normal proerty, i.e. not the ID"
  [{::attr/keys [key->attribute]} target-schema k]
  (let [{:keys [::attr/schema]
         ::attr/keys [identity?]} (key->attribute k)]
    (and (= schema target-schema) (not identity?))))

(defn- clear-entity-singular-attributes-txn [graph [ident singular-props]]
  (if-let [node-id (ffirst (graph/resolve-triple graph '?n :id ident))]
    (for [prop singular-props
          :let [existing-val (ffirst (ensure!
                                       (graph/resolve-triple graph node-id prop '?xval)
                                       #(-> % next count zero?)
                                       (str "More than one existing value on " ident " " prop)))]
          :when (some? existing-val)]
      [:db/retract node-id prop existing-val])
    (do (log/warn "Expected to find an entity with the ident" ident "to clear its singular props but no match")
        nil))

  )

(defn clear-singular-attributes-txn
  "Generate retractions for existing values of the given `singular-props` attributes of the given `ident` entities
  NOTE: It only clears the single attribute. If it points to a dependant entity, it remains in existence.
  Hopefully RAD handles removing those."
  [db ident->singular-props]
  (let [graph (d/graph db)]
    (->> (mapcat (partial clear-entity-singular-attributes-txn graph) ident->singular-props)
         not-empty)))

(defn- entity-delta->singular-attrs [{::attr/keys [key->attribute] :as env} schema entity-delta]
  (->> entity-delta
       (filter (fn [[k v]] (and (util/to-one? env k)
                                (non-id-schema-prop? env schema k)
                                (map? v)
                                (contains? v :after))))
       (map key)
       set
       not-empty))

(defn- assoc!-some-1 [m k v]
  (cond-> m
          v (assoc! k v)))

(defn delta->singular-attrs-to-clear
  "Derives from the form delta and attribute definitions which singular attributes should be cleared of current value"
  [key->attribute schema delta]
  (let [env {::attr/key->attribute key->attribute}]
   (->> (reduce-kv (fn [m [_ id-val :as ident] entity-delta]
                     (cond-> m
                             (not (tempid/tempid? id-val))
                             (assoc!-some-1 ident (entity-delta->singular-attrs env schema entity-delta))))
                   (transient {})
                   delta)
        persistent!
        not-empty)))

(defn retract-entity
  "Retract a (flat) entity from the DB, given the value of its `:id` attribute. Returns same as `d/transact`"
  [conn id]
  (let [retract-txn (d/q '[:find :db/retract ?e ?a ?v :where [?e ?a ?v] [?e :id ?id] :in $ ?id]
                         (d/db conn) id)
        retract-id (first (filter #(= :id (nth % 2)) retract-txn))
        _ (log/debug "Retracting entity `" (pr-str id) "` with:" (vec retract-txn))
        res1 (d/transact conn retract-txn)
        ;; TODO TMP Fix of Asami failing to retract :id if its value = another property Entry - https://github.com/quoll/asami/issues/5
        res2 (when retract-id (d/transact conn [retract-id]))]
    (or res2 res1)))

;(defn- uuid-ident?                                          ; copied from datomic-common
;  "Returns true if the ID in the given ident uses UUIDs for ids."
;  [{::attr/keys [key->attribute] :as env} ident]
;  (= :uuid (some-> ident first key->attribute ::attr/type)))

(defn generate-id                                           ; based on ...database-adapters.key-value.pathom/unwrap-id
  "Generate an id for a new entity being  saved. You need to pass a `suggested-id` as a UUID or a tempid.
  If it is a tempid and the ID column is a UUID, then the UUID *from* the tempid will be used."
  [{::attr/keys [key->attribute] :as env} k suggested-id]
  (let [{::attr/keys [type]} (key->attribute k)]
    (cond
      (= :uuid type) (cond
                       (tempid/tempid? suggested-id) (:id suggested-id)
                       (uuid? suggested-id) suggested-id    ; does this ever happen?
                       :else (throw (ex-info "Only unwrapping of tempid/uuid is supported" {:id suggested-id})))
      :otherwise (throw (ex-info "Don't know how to generate an ID for non-uuid ID attribute" {:attribute k})))))

(defn create-tempid->generated-id [env delta]
  (dups/tempids->generated-ids generate-id env delta))

(defn asami-lookup-ref->ident
  "The opposite of [[ident->asami-lookup-ref]], i.e. `[:id <ident>] -> <ident>`"
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

(defn new-entity-ident->tx-data
  "Produce the tx-data for `d/transact` necessary to add a new entity - typically adding the ident's key and value as
  a property plus other required entity properties."
  [[id-prop id-val :as ident]]
  ;; NOTE: We assume tempids have been resolved and replaced with real ones by now
  (let [eid (ident->asami-lookup-ref nil ident)]
    [[:db/add eid :id ident] ; add the internal Asami :id prop
     [:db/add eid id-prop id-val]   ; add the external id itself
     [:db/add eid :a/entity true]])) ; mark it as standalone entity => only ref-ed, not inlined when fetching a parent one

(defn prop->tx-data
  "Add/remove the property's value(s) into/from Asami.
  For singular attributes, the value needs to be wrapped in a set.
  References are encoded in the way Asami expects."
  [{:keys [tempid->real-id] :as env+} operation eid k vals]
  (let [ref?? (ref? env+ k)]
    (mapv (fn [v] [operation
                   eid
                   k
                   (cond->> v
                            ref?? (ident->asami-lookup-ref tempid->real-id))])
          vals)))

(defn prop-delta->tx-data
  "Turns a single delta for a single entity and property into transaction(s) (multiple if cardinality = many)"
  [env+ eid k {:keys [before after] :as delta}] cat
  ;; NOTE: `delta` is typically map {:before <val>, :after <val>} expect for the ID attribute

  (let [singular? (to-one? env+ k)
        ;; We turn singular values into a set so we can handle them in the same way as to-many; Asami does insert each
        ;; set value separately, ie. never the whole set as-is.
        before (if singular? (some-> before hash-set) (set before))
        after (if singular? (some-> after hash-set) (set after))]
    (concat
      ;; Note: Singular attr values are retracted separately
      (when-not singular? (prop->tx-data env+ :db/retract eid k (set/difference before after)))
      (prop->tx-data env+ :db/add eid k (set/difference after before)))))

(defn delta->value-txn
  "Turn Fulcro delta for non-id attributes into Asami tx-data"
  [{:keys [tempid->real-id] :as env+} schema delta]
  (eduction
    (mapcat (fn entity-delta->tx-data [[[_ id :as ident] entity-delta]]
              (let [eid (ident->asami-lookup-ref tempid->real-id ident)]
                (eduction
                  (filter #(non-id-schema-prop? env+ schema (key %)))
                  (mapcat (partial apply prop-delta->tx-data env+ eid))
                  entity-delta))))
    delta))

(>defn delta->txn
  "Turn Fulcro form delta into an Asami update transaction. Example delta (only one entry):

  {[:account/id #uuid \"ffffffff-ffff-ffff-ffff-000000000100\"]
   {:account/active? {:before true, :after false}}}

   Returns a map with three keys:
    - `txn` - the Asami transactions themselves
    - `tempid->generated-id` - mapping from Fulcro's tempids in delta to real, external IDs generated for the new entities

   BEWARE: It does not retract existing values of singular attributes being updated/removed. It should not be used directly,
   use [[delta->txn-with-retractions]] instead."
  [env schema delta]
  [map? keyword? map? => map?] ; TODO Specify what env keys we actually use
  (let [tempid->generated-id (create-tempid->generated-id env delta)
        ;;; For new entities being created, add the identifier attributes - the :<entity>/id <val> and :id <ident> ones:
        new-ids-txn (eduction
                      ;; There are likely better ways to do this, e.g. checking (and (tempid/tempid? id) (uuid-ident? env ident))
                      (map (partial ident->asami-lookup-ref tempid->generated-id))
                      (filter (comp ::new? meta))
                      (mapcat (comp new-entity-ident->tx-data asami-lookup-ref->ident))

                      (keys delta))]
    {:tempid->generated-id tempid->generated-id
     :txn (into []
                (concat
                  ;non-native-id-attributes-txn
                  new-ids-txn
                  (delta->value-txn
                    (assoc env :tempid->real-id tempid->generated-id)
                    schema delta)))}))

(defn delta->txn-with-retractions
  "Like [[delta->txn]] but includes retractions for all involved singular attributes."
  [{::attr/keys [key->attribute] :as env} db schema delta]
  (let [retractions (->> (delta->singular-attrs-to-clear key->attribute schema delta)
                         (clear-singular-attributes-txn db))
        changes  (delta->txn env schema delta)]
    (update changes :txn (partial concat retractions))))