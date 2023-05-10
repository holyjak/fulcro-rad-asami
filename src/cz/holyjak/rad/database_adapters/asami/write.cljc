(ns cz.holyjak.rad.database-adapters.asami.write
  "Support for turning form deltas into Asami transactions etc."
  (:require
    [asami.core :as d]
    [asami.entities :as entities]
    [asami.graph :as graph]
    [asami.storage :as storage]
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [com.fulcrologic.guardrails.core :refer [>defn =>]]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [cz.holyjak.rad.database-adapters.asami :as-alias asami]
    [cz.holyjak.rad.database-adapters.asami.duplicates :as dups]
    [cz.holyjak.rad.database-adapters.asami.util :as util :refer [ensure! ref? to-one?]]
    [taoensso.timbre :as log]))

(s/def ::attr/key->attribute map?)
(s/def ::env (s/keys :req [::attr/key->attribute]))

(defn transact-generated
  "EXPERIMENTAL. Like asami.core/transact but first generates the _tx-data_ (sequence of triplets)
   via `(tx-generator-fn graph)`. The generator is run when it has exclusive access to the storage,
   i.e. no other transaction will change the DB in the meantime.

   Based on simplified and extended code of `asami.core/transact` does.

   BEWARE: The DB is locked while `tx-generator-fn` runs so make it fast."
  [connection tx-generator-fn]
  ;; Detached databases need to be reattached when transacted into
  (d/check-attachment connection)

  (let [vtempids       (volatile! {}) ;; volatile to capture the tempid map from built-triples
        generated-data (volatile! [[] []]) ;; volatile to capture the asserted and retracted data in a transaction
        [db-before db-after]
        (storage/transact-update
          connection
          (fn [graph tx-id]
            (let [tx-data-seq (util/ensure! (tx-generator-fn graph) (some-fn nil? sequential?)
                                            "tx-generator-fn must produce a sequence of data (maps/triples)")
                  [asserts retracts tempids] (entities/build-triples graph tx-data-seq)]
              (vreset! vtempids tempids)
              (graph/graph-transact graph tx-id asserts retracts generated-data))))

        ;; pull out the info captured during the transaction
        [triples retracts] (deref generated-data)]
    {:db-before db-before
     :db-after  db-after
     :tx-data   (concat retracts triples)
     :tempids   @vtempids}))

(defn non-id-schema-attr?                                         ; copied from datomic-common + add docs, renamed
  "The attribute belongs to the current schema (= DB) and is a normal property, i.e. not the ID"
  [target-schema attr]
  (let [{::attr/keys [identity? schema]} attr]
    (and (= schema target-schema) (not identity?))))

(defn non-id-schema-prop?                                         ; copied from datomic-common + add docs, renamed
  "The attribute belongs to the current schema (= DB) and is a normal property, i.e. not the ID"
  [{::attr/keys [key->attribute]} target-schema k]
  (if-let [attr (key->attribute k)]
    (non-id-schema-attr? target-schema attr)
    (do
      (log/warn "Encountered unknown attribute" k
                "Is it properly registered with Pathom?"
                "Known nr. attrs = " (count key->attribute))
      nil)))

(defn- ident->node-id
  "Leverage `:id` to find the node-id of the given ident's entity"
  [graph ident]
  (try (ffirst (graph/resolve-triple graph '?n :id ident))
       (catch #?(:clj ClassCastException :cljs :default) e
         (throw (ex-info (str "Finding Asami node with :id " (pr-str ident)
                              " failed. Possibly there is a type mismatch between"
                              " its value and the corresponding DB value. (Are you"
                              " passing a string where DB has an uuid <=> forget to"
                              " prefix it with `#uuid` ?) Error: " (ex-message e))
                         {:ident ident})))))

(defn- clear-entity-singular-attributes-txn [graph [ident singular-props]]
  (if-let [node-id (ident->node-id graph ident)]
    (for [prop singular-props
          :let [existing-val (ffirst (ensure!
                                       (graph/resolve-triple graph node-id prop '?xval)
                                       (comp empty? next)
                                       (str "More than one existing value on " ident " " prop)))]
          :when (some? existing-val)]
      [:db/retract node-id prop existing-val])
    (do (log/warn "Expected to find an entity with the ident" ident "to clear its singular props but no match")
        nil)))

(defn- as-graph [graph-or-db]
  (if (satisfies? graph/Graph graph-or-db)
    graph-or-db
    (d/graph graph-or-db)))

(defn clear-singular-attributes-txn
  "Generate retractions for existing values of the given `singular-props` attributes of the given `ident` entities
  NOTE: It only clears the single attribute. If it points to a dependant entity, it remains in existence.
  Hopefully RAD handles removing those."
  ;; Perhaps add an attribute flag telling us what entities cannot exist on their own and only have 1 parent and thus
  ;; should be deleted?
  [graph-or-db ident->singular-props]
  (->> (mapcat (partial clear-entity-singular-attributes-txn (as-graph graph-or-db)) ident->singular-props)
       not-empty))

(defn keep-attr-deltas
  "Like core/keep on the seq of entity delta entries, but with the signature `(fn [rad-attribute {:keys [before after]}])`.
  Notice the rad-attribute could be `nil`, e.g. if not properly registered with Pathom.
  Here, `entity-delta` is typically the map `{<qualified attr key> {:before .., :after ...}, ...}`."
  [attr-filter-fn key->attribute entity-delta]
  (keep (fn [[attr-key attr-delta]] (attr-filter-fn (get key->attribute attr-key) attr-delta)) entity-delta))

(defn- entity-delta->singular-attrs [{::attr/keys [key->attribute] :as _env} schema entity-delta]
  (->> entity-delta
       (keep-attr-deltas (fn [attr attr-delta]
                           (when (and (not (attr/to-many? attr))
                                      (non-id-schema-attr? schema attr)
                                      (map? attr-delta) ; being extra cautious here...
                                      (contains? attr-delta :after)) ; notice the value could be nil => need contains?
                             (::attr/qualified-key attr)))
                            key->attribute)
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

(defn retract-entity-txn
  "Return the transaction data to retract a (flat) entity from the DB, given its ident.

  BEWARE: Until https://github.com/quoll/asami/issues/5 is fixed, you might need to retract ID
          attributes separately in a subsequent transaction."
  [db ident]
  (d/q '[:find :db/retract ?e ?a ?v :where [?e ?a ?v] [?e :id ?id] :in $ ?id]
       db ident))

(defn retract-entity
  "Retract a (flat) entity from the DB, given the value of its `:id` attribute. Returns same as `d/transact`"
  [conn id]
  (let [retract-txn (retract-entity-txn (d/db conn) id)
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
  [{::attr/keys [key->attribute] :as _env} k suggested-id]
  (let [{::attr/keys [type] :as attr} (key->attribute k)]
    (when-not attr
      (throw (ex-info (str "Couldn't find the attribute "
                           k
                           ", is it properly registered with Pathom?")
                      {:attribute-name k,
                       :known-attrs (keys key->attribute)})))
    (cond
      (= :uuid type) (cond
                       (tempid/tempid? suggested-id) (:id suggested-id)
                       (uuid? suggested-id) suggested-id    ; does this ever happen?
                       :else (throw (ex-info "Only unwrapping of tempid/uuid is supported" {:id suggested-id})))
      :else (throw (ex-info "Don't know how to generate an ID for non-uuid ID attribute"
                            {:attribute-name k, :type type, :attr attr})))))

(defn ^:no-doc create-tempid->generated-id [env delta]
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
  ([ident] (new-entity-ident->tx-data nil ident))
  ([key->attribute [id-prop id-val :as ident]]
   ;; NOTE: We assume tempids have been resolved and replaced with real ones by now
   (let [entity? (not (::asami/owned-entity? (get key->attribute id-prop)))
         eid (ident->asami-lookup-ref nil ident)]
     (cond-> [[:db/add eid :id ident] ; add the internal Asami :id prop
              [:db/add eid id-prop id-val]]  ; add the external id itself
             entity? (conj [:db/add eid :a/entity true]))))) ; mark it as standalone entity => only ref-ed, not inlined when fetching a parent one

(defn- ref-to-owned-entity?
  "Is the given attribute a :ref and the target entity is dependent on it, i.e. marked with `::asami/owned-entity?`?"
  [key->attribute attr-key]
  (let [{::attr/keys [target type]} (get key->attribute attr-key)]
    (and (= type :ref)
         (::asami/owned-entity? (get key->attribute target)))))

(defn ^:no-doc prop->tx-data
  "Add/remove the property's value(s) into/from Asami.
  For singular attributes, the value needs to be wrapped in a set.
  References are encoded in the way Asami expects."
  [{:keys [::attr/key->attribute tempid->real-id] :as env+} operation eid k vals]
  (->> vals
       (mapcat (fn [v]
                 (let [real-id (when (ref? env+ k)
                                 (ident->asami-lookup-ref tempid->real-id v))]
                   (cond-> [[operation eid k (or real-id v)]]
                           (and (ref-to-owned-entity? key->attribute k) (= operation :db/add)) ; TODO Do we need to retract manually too
                           (conj [:db/add eid :a/owns real-id])))))
       (into [])))

(defn attr-delta->before+after-sets
  "Returns the before and after of the delta as sets (or nil, if missing), whether singular or not.
  Thus downstream code doesn't need to care about arity."
  [env attr-key {:keys [before after] :as _attr-delta}]
  (let [singular? (to-one? env attr-key)
        ;; We turn singular values into a set so we can handle them in the same way as to-many; Asami does insert each
        ;; set value separately, ie. never the whole set as-is.
        before (if singular? (some-> before hash-set) (set before))
        after (if singular? (some-> after hash-set) (set after))]
    [before after]))

(defn prop-delta->tx-data
  "Turns a single delta for a single entity and property into transaction(s) (multiple if cardinality = many)"
  [{::attr/keys [#_key->attribute] :as env+} eid k attr-delta]
  ;; NOTE: `delta` is typically map {:before <val>, :after <val>} expect for the ID attribute
  (let [singular? (to-one? env+ k)
        [before after] (attr-delta->before+after-sets env+ k attr-delta)]
    (concat
      ;; Note: Singular attr values are retracted separately, see clear-singular-attributes-txn
      (when-not singular? (prop->tx-data env+ :db/retract eid k (set/difference before after)))
      (prop->tx-data env+ :db/add eid k (set/difference after before)))))

(defn delta->value-txn
  "Turn Fulcro delta for non-id attributes into Asami tx-data"
  [{:keys [tempid->real-id] :as env+} schema delta]
  (eduction
    (mapcat (fn entity-delta->tx-data [[ident entity-delta]]
              (let [eid (ident->asami-lookup-ref tempid->real-id ident)]
                (eduction
                  (filter #(non-id-schema-prop? env+ schema (key %)))
                  (mapcat (partial apply prop-delta->tx-data env+ eid))
                  entity-delta))))
    delta))

(defn ^:no-doc  delta->txn*
  [{::attr/keys [key->attribute] :as env} schema delta]
  (let [tempid->generated-id (create-tempid->generated-id env delta)
        ;;; For new entities being created, add the identifier attributes - the :<entity>/id <val> and :id <ident> ones:
        new-ids-txn (eduction
                      ;; There are likely better ways to do this, e.g. checking (and (tempid/tempid? id) (uuid-ident? env ident))
                      (map (partial ident->asami-lookup-ref tempid->generated-id))
                      (filter (comp ::new? meta))
                      (mapcat (comp (partial new-entity-ident->tx-data key->attribute) asami-lookup-ref->ident))

                      (keys delta))]
    {:tempid->generated-id tempid->generated-id
     :txn (into []
                (concat
                  ;non-native-id-attributes-txn
                  new-ids-txn
                  (delta->value-txn
                    (assoc env :tempid->real-id tempid->generated-id)
                    schema delta)))}))

(defn orphaned-owned-entities-retraction-txn
  "Make a retraction txn including all 'owned' entities that their parents stopped referring to.
  (Essentially, implementing cascading delete.)"
  [graph-or-db key->attribute delta]
  (let [env {::attr/key->attribute key->attribute}]
    (letfn [(owned-idents-to-delete [[attr-key attr-delta]]
              (when (ref-to-owned-entity? key->attribute attr-key)
                (->> (attr-delta->before+after-sets env attr-key attr-delta)
                     (apply set/difference)
                     not-empty)))
            (ident->retractions [graph ident]
              (let [node-id (ident->node-id graph ident)]
                (->> (graph/resolve-triple graph node-id '?prop '?val)
                     (mapv (fn [prop+val] (into [:db/retract node-id] prop+val))))))
            (delta->idents-to-remove [delta]
              (transduce
                (comp
                  (keep (fn [[[_ id-val] entity-delta]]
                          (when-not (tempid/tempid? id-val)
                            (eduction
                              (keep owned-idents-to-delete)
                              entity-delta))))
                  cat)
                into #{}
                delta))]
      (->> (delta->idents-to-remove delta)
           (into [] (comp (map (partial ident->retractions (as-graph graph-or-db))) cat))
           not-empty))))

(>defn delta->txn-map-with-retractions
  "Turn Fulcro form delta into an Asami update transaction. Example delta (only one entry):

  {[:account/id #uuid \"ffffffff-ffff-ffff-ffff-000000000100\"]
   {:account/active? {:before true, :after false}}}

   Returns a map with two keys:
    - `txn` - the Asami transactions themselves
    - `tempid->generated-id` - mapping from Fulcro's tempids in delta to real, external IDs generated for the new entities

  The transactions will include retractions for all involved singular attributes; the delta's
  `:before` value of such singular attributes is ignored."
  [{::attr/keys [key->attribute] :as env} graph-or-db schema delta]
  [::env  any? keyword? map? => map?]
  (let [retractions (->> (delta->singular-attrs-to-clear key->attribute schema delta)
                         (clear-singular-attributes-txn graph-or-db)
                         (concat (orphaned-owned-entities-retraction-txn graph-or-db key->attribute delta))
                         (remove nil?)
                         not-empty)
        changes  (delta->txn* env schema delta)]
    (update changes :txn (partial concat retractions))))