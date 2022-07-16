(ns cz.holyjak.rad.database-adapters.asami.write
  (:require
    [asami.core :as d]
    [clojure.set :as set]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [cz.holyjak.rad.database-adapters.asami.duplicates :as dups]
    [taoensso.timbre :as log]))

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

(defn to-one? [{::attr/keys [key->attribute]} k]            ; copied from datomic-common
  (when key->attribute (not (boolean (some-> (get key->attribute k) (attr/to-many?))))))

(defn ref? [{::attr/keys [key->attribute]} k]               ; copied from datomic-common
  (when key->attribute (= :ref (some-> k key->attribute ::attr/type))))

(defn schema-value?                                         ; copied from datomic-common (+ add docs)
  "The attribute belongs to the current schema (= DB) and is a value, i.e. not the ID"
  [{::attr/keys [key->attribute]} target-schema k]
  (let [{:keys [::attr/schema]
         ::attr/keys [identity?]} (key->attribute k)]
    (and (= schema target-schema) (not identity?))))

(defn- tempids-in-delta                                     ; copied from cz.holyjak.rad.database-adapters.key-value.pathom
  "delta is key-ed by ident, so easy to find all the ids that are tempid-s"
  [delta]
  (into #{} (keep (fn [[table id :as ident]]
                    (when (tempid/tempid? id)
                      id))
                  (keys delta))))

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

(defn ident->asami-id
  "Turn a Fulcro ident into something Asami interprets as an entity id"
  [tempid->real-id [key id :as ident]]
  (if-let [id' (tempid->real-id id)]
    (with-meta [:id [key id']] {::new? true})
    [:id ident]))

(defn asamify-ref
  "Turn a Fulcro ident to a correct Asami reference to another entity. We can either use its :id as here:
  `[#a/n [16] :order/customer-ref [:id [:customer/id 2]]]`"
  [tempid->real-id [key id :as ident]]
  (if-let [id' (get tempid->real-id id)]
    ^::new? [:id [key id']]
    [:id ident]))

(defn- force-value-replacement
  "Asami attributes are multi-valued by default and `add` just adds a new value.
  To tell it we want to replace the current value we need to append ' to the attribute keyword."
  [k]
  (keyword (namespace k) (str (name k) \')))

(defn ->entity-transactions
  "Create tx quadruplets: `[:db/add|remove <entity id/lookup> <property> <value>]`, one for each of `vals`, with
  encoding references in the way Asami expects and marking replacements of an existing value as such."
  [{:keys [tempid->real-id] :as env+} operation eid k vals]
  (let [ref (ref? env+ k)
        singular? (to-one? env+ k)
        new-entity? (-> eid meta ::new?)]
    (map (fn [v] [operation
                  eid
                  (cond-> k
                          (and (not new-entity?)
                               singular?
                               (= operation :db/add))
                          (force-value-replacement))
                  (cond->> v
                           ref (asamify-ref tempid->real-id))])
         vals)))

(defn delta->entity-transactions
  "Turns a single delta for a single entity and property into transaction(s) (multiple if cardinality = many)"
  [env+ eid k {:keys [before after] :as delta}]
  ;; NOTE: `delta` is typically map {:before <val>, :after <val>} but can also be a single value, for
  ;; the ID attribute or an enum attribute
  ;(def *args [env+ eid k {:before before :after after}])
  (let [singular? (to-one? env+ k)]
    (cond
      (and singular? (nil? after))
      (->entity-transactions env+ :db/retract eid k [before])

      (and singular?)
      (->entity-transactions env+ :db/add eid k [after])

      :else
      (let [before (set before)
            after (set after)]
        (concat
          (->entity-transactions env+ :db/add eid k (set/difference after before))
          (->entity-transactions env+ :db/retract eid k (set/difference before after)))))))

(defn new-entity-tx
  "Create an ({} form) transaction to insert a new entity into Asami.
  Ensure proper storage of the ident."
  [env [id-prop id-value :as ident] own-props]
  (into
    {id-prop id-value                ; the id attribute itself, e.g. `:person/id <val>`
     :id ident}                      ; ident as Asami's :id for lookups
    (map (fn asamify-val [[prop value]]
           (let [to-many?? (not (to-one? env prop)),
                 ;; NOTE: Currently ref?? always false as we offload all refs to `tmp-refs` for *now* [do we?!]
                 ref?? (ref? env prop)]
             [prop (cond->> value
                            (and to-many?? ref??)
                            (map #(asamify-ref nil %))

                            (and (not to-many??) ref??)
                            (asamify-ref nil)

                            ;; many-valued values should be *sets* for Asami; could cause issues though..
                            ;; FIXME Support asami-options/no-set? to prevent attribute set-ification
                            to-many?? (set))])))
    own-props))

;(defn new-entity-txn->entity-map
;  ""
;  [new? entity-txn])
(defn new-entity-delta->txs
  "Create transactions for a *new* entity.
    - `env` the environment
    - `eid` Asami lookup id such as `:id <ident>`
    - `entity-delta` Fulcro form delta for the for the entity, ie. {<prop> {:after <new value>}, ..}"
  ;; Asami 2.3.0 does not allow us to create an entity and refer to it using its ID when
  ;; using the `:id ..` custom ID - so we a) cannot use the triplet form for its props and
  ;; b) we cannot add references to it to other entities => 1 tx to create
  ;; the entity (using the map form), 2nd to add references to the new entity (or to other new entities),
  ;; which must be transacted later.
  [{::attr/keys [key->attribute] :as env} [_ [id-prop id-value :as ident] :as eid] entity-delta]
  ;when (and (tempid/tempid? id) (uuid-ident? env ident))
  (let [prop->val (update-vals entity-delta :after) ; only retain the new values
        as-col (fn as-col [[prop value]]
                 ;; Ensure that the `value` is always a vector, even for to-one props
                 (cond-> value (to-one? env prop) (vector)))

        ;; Group props into simple value props and "foreign key" to new entity props + split multi-valued props
        ;; (as some vals of a to-many ref could point to new entities while others to existing ones)
        {tmp-refs :ref2tempid, own-props :other}
        (->
          (->> prop->val
               ;; Split multi-valued props into a seq of [k v1] [k v2] ... [k vn]
               (mapcat (fn [[k vs :as entry]] (if (to-one? env k) [entry] (map (partial vector k) vs))))
               (group-by (fn new-entity-ref? [[prop maybe-ident]]
                           (if (and (ref? env prop) (tempid/tempid? (second maybe-ident))) :ref2tempid :other))))
          #_(update-vals #(map second))) ; TODO: Why was this here? (also, it is broken, missing %)

        ;; We create the entity using the {} tx b/c it is less work for me than generating the quadruplets
        create-entity-tx (new-entity-tx env ident own-props)]
    (into [create-entity-tx]
          (mapcat #(->entity-transactions env :db/add eid (key %) (as-col %)))
          tmp-refs)))

(defn delta->value-txn
  "Create transactions for non-id attributes"
  [{:keys [tempid->real-id] :as env+} schema delta]         ; copied & modified from datomic-common
  (->> delta
       (mapcat
         (fn [[[_ id :as ident] entity-delta]]
           (let [eid (ident->asami-id tempid->real-id ident)
                 relevant-entity-deltas (->> entity-delta
                                             (filter #(schema-value? env+ schema (key %))))
                 new-entity? (tempid/tempid? id)]
             (if new-entity?
               (new-entity-delta->txs env+ eid relevant-entity-deltas)
               (->> relevant-entity-deltas
                    (mapcat (fn [[k delta]] (delta->entity-transactions env+ eid k delta))))))))
       vec))

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
        ;non-native-id-attributes-txn (keep (fn [[k id :as ident]]
        ;                                     (when (and (tempid/tempid? id) (uuid-ident? env ident))
        ;                                       {;; the id attribute itself, e.g. :person/id -
        ;                                        k (tempid->generated-id id)
        ;                                        ;; the ident such as [:person/id <value>] stored as Asami's :id for lookups:
        ;                                        :id [k (tempid->generated-id id)]}))
        ;                                   (keys delta))
        #_#_asami-id-attributes-txn (map (fn [[_ eid k id]] [:db/add eid :id [k id]]) non-native-id-attributes-txn)]
    {:tempid->generated-id tempid->generated-id
     :txn (into []
                (concat
                  ;non-native-id-attributes-txn
                  ;asami-id-attributes-txn
                  (delta->value-txn
                    (assoc env :tempid->real-id tempid->generated-id)
                    schema delta)))}))
