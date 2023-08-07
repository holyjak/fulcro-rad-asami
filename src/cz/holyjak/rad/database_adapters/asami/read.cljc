(ns cz.holyjak.rad.database-adapters.asami.read
  "Support for reading RAD entities from Asami"
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [com.fulcrologic.guardrails.core :refer [>def >defn =>]]
    [com.fulcrologic.rad.attributes :as attr]
    [cz.holyjak.rad.database-adapters.asami :as-alias asami]
    [cz.holyjak.rad.database-adapters.asami.util :as util :refer [to-many? ref?]]
    [edn-query-language.core :as eql]
    [taoensso.timbre :as log]
    ;[com.fulcrologic.rad.attributes-options :as ao]
    ;[clojure.walk :as walk]
    [asami.core :as d]))

;; ---------------------------------------------------------------------------------------------------------
(defn- asami-ref->pathom! ; x FEAT-NAT-IDS - for these, include the entity PK as meta on the entity? (-> e meta :asami/pk) = :<entity>/id ?
  "Translate Asami ref like `{:id val}` where val is for us always an ident into
  `{<ident prop> <ident val>}`, e.g. `{::address/id #uuid '123'}`"
  [k v]
  (cond
    ;; If we get [:id [::address/id #uuid "465d1920-0d3f-4dac-9027-d0bca986a6c8"]]
    (and (vector? v) (= 2 (count v)) (= :id (first v)) (eql/ident? (second v)))
    (->> v second (apply hash-map))

    ;; If we get {:id [::address/id #uuid "465d1920-0d3f-4dac-9027-d0bca986a6c8"]}
    (and (map? v) (:id v) #_(= 1 (count v)) (eql/ident? (:id v)))
    (merge (dissoc v :id)
           (->> v :id (apply hash-map)))

    ;; We get an empty map when the target entity of the ref has been deleted from the DB
    (= v {})
    nil

    :else (throw (ex-info "A ref attribute value does not contain `:id <ident>`"
                          {:key k, :value v}))))

(defn transform-entity-property
  "Adjust raw Asami data so that it is suitable for Pathom"
  [{_ ::attr/key->attribute :as env} k v]
  (let [pr-type            #(let [t (type %)] (if #?(:clj (class? t) :cljs false) (.getSimpleName t) (str t)))
        to-many??          (to-many? env k)
        ensure-ref-is-map! (fn [v-orig v']
                             (let [ref-val (cond-> v' to-many?? first)
                                   processed? (not= v' v-orig)]
                               (assert (map? ref-val) ; could fail due to badly inserted data
                                       (str "c.h.r.d.asami.read/transform-entity: The ref in the the to-"
                                            (if to-many?? "many" "one")
                                            " attr " k " should be a map (with :<?>/id) but it's type is "
                                            (pr-type ref-val) " The whole " (when processed? "(adjusted) ")
                                            "value of the attr is: "
                                            (pr-str v') (when processed?
                                                          (str " Its original value was " (pr-str v-orig)))))
                               v'))]
    ;; check
    (when (and to-many?? (set? v) (= 1 (count v)) (vector? (first v)) (next (first v)) (map? (ffirst v)))
      ;; We got st. like #{[{:x/id "1"}, ...]}, which indicates badly inserted data where the user wanted to
      ;; insert multiple entities/refs but ended up with a single value of type vector instead
      (throw (ex-info (str "Bad to-many attribute value inserted in the DB: Should have been multiple "
                           " entities but is a single vector value containing these entities. Attr.: "
                           k)
                      {:attribute-key k, :attribute-value v})))
    (cond-> v
            (nil? v)
            (->> (do (log/warn "nil value in database for attribute" k)))  ; Note: Not sure that this can ever happen

            ;; NOTE: Asami returns to-many props as a set if 2+ values or a single value if just one
            ;; => unify to always be a vector
            (and to-many??
                 (not (set? v))
                 (not (sequential? v)))
            ;; A single value (a map or a primitive) => wrap in a sequence
            (vector)

            ;; Turn the to-many set returned by Asami for 2+ elements into vector b/c Pathom does not handle sets
            to-many??
            vec

            ;; 1. Translate refs from Asami's {:id [<id prop> <val>]} to Pathom's {<id prop> <val>}
            ;; 2. For nested child entities (created using the {} form of tx-data), transform recursively
            (ref? env k) ; should always be a map for a ref *in theory*
            (->> (ensure-ref-is-map! v) (util/map-over-many-or-one to-many?? (partial asami-ref->pathom! k))))))

(defn transform-entity
  "Adjust raw Asami data so that it is suitable for Pathom"
  [{_ ::attr/key->attribute :as env} entity]
  {:pre [entity (::attr/key->attribute env)]}
  (reduce-kv (fn [m k v] (assoc m k (transform-entity-property env k v))) {}
             ;; rm :id b/c it is an ident and the entity also has {<pk> pk-val} ; x FEAT-NAT-IDS
             (dissoc entity :id)))

(comment
  (transform-entity-property {::attr/key->attribute {:e/my-ref #::attr{:qualified-key :e/my-ref, :type :ref, :cardinality :many}}}
                             :e/my-ref {:id [:entity/id 123]})
 (transform-entity {::attr/key->attribute {:e/my-ref #::attr{:qualified-key :e/my-ref, :type :ref, :cardinality :many}}}
                   {:e/random-val "str"
                    :e/my-ref    {:id [:entity/id 123]}}))


(defn- ids->entities
  ([db qualified-key ids] (ids->entities db qualified-key ids false))
  ([db qualified-key ids nested?]
   ;; We require that each entity has `:id [:<entity>/id <id value>]` and thus use that for the lookup:
   ;; (if we supported native IDs then we could also (asami.graph/new-node long-id-value) but would need the
   ;; RAD attribute to decide
   (sequence
     ;; NOTE: For refs this will return just *ID-maps*; ex.: `{:id [:address/id 123]}`. To return the full
     ;; data of the child, we would need to pass the 3rd argument (nested?) as true
     (comp (remove tempid/tempid?)
           (map #(d/entity db [qualified-key %] nested?))
           (remove nil?)) ; note: remove nil? messes up with Pathom batching expecting the same number of results, with
                          ; id for the missing; fortunately P. provides a fn to fix it upstream, which we do
     ids)))

(comment
  ;(d/q '[:find :db/retract ?e ?a ?v :where [?e ?a ?v] [?e :id ?id] :in $ ?id]
  ;     (first *args) [:cz.holyjak.rad.test-schema.address/id #uuid "ffffffff-ffff-ffff-ffff-000000000001"])
  ;(-> (apply ids->entities *args) first transform-entity)
  ;(d/q '[:find ?e ?a ?v :where [?e ?a ?v]] (first *args))
  ;(ids->entities *db :cz.holyjak.rad.test-schema.person/id [#uuid "ffffffff-ffff-ffff-ffff-000000000100"])
  ;(d/q '[:find ?a ?v :where [?e :cz.holyjak.rad.test-schema.person/id] [?e ?a ?v]] *db)
  )

(>def ::id-map (s/map-of keyword? any?))
(>def ::id-map-or-maps (s/or :one ::id-map
                             :many (s/every ::id-map)))
(>def ::entity-or-entities (s/or :one (s/nilable map?)
                                 :many (s/every map?)))


(>def ::attr/key->attribute ::attr/attribute-map)
(>def ::asami/id-attribute (s/keys :req [::attr/qualified-key]))
(>def ::env (s/keys :req [::attr/key->attribute
                          ::asami/id-attribute]))

(>defn entities
  "Query the database for an entity or entities. Uses the `id-attribute` to get the entity's id attribute name and the
  `input` that should contain the id(s) that need to be queried for (ex.: `{:order/id 1}` or `[{:order/id 1} ..]`).
  Returns the data in a Pathom-friendly way (i.e. to-many value becomes always a vector)."
  [{_ ::attr/key->attribute ::asami/keys [id-attribute] :as env}
   input
   db]
  [map? ::id-map-or-maps any? => ::entity-or-entities]
  (let [{::attr/keys [qualified-key] ::asami/keys [fetch-nested?]} id-attribute
        batch? (sequential? input)]
    (let [ids (if batch?
                (into [] (keep qualified-key) input)
                [(get input qualified-key)])
          entities (ids->entities db qualified-key ids fetch-nested?)
          result (sequence (map (partial transform-entity (select-keys env [::attr/key->attribute]))) entities)]
      (if batch?
        result
        (first result)))))

(comment
  ;(ids->entities cz.holyjak.rad.database-adapters.asami.core/dbm :order/id [1])

  ;(entities {::asami/id-attribute {::attr/qualified-key :order/id}}
  ;          {:order/id 2}
  ;          (d/connect (cz.holyjak.rad.database-adapters.asami/config->url {:asami/driver :local, :asami/database "playground3"})))

  ;(sp/select
  ;  (sp/walker #(and (map? %) (= :join (:type %))))
  ;  ; map with :type :join -> get :key
  ;  (eql/query->ast [:person/id
  ;                   {:person/addresses [:address/id :address/street]}
  ;                   {:person/things [:thing/id :thing/label]}]))
  ;(seq (map *eid->ent *entry-eids))
  ;(keep (fn [[eid ent]] (when (:cz.holyjak.rad.test-schema.person/id ent) eid)) *eid->ent)
  ;(:identities (:cz.holyjak.rad.test-schema.person/role *key->attribute))
  )

; ------------------------------------------------------------------------------------------------ EXPERIMENTAL

;[incoming-query         [::person/id
;                         {::person/addresses [::address/id ::address/street]}
;                         {::person/things [::thing/id ::thing/label]}]
;
; expected-asami-query [:db/id
;                       {::person/addresses [::address/id ::address/street]}
;                       {::person/things [:db/id ::thing/label]}]]
;(>defn pathom-query->asami-query
;  ;; TODO: Support custom where conditions or at least finding the target entity (as in [:user/email "me@.."])!
;  "EQL -> [query attr-filter], to be run like `(d/q query db attr-filter)`"
;  [_all-attributes pathom-query]
;  [::attr/attributes ::eql/query => vector?]
;  ;; EACH LEVEL:
;  ;;  1. content = keywords and ffirst of maps => add to attr filter [could also (or [?e-cnt ?attr1] ...)]
;  ;;  2. joins - add `[?e <join attr>]` and `[_ <join attr> ?e]` to entity filter
;  ;; THEN: Run query, group triplets by entity id into a lookup eid -> entity map, re-construct data tree wrt query
;  (let [{:keys [attrs joins]}
;        (->>
;          (tree-seq (complement keyword?)
;                    #(if (map? %) (vals %) %)
;                    pathom-query)
;          (filter (some-fn keyword? map?))
;          (group-by #(if (keyword? %) :attrs :joins)))
;
;        join-attrs
;        (map ffirst joins)
;
;        entity-filter
;        (->> join-attrs
;             (mapcat (fn [join-prop] [['?e join-prop] ['_ join-prop '?e]]))
;             (cons 'or))]
;    ;; NOTE: Paula - This does try to look for the :order/descr attributes on products and the :product/name
;    ;; attribute on orders, but that's actually cheap, and probably cheaper than anything else.
;    ;; An alternative (might be slower + shows limits of bindings):
;    ;[:find ?e ?a ?v
;    ; :where [?e ?a ?v]
;    ; (or (and [(identity :order/descr) ?a] [?e :order/product _])
;    ;     (and [(identity :product/name) ?a] [_ :order/product ?e]))]
;    [{:find '[?e ?a ?v]
;      :in '[$ [?a ...]]
;      :where ['[?e ?a ?v] entity-filter]}
;     (into attrs join-attrs)]))
#_(d/q '[:find ?e ?a ?v
         :in $ [?a ...]
         :where [?e ?a ?v]
         (or [?e :order/product]
             [_ :order/product ?e])]
       dbm
       [:order/descr :product/name])
;
;(comment
;  (pathom-query->asami-query nil [:order/descr {:order/product [:product/name]}]))
;
;(defn- prop->id-prop [key->attribute attr-key]
;  (let [attr (key->attribute attr-key)]
;    (if (ao/identity? attr)
;      (ao/qualified-key attr)
;      (util/ensure! (first (ao/identities attr))
;               (str "Attribute " attr-key " lacks " ao/identities)))))

; ([#a/n[4] :order/descr "Order XYZ"] [#a/n[4] :order/product #a/n[5]] [#a/n[5] :product/name "Bread"])
;(defn asami-result->pathom-result
;  "Triplets from a Datalog query -> data tree"
;  [key->attribute pathom-query asami-result]
;  (let [join? #(= :ref (::attr/type (get key->attribute %)))
;        maybe-ensure-seq (fn [k v] (cond-> v
;                                           (and (= :many (::attr/cardinality (get key->attribute k)))
;                                                (not (sequential? v)))
;                                           (vector)))
;        first-prop (-> (eql/query->ast pathom-query) :children first :key)
;        entry-entity-id-prop (prop->id-prop key->attribute first-prop)
;        eid->ent (update-vals
;                   (group-by first asami-result)
;                   (partial reduce (fn [m [_ k v]]
;                                     (update m k #(cond
;                                                    (nil? %) v
;                                                    ;; append to existing multi-valued attr.:
;                                                    (vector? %) (conj v)
;                                                    ;; merge existing 1 value with new:
;                                                    :else [% v])))
;                            {}))
;        entry-eids (set (keep (fn [[eid ent]] (when (entry-entity-id-prop ent) eid)) eid->ent))
;        entry-entities (map eid->ent entry-eids)
;        resolve-reference (fn resolve-reference [val] (if (sequential? val)
;                                                        (map resolve-reference val)
;                                                        (get eid->ent val :N/A #_val)))
;        result (walk/prewalk
;                 (fn resolve-references [form]
;                   (if-let [[k v] (and (map-entry? form)
;                                       (join? (key form))
;                                       form)]
;                     #?(:clj
;                        (clojure.lang.MapEntry/create
;                          k
;                          (maybe-ensure-seq k (resolve-reference v)))
;                        :cljs
;                        [k (maybe-ensure-seq k (resolve-reference v))])
;                     form))
;                 entry-entities)]
;    (when (seq asami-result)
;      (assert (seq entry-entities) "Could not find top-level entities in the result => bug / forgot to include <entity>/id in query?!"))
;    (if (next result)
;      (vec result)
;      (first result))))
;
;(defn pull [{:keys [all-attributes key->attribute]} pathom-query]
;  (->> (pathom-query->asami-query all-attributes pathom-query)
;       (asami-result->pathom-result key->attribute pathom-query)))
;
;(comment
;  (asami-result->pathom-result
;    :TODO
;    :TODO
;    (list [4 :order/id 1] [4 :order/descr "Order XYZ"] [4 :order/product 5] [5 :product/name "Bread"])))
