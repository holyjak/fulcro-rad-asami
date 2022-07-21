(ns cz.holyjak.rad.database-adapters.asami.query
  (:require
    [com.fulcrologic.guardrails.core :refer [>defn =>]]
    [com.fulcrologic.rad.attributes :as attr]
    [cz.holyjak.rad.database-adapters.asami :as-alias asami]
    [edn-query-language.core :as eql]
    [taoensso.timbre :as log]
    [com.fulcrologic.rad.attributes-options :as ao]
    [clojure.walk :as walk]
    [asami.core :as d]))

;[incoming-query         [::person/id
;                         {::person/addresses [::address/id ::address/street]}
;                         {::person/things [::thing/id ::thing/label]}]
;
; expected-asami-query [:db/id
;                       {::person/addresses [::address/id ::address/street]}
;                       {::person/things [:db/id ::thing/label]}]]
(>defn pathom-query->asami-query
  ;; TODO: Support custom where conditions or at least finding the target entity (as in [:user/email "me@.."])!
  "EQL -> [query attr-filter], to be run like `(d/q query db attr-filter)`"
  [_all-attributes pathom-query]
  [::attr/attributes ::eql/query => ::eql/query]
  ;; EACH LEVEL:
  ;;  1. content = keywords and ffirst of maps => add to attr filter [could also (or [?e-cnt ?attr1] ...)]
  ;;  2. joins - add `[?e <join attr>]` and `[_ <join attr> ?e]` to entity filter
  ;; THEN: Run query, group triplets by entity id into a lookup eid -> entity map, re-construct data tree wrt query
  (let [{:keys [attrs joins]}
        (->>
          (tree-seq (complement keyword?)
                    #(if (map? %) (vals %) %)
                    pathom-query)
          (filter (some-fn keyword? map?))
          (group-by #(if (keyword? %) :attrs :joins)))

        join-attrs
        (map ffirst joins)

        entity-filter
        (->> join-attrs
             (mapcat (fn [join-prop] [['?e join-prop] ['_ join-prop '?e]]))
             (cons 'or))]
    ;; NOTE: Paula - This does try to look for the :order/descr attributes on products and the :product/name
    ;; attribute on orders, but that's actually cheap, and probably cheaper than anything else.
    ;; An alternative (might be slower + shows limits of bindings):
    ;[:find ?e ?a ?v
    ; :where [?e ?a ?v]
    ; (or (and [(identity :order/descr) ?a] [?e :order/product _])
    ;     (and [(identity :product/name) ?a] [_ :order/product ?e]))]
    [{:find '[?e ?a ?v]
      :in '[$ [?a ...]]
      :where ['[?e ?a ?v] entity-filter]}
     (into attrs join-attrs)]))
#_(d/q '[:find ?e ?a ?v
         :in $ [?a ...]
         :where [?e ?a ?v]
         (or [?e :order/product]
             [_ :order/product ?e])]
       dbm
       [:order/descr :product/name])

(comment
  (pathom-query->asami-query nil [:order/descr {:order/product [:product/name]}]))

(defn- ensure! [v msg]
  (assert v msg)
  v)

(defn- prop->id-prop [key->attribute attr-key]
  (let [attr (key->attribute attr-key)]
    (if (ao/identity? attr)
      (ao/qualified-key attr)
      (ensure! (first (ao/identities attr))
               (str "Attribute " attr-key " lacks " ao/identities)))))

; ([#a/n[4] :order/descr "Order XYZ"] [#a/n[4] :order/product #a/n[5]] [#a/n[5] :product/name "Bread"])
(defn asami-result->pathom-result
  "Triplets from a Datalog query -> data tree"
  [key->attribute pathom-query asami-result]
  (let [join? #(= :ref (::attr/type (get key->attribute %)))
        maybe-ensure-seq (fn [k v] (cond-> v
                                           (and (= :many (::attr/cardinality (get key->attribute k)))
                                                (not (sequential? v)))
                                           (vector)))
        first-prop (-> (eql/query->ast pathom-query) :children first :key)
        entry-entity-id-prop (prop->id-prop key->attribute first-prop)
        eid->ent (update-vals
                   (group-by first asami-result)
                   (partial reduce (fn [m [_ k v]]
                                     (update m k #(cond
                                                    (nil? %) v
                                                    ;; append to existing multi-valued attr.:
                                                    (vector? %) (conj v)
                                                    ;; merge existing 1 value with new:
                                                    :else [% v])))
                            {}))
        entry-eids (set (keep (fn [[eid ent]] (when (entry-entity-id-prop ent) eid)) eid->ent))
        entry-entities (map eid->ent entry-eids)
        resolve-reference (fn resolve-reference [val] (if (sequential? val)
                                                        (map resolve-reference val)
                                                        (get eid->ent val :N/A #_val)))
        result (walk/prewalk
                 (fn resolve-references [form]
                   (if-let [[k v] (and (map-entry? form)
                                       (join? (key form))
                                       form)]
                     #?(:clj
                        (clojure.lang.MapEntry/create
                          k
                          (maybe-ensure-seq k (resolve-reference v)))
                        :cljs
                        [k (maybe-ensure-seq k (resolve-reference v))])
                     form))
                 entry-entities)]
    (when (seq asami-result)
      (assert (seq entry-entities) "Could not find top-level entities in the result => bug / forgot to include <entity>/id in query?!"))
    (if (next result)
      (vec result)
      (first result))))

(defn pull [{:keys [all-attributes key->attribute]} pathom-query]
  (->> (pathom-query->asami-query all-attributes pathom-query)
       (asami-result->pathom-result key->attribute pathom-query)))

;; ---------------------------------------------------------------------------------------------------------

#_  ; Pros: Makes tempid -> id, good if lookup after save??? Cons: Throws on non-uuid IDs
;; ± copied from https://github.com/fulcrologic/fulcro-rad-kvstore/blob/master/src/main/com/fulcrologic/rad/database_adapters/key_value/pathom.cljc
(defn unwrap-id
  "Generate an id. You need to pass a `suggested-id` as a UUID or a tempid. If it is a tempid and the ID column is a UUID, then
  the UUID *from* the tempid will be used."
  [{::attr/keys [key->attribute] :as env} k suggested-id]
  (let [{::attr/keys [type]} (key->attribute k)]
    (cond
      (= :uuid type) (cond
                       (tempid/tempid? suggested-id) (:id suggested-id)
                       (uuid? suggested-id) suggested-id
                       :else (throw (ex-info "Only unwrapping of tempid/uuid is supported" {:id suggested-id})))
      :otherwise (throw (ex-info "Cannot generate an ID for non-uuid ID attribute" {:attribute k})))))

;; ± copied from https://github.com/fulcrologic/fulcro-rad-kvstore/blob/master/src/main/com/fulcrologic/rad/database_adapters/key_value/pathom.cljc
(defn idents->value
  "reference is an ident or a vector of idents, or a scalar (in which case not a reference). Does not do any database
  reading, just changes [table id] to {table id}"
  [reference]
  (cond
    (eql/ident? reference) (apply array-map reference)
    (vector? reference) (mapv idents->value reference)
    :else reference))

;; ± copied from https://github.com/fulcrologic/fulcro-rad-kvstore/blob/master/src/main/com/fulcrologic/rad/database_adapters/key_value/pathom.cljc
(defn transform-entity
  "Transform so all the joins are no longer idents but ident-like entity maps (so they can be resolved by Pathom, if desired)"
  [entity]
  (into {}
        (map (fn [[k v]]
               (if (nil? v)
                 (do
                   (log/warn "nil value in database for attribute" k)
                   [k v])
                 [k (idents->value v)]))
             (dissoc entity :id))))

(defn- ids->entities [db qualified-key ids]
  ;; We require that each entity has `:id [:<entity>/id <id value>]` and thus use that for the look up:
  ;; (if we supported native IDs then we could also (asami.grph/new-node long-id-value) but would need the
  ;; RAD attribute to decide
  (sequence
    ;; NOTE: For refs this will return just *ID-maps*; ex.: `{:id [:address/id 123]}`. To return the full
    ;; data of the child, we would need to pass the 3rd argument (nested?) as true
    (comp (map #(d/entity db [qualified-key %]))
          (remove nil?))
    ids))

(comment
  ;(d/q '[:find :db/retract ?e ?a ?v :where [?e ?a ?v] [?e :id ?id] :in $ ?id]
  ;     (first *args) [:cz.holyjak.rad.test-schema.address/id #uuid "ffffffff-ffff-ffff-ffff-000000000001"])
  ;(-> (apply ids->entities *args) first transform-entity)
  ;(d/q '[:find ?e ?a ?v :where [?e ?a ?v]] (first *args))
  ;(ids->entities *db :cz.holyjak.rad.test-schema.person/id [#uuid "ffffffff-ffff-ffff-ffff-000000000100"])
  ;(d/q '[:find ?a ?v :where [?e :cz.holyjak.rad.test-schema.person/id] [?e ?a ?v]] *db)
  )

;; ± copied from https://github.com/fulcrologic/fulcro-rad-kvstore/blob/master/src/main/com/fulcrologic/rad/database_adapters/key_value/pathom.cljc
(>defn entity-query
  "Query the database for an entity. Uses the `id-attribute` to get the entity's id attribute name and the `input` that
  should contain the id(s) that need to be queried for (ex.: `{:order/id 1}` or `[{:order/id 1} ..]`)"
  [{::asami/keys [id-attribute]
    :as _env}
   input
   db]
  [map? any? any? => any?]
  (let [{::attr/keys [qualified-key]} id-attribute
        one? (not (sequential? input))]
    (assert id-attribute)
    (assert qualified-key)
    (assert (or (map? input) (sequential? input)))
    (let [ids (if one?
                [(qualified-key input)]
                (into [] (keep qualified-key) input))
          #_#_ids (map #(unwrap-id env qualified-key %) ids)
          entities (ids->entities db qualified-key ids)
          result (mapv transform-entity entities)]
      (if one?
        (first result)
        result))))

  (comment
    ;(ids->entities cz.holyjak.rad.database-adapters.asami.core/dbm :order/id [1])
    (entity-query {::asami/id-attribute {::attr/qualified-key :order/id}}
                  {:order/id 2}
                  (d/connect (asami/config->url {:asami/driver :local, :asami/database "playground3"})))

    ;(sp/select
    ;  (sp/walker #(and (map? %) (= :join (:type %))))
    ;  ; map with :type :join -> get :key
    ;  (eql/query->ast [:person/id
    ;                   {:person/addresses [:address/id :address/street]}
    ;                   {:person/things [:thing/id :thing/label]}]))
    ;(seq (map *eid->ent *entry-eids))
    ;(keep (fn [[eid ent]] (when (:cz.holyjak.rad.test-schema.person/id ent) eid)) *eid->ent)
    ;(:identities (:cz.holyjak.rad.test-schema.person/role *key->attribute))
    (asami-result->pathom-result
      :TODO
      :TODO
      (list [4 :order/id 1] [4 :order/descr "Order XYZ"] [4 :order/product 5] [5 :product/name "Bread"])))
