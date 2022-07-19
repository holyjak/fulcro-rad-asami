(ns cz.holyjak.rad.database-adapters.asami-impl-spec
  "Low-level, implementation-dependat tests"
  (:require
    [clojure.set :as set]
    [clojure.test :refer [use-fixtures]]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [com.fulcrologic.rad.attributes :as attr]
    [cz.holyjak.rad.database-adapters.asami :as asami]
    [cz.holyjak.rad.database-adapters.asami.core :as asami-core]
    [cz.holyjak.rad.database-adapters.asami.write :as write]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [com.fulcrologic.rad.ids :as ids]
    [cz.holyjak.rad.test-schema.address :as address]
    [cz.holyjak.rad.test-schema.person :as person]
    [cz.holyjak.rad.test-schema.thing :as thing]
    [asami.core :as d]
    [fulcro-spec.core :refer [specification assertions =fn=>]]))

(def all-attributes (vec (concat person/attributes address/attributes thing/attributes)))
(def key->attribute (into {}
                          (map (fn [{::attr/keys [qualified-key] :as a}]
                                 [qualified-key a]))
                          all-attributes))

(def ^:dynamic *conn* nil)
(def ^:dynamic *env* {})

(def asami-config {:asami/driver :mem, :asami/database "test"})

(defn start-connection []
  (:production (asami/start-connections {aso/databases {:production asami-config}})))

(defn reset-db []
  (d/delete-database (asami-core/config->url asami-config)))

(defn with-reset-database [tests]
  (reset-db)
  (tests))

(defn with-env [tests]
  (let [conn (start-connection)]
    (binding [*conn* conn
              *env* {::attr/key->attribute key->attribute
                     aso/connections {:production conn}
                     #_#_aso/databases {:production (atom (d/db conn))}}]
      (tests))))

(use-fixtures :once with-reset-database)
(use-fixtures :each with-env)

(defn runnable? [txn]
  (try
    @(d/transact *conn* txn)
    true
    (catch Exception e
      false #_(.getMessage e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To-one
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(specification "delta->txn: simple flat delta, existing entity, non-native ID. UPDATE to-one"
               (let [id (ids/new-uuid 1)
                     _ @(d/transact *conn* [{:id [::address/id (ids/new-uuid 1)]
                                             ::address/id (ids/new-uuid 1)
                                             ::address/street "111 Main"}])
                     delta {[::address/id id] {::address/id {:before id :after id}
                                               ::address/street {:before "111 Main" :after "111 Main St"}}}]
                 (let [{:keys [tempid->txid txn]} (write/delta->txn *env* :production delta)]
                   (assertions
                     "has no tempid mappings"
                     (empty? tempid->txid) => true
                     "Includes lookup refs for the non-native ID, and changes for the facts that changed"
                     txn => [[:db/add [:id [::address/id id]] ::address/street' "111 Main St"]]
                     (runnable? txn) => true)))
               (let [id (ids/new-uuid 1)
                     delta {[::address/id id] {::address/id {:before id :after id}
                                               ::address/enabled? {:before nil :after false}}}]
                 (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                   (assertions
                     "setting boolean false does an add"
                     txn => [[:db/add
                              [:id [:cz.holyjak.rad.test-schema.address/id
                                    #uuid "ffffffff-ffff-ffff-ffff-000000000001"]]
                              :cz.holyjak.rad.test-schema.address/enabled?'
                              false]])))
               (let [id (ids/new-uuid 1)
                     delta {[::address/id id] {::address/id {:before id :after id}
                                               ::address/enabled? {:before false :after nil}}}]
                 (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                   (assertions
                     "removing boolean false does a retract"
                     txn => [[:db/retract
                              [:id [:cz.holyjak.rad.test-schema.address/id
                                    #uuid "ffffffff-ffff-ffff-ffff-000000000001"]]
                              :cz.holyjak.rad.test-schema.address/enabled?
                              false]]))))

(specification "delta->txn: simple flat delta, existing entity, non-native ID. ADD to-one ATTRIBUTE"
               (let [id (ids/new-uuid 1)
                     _ @(d/transact *conn* [{:id [::address/id (ids/new-uuid 1)] ::address/id (ids/new-uuid 1)}])
                     delta {[::address/id id] {::address/street {:after "111 Main St"}}}]
                 (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                   (assertions
                     "Includes lookup refs for the non-native ID, and changes for the facts that changed"
                     txn => [[:db/add [:id [::address/id id]] ::address/street' "111 Main St"]]
                     (runnable? txn) => true))))

(specification "delta->txn: simple flat delta, existing entity, non-native ID. DELETE to-one ATTRIBUTE"
               (let [id (ids/new-uuid 1)
                     _ @(d/transact *conn* [{:id [::address/id id]
                                             ::address/id id
                                             ::address/street "111 Main"}])
                     delta {[::address/id id] {::address/id {:before id :after id}
                                               ::address/street {:before "111 Main" :after nil}}}]
                 (let [{:keys [tempid->txid txn]} (write/delta->txn *env* :production delta)]
                   (assertions
                     "has no tempid mappings"
                     (empty? tempid->txid) => true
                     "Includes lookup refs for the non-native ID, and changes for the facts that changed"
                     txn => [[:db/retract [:id [::address/id id]] ::address/street "111 Main"]]
                     (runnable? txn) => true))))

#_; TODO fix?! - native id support
    (specification "delta->txn: simple flat delta, new entity, native ID"
                   (let [id1 (tempid/tempid (ids/new-uuid 1))
                         str-id (str (:id id1))
                         delta {[::person/id id1] {::person/id {:after id1}
                                                   ::person/email {:after "joe@nowhere.com"}}}]
                     (let [{:keys [tempid->txid txn]} (write/delta->txn *env* :production delta)]
                       (assertions
                         "includes tempid temporary mapping"
                         (get tempid->txid id1) => -1
                         "Includes an add for the specific facts changed"
                         txn => [[:db/add str-id ::person/email "joe@nowhere.com"]]
                         (runnable? txn) => true))))

#_; TODO fix?! - native id support
    (specification "delta->txn: simple flat delta, existing entity, native ID, ADD attribute"
                   (let [{{:strs [id]} :tempids} @(d/transact *conn* [{:db/id "id"
                                                                       ::person/full-name "Bob"}])
                         delta {[::person/id id] {::person/email {:after "joe@nowhere.net"}}}]
                     (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                       (assertions
                         "Includes simple add based on real datomic ID"
                         txn => [[:db/add id ::person/email "joe@nowhere.net"]]
                         (runnable? txn) => true))))

#_; TODO fix?! - native id support
    (specification "delta->txn: simple flat delta, existing entity, native ID, UPDATE attribute"
                   (let [{{:strs [id]} :tempids} @(d/transact *conn* [{:db/id "id"
                                                                       ::person/full-name "Bob"}])
                         delta {[::person/id id] {::person/full-name {:before "Bob" :after "Bobby"}}}]
                     (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                       (assertions
                         "Includes simple add based on real datomic ID"
                         txn => [[:db/add id ::person/full-name "Bobby"]]
                         (runnable? txn) => true))))

#_; TODO fix?! - native id support
    (specification "delta->txn: simple flat delta, existing entity, native ID, DELETE attribute"
                   (let [{{:strs [id]} :tempids} @(d/transact *conn* [{:db/id "id"
                                                                       ::person/full-name "Bob"}])
                         delta {[::person/id id] {::person/full-name {:before "Bob"}}}]
                     (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                       (assertions
                         "Includes simple add based on real datomic ID"
                         txn => [[:db/retract id ::person/full-name "Bob"]]
                         "Is a runnable txn"
                         (runnable? txn) => true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To-one Enumerations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_; TODO fix?! - native id support
    (specification "delta->txn: simple flat delta, existing entity, native ID, ADD to-one enum"
                   (let [{{:strs [id]} :tempids} @(d/transact *conn* [{:db/id "id"
                                                                       ::person/full-name "Bob"}])
                         delta {[::person/id id] {::person/role {:after :cz.holyjak.rad.test-schema.person.role/admin}}}]
                     (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                       (assertions
                         "Includes simple add based on real datomic ID"
                         txn => [[:db/add id ::person/role :cz.holyjak.rad.test-schema.person.role/admin]]
                         (runnable? txn) => true))))

#_; TODO fix?! - native id support
    (specification "delta->txn: simple flat delta, existing entity, native ID, REMOVE to-one enum"
                   (let [{{:strs [id]} :tempids} @(d/transact *conn* [{:db/id "id"
                                                                       ::person/role :cz.holyjak.rad.test-schema.person.role/admin
                                                                       ::person/full-name "Bob"}])
                         delta {[::person/id id] {::person/role {:before :cz.holyjak.rad.test-schema.person.role/admin}}}]
                     (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                       (assertions
                         "Includes simple add based on real datomic ID"
                         txn => [[:db/retract id ::person/role :cz.holyjak.rad.test-schema.person.role/admin]]
                         (runnable? txn) => true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To-many Enumerations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(specification "delta->txn: simple flat delta, existing entity, non-native ID, UPDATE (ADD) to-many enum"
               (let [id (rand-int 1000)
                     ident [::person/id id]
                     _ @(d/transact *conn* [{:id ident
                                             ::person/permissions [:cz.holyjak.rad.test-schema.person.permissions/read
                                                                   :cz.holyjak.rad.test-schema.person.permissions/write]}])
                     delta {[::person/id id] {::person/permissions {:before [:cz.holyjak.rad.test-schema.person.permissions/read
                                                                             :cz.holyjak.rad.test-schema.person.permissions/write]
                                                                    :after [:cz.holyjak.rad.test-schema.person.permissions/read
                                                                            :cz.holyjak.rad.test-schema.person.permissions/execute
                                                                            :cz.holyjak.rad.test-schema.person.permissions/write]}}}]
                 (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                   (assertions
                     "Includes simple add based on real datomic ID"
                     txn => [[:db/add [:id ident] ::person/permissions :cz.holyjak.rad.test-schema.person.permissions/execute]]
                     (runnable? txn) => true))))


(specification "delta->txn: simple flat delta, existing entity, non-native ID, UPDATE (add/remove) to-many enum"
               (let [id (rand-int 1000)
                     ident [::person/id id]
                     _ @(d/transact *conn* [{:id ident
                                             ::person/permissions [:cz.holyjak.rad.test-schema.person.permissions/read
                                                                   :cz.holyjak.rad.test-schema.person.permissions/write]}])
                     delta {[::person/id id] {::person/permissions {:before [:cz.holyjak.rad.test-schema.person.permissions/read
                                                                             :cz.holyjak.rad.test-schema.person.permissions/write]
                                                                    :after [:cz.holyjak.rad.test-schema.person.permissions/execute]}}}]
                 (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                   (assertions
                     "Includes simple add based on real datomic ID"
                     (set txn) => #{[:db/add [:id ident] ::person/permissions :cz.holyjak.rad.test-schema.person.permissions/execute]
                                    [:db/retract [:id ident] ::person/permissions :cz.holyjak.rad.test-schema.person.permissions/write]
                                    [:db/retract [:id ident] ::person/permissions :cz.holyjak.rad.test-schema.person.permissions/read]}
                     (runnable? txn) => true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; References to entities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(specification "Completely new entity with references to new children"
               (let [new-person-id (ids/new-uuid 1)
                     new-address-id1 (ids/new-uuid 2)
                     new-address-id2 (ids/new-uuid 3)
                     tempid1 (tempid/tempid new-person-id)
                     tempid2 (tempid/tempid new-address-id1)
                     tempid3 (tempid/tempid new-address-id2)
                     delta {[::person/id tempid1] {::person/id {:after tempid1}
                                                   ::person/full-name {:after "Tony"}
                                                   ::person/primary-address {:after [::address/id tempid2]}
                                                   ::person/addresses {:after [[::address/id tempid2] [::address/id tempid3]]}}
                            [::address/id tempid2] {::address/id {:after tempid2}
                                                    ::address/street {:after "A St"}}
                            [::address/id tempid3] {::address/id {:after tempid3}
                                                    ::address/street {:after "B St"}}}

                     expected #{{:id [::person/id new-person-id]
                                 ::person/id new-person-id}
                                [:db/add [:id [::person/id new-person-id]] :cz.holyjak.rad.test-schema.person/full-name "Tony"]
                                [:db/add [:id [::person/id new-person-id]] :cz.holyjak.rad.test-schema.person/primary-address :id [:cz.holyjak.rad.test-schema.address/id new-address-id1]]
                                [:db/add [:id [::person/id new-person-id]] :cz.holyjak.rad.test-schema.person/addresses [:id [:cz.holyjak.rad.test-schema.address/id new-address-id2]]]
                                [:db/add [:id [::person/id new-person-id]] :cz.holyjak.rad.test-schema.person/addresses :id [:cz.holyjak.rad.test-schema.address/id new-address-id1]]

                                {:id [:cz.holyjak.rad.test-schema.address/id new-address-id1]
                                 :cz.holyjak.rad.test-schema.address/id new-address-id1}
                                [:db/add :id [:cz.holyjak.rad.test-schema.address/id new-address-id1] :cz.holyjak.rad.test-schema.address/street "A St"]

                                {:id [:cz.holyjak.rad.test-schema.address/id new-address-id2]
                                 :cz.holyjak.rad.test-schema.address/id new-address-id2}
                                [:db/add [:id [:cz.holyjak.rad.test-schema.address/id new-address-id2]] :cz.holyjak.rad.test-schema.address/street "B St"]
                                }]
                 (let [{:keys [txn]} (write/delta->txn *env* :production delta)]
                   (assertions
                     "Adds the non-native IDs, and the proper values"
                     (set/difference (set txn) expected) => #{}
                     (set txn) => expected
                     (runnable? txn) => true))))

(specification "Existing entity, add new to-one child"
               (let [person-id (ids/new-uuid 2)
                     _ @(d/transact *conn* [{::person/id person-id
                                             ::person/full-name "Bob"
                                             ::person/addresses [{::address/id (ids/new-uuid 1)
                                                                  ::address/street "A St"}]}])
                     new-address-id1 (ids/new-uuid 1)
                     tempid1 (tempid/tempid new-address-id1)
                     delta {[::person/id person-id] {::person/primary-address {:after [::address/id tempid1]}}
                            [::address/id tempid1] {::address/id {:after tempid1}
                                                    ::address/street {:after "B St"}}}]
                 (let [{:keys [tempid->generated-id txn]} (write/delta->txn *env* :production delta)]
                   (assertions
                     "Returns tempid mappings"
                     tempid->generated-id => {tempid1 (ids/new-uuid 1)}
                     "Adds the new address"
                     (set txn) =fn=> #(contains? % {:id [::address/id new-address-id1], ::address/id new-address-id1, ::address/street "B St"})
                     ;"Sets the street of the new address"
                     ;(set txn) =fn=> #(contains? % [:db/add [:id [::address/id new-address-id1]] ::address/street "B St"])
                     "Replaces the primary address"
                     (set txn) =fn=> #(contains? % [:db/add [:id [::person/id person-id]]
                                                    ::person/primary-address' [:id [::address/id new-address-id1]]])
                     "Valid Asami tx"
                     (runnable? txn) => true))))

(specification "Existing entity, add new to-many child"
               (let [person-id (ids/new-uuid 2)
                     person-ident [::person/id person-id]
                     orig-address-id1 (ids/new-uuid 1)
                     _ @(d/transact *conn* [{:id person-ident
                                             ::person/full-name "Bob"
                                             ::person/addresses [{::address/id orig-address-id1
                                                                  ::address/street "A St"}]}])
                     new-addr-id (ids/new-uuid 3)
                     new-addr-tempid (tempid/tempid new-addr-id)
                     delta {person-ident {::person/addresses {:before [[::address/id orig-address-id1]]
                                                              :after [[::address/id orig-address-id1] [::address/id new-addr-tempid]]}}
                            [::address/id new-addr-tempid] {::address/id {:after new-addr-tempid}
                                                            ::address/street {:after "B St"}}}
                     ;; the test:
                     {:keys [tempid->generated-id txn]} (write/delta->txn *env* :production delta)]
                 (assertions
                   "Includes remappings for new entities"
                   tempid->generated-id => {new-addr-tempid new-addr-id}
                   "Adds the new address"
                   (set txn) =fn=> #(contains? % {:id [::address/id new-addr-id]
                                                  ::address/street "B St"
                                                  ::address/id new-addr-id})
                   "Links the person to the new address"
                   (set txn) =fn=> #(contains? % [:db/add [:id person-ident] ::person/addresses [:id [::address/id new-addr-id]]])
                   ;"Sets the new address' street"
                   ;(set txn) =fn=> #(contains? % [:db/add [:id [::address/id new-addr-id]] ::address/street "B St"])
                   "All transactions accounted for"
                   (count txn) => 2 ; one creating the address, one adding a ref to it to the person
                   "Valid Asami tx"
                   (runnable? txn) => true)))


(specification "Existing entity, replace to-many children"
               (let [person-id (ids/new-uuid 1)
                     person-ident [::person/id person-id]
                     orig-address-id1 (ids/new-uuid 1)
                     _ @(d/transact *conn* [{:id person-ident
                                             ::person/full-name "Bob"
                                             ::person/addresses [{::address/id orig-address-id1
                                                                  ::address/street "A St"}]}])
                     new-address-id1 (ids/new-uuid 100)
                     new-address-tempid1 (tempid/tempid new-address-id1)
                     delta {person-ident {::person/addresses {:before [[::address/id (ids/new-uuid 1)]]
                                                              :after [[::address/id new-address-tempid1]]}}
                            [::address/id new-address-tempid1] {::address/id {:after new-address-tempid1}
                                                                ::address/street {:after "B St"}}}
                     ;; test
                     {:keys [tempid->generated-id txn]} (write/delta->txn *env* :production delta)]
                 (assertions
                   "Includes remappings for new entities"
                   tempid->generated-id => {new-address-tempid1 new-address-id1}
                   "Adds the non-native IDs, and the proper values"
                   (set txn) => #{{:id [::address/id new-address-id1]
                                   ::address/id new-address-id1
                                   ::address/street "B St"}
                                  [:db/add [:id person-ident] ::person/addresses [:id [::address/id new-address-id1]]]
                                  [:db/retract [:id person-ident] ::person/addresses [:id [::address/id (ids/new-uuid 1)]]]
                                  }
                   (runnable? txn) => true)))
