(ns cz.holyjak.rad.database-adapters.asami.write-spec
  (:require
    [asami.core :as d]
    [cz.holyjak.rad.database-adapters.asami :as asami]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [cz.holyjak.rad.database-adapters.asami.connect :as asami-core]
    [cz.holyjak.rad.database-adapters.asami.util :as util :refer [ensure!]]
    [fulcro-spec.core :refer [specification assertions component behavior when-mocking => =throws=>]]
    [com.fulcrologic.rad.ids :as ids]
    [cz.holyjak.rad.test-schema.person :as person]
    [cz.holyjak.rad.test-schema.person-quality :as person-quality]
    [cz.holyjak.rad.test-schema.address :as address]
    [cz.holyjak.rad.test-schema.thing :as thing]
    [com.fulcrologic.rad.attributes :as attr]
    [cz.holyjak.rad.database-adapters.asami.write :as write]
    [fulcro-spec.core :refer [specification assertions]]
    [clojure.test :refer [use-fixtures]]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [asami.graph :as graph]))

(def all-attributes (vec (concat person/attributes address/attributes thing/attributes)))
(def key->attribute (into {}
                          (map (fn [{::attr/keys [qualified-key] :as a}]
                                 [qualified-key a]))
                          all-attributes))

(get key->attribute :cz.holyjak.rad.test-schema.person/things)
(keys key->attribute)

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
                     aso/connections       {:production conn}
                     #_#_aso/databases {:production (atom (d/db conn))}}]
      (tests))))

(use-fixtures :once with-reset-database)
(use-fixtures :each with-env)

(specification "ident->props => retractions" ; clear-singular-attributes-txn
  (let [pid (random-uuid)
        db (->> [{:id [:person/id pid]
                  :person/email "before@email.com"}]
                (d/transact *conn*)
                deref
                :db-after)
        db-double-name (->> [[:db/add [:id [:person/id pid]] :person/full-name "Name One"]
                             [:db/add [:id [:person/id pid]] :person/full-name "Name Two"]]
                            (d/transact *conn*)
                            deref
                            :db-after)
        pid2 (random-uuid)
        pid3 (random-uuid)
        db3  (->> [{:id                     [:person/id pid2]
                    :person/email           "two@email.com"
                    :person/role            :user
                    :person/account-balance 300}
                   {:id [:person/id pid3] :person/email "three@email.com"}]
                  (d/transact *conn*)
                  deref
                  :db-after)
        node-id2 (d/q util/q-ident->node-id db3 [:person/id pid2])
        node-id3 (d/q util/q-ident->node-id db3 [:person/id pid3])
        node-id (ensure! (d/q util/q-ident->node-id db [:person/id pid]) "person should exist in the db")]
    (assertions
      "Returns no retractions for entities that do not exist"
      (write/clear-singular-attributes-txn db {[:no-such-thing/id 1] #{:fake/prop}}) => nil
      "Returns no retractions if the property has no current value"
      (write/clear-singular-attributes-txn db {[:person/id pid] #{:person/full-name}}) => nil
      "Returns retraction of the current value"
      (write/clear-singular-attributes-txn db {[:person/id pid] #{:person/email}})
      => [[:db/retract node-id :person/email "before@email.com"]]
      "Fails if there are multiple values for a presumably singular attr"
      (write/clear-singular-attributes-txn db-double-name {[:person/id pid] #{:person/full-name}})
      =throws=> AssertionError
      "Test with multiple props/ident and multiple idents in the input"
      (->
        (write/clear-singular-attributes-txn
          db3
          {[:person/id pid2] #{:person/email :person/role :person/account-balance}
           [:person/id pid3] #{:person/email}})
        set)
        => #{[:db/retract node-id2 :person/email "two@email.com"]
             [:db/retract node-id2 :person/role :user]
             [:db/retract node-id2 :person/account-balance 300]
             [:db/retract node-id3 :person/email "three@email.com"]})))

(specification "delta->singular-attrs-to-clear"
  (let [pid                 (ids/new-uuid 10)
        tempid             (tempid/tempid)
        id1                 (ids/new-uuid 100)
        id2                 (ids/new-uuid 200)]
    (assertions
      "ID excluded, despite being singular"
      (write/delta->singular-attrs-to-clear
        key->attribute :production
        {[::person/id pid] {::person/id pid}})
      => nil
      "Updated singular attribute is to be cleared"
      (write/delta->singular-attrs-to-clear
        key->attribute :production
        {[::person/id pid] {::person/id pid, ::person/full-name {:before "Jo" :after "June"}}})
      => {[::person/id pid] #{::person/full-name}}
      "Removed singular attribute is to be cleared (RAD would remove it too but let's not rely on it having latest value)"
      (write/delta->singular-attrs-to-clear
        key->attribute :production
        {[::person/id pid] {::person/id pid, ::person/full-name {:before "Jo" :after nil}}})
      => {[::person/id pid] #{::person/full-name}}
      "New entity is ignored, no matter its singular attrs"
      (write/delta->singular-attrs-to-clear
        key->attribute :production
        {[::person/id tempid] {::person/id tempid, ::person/full-name {:after "June"}}})
      => nil
      "Newly set singular attribute on existing entity is to be cleared (for I don't want to trust Fulcro's :before 100%)"
      (write/delta->singular-attrs-to-clear
        key->attribute :production
        {[::person/id pid] {::person/id tempid, ::person/email {:after "new@ma.il"}}})
      => {[::person/id pid] #{::person/email}}
      "Non-singular attrs are ignored"
      (write/delta->singular-attrs-to-clear
        key->attribute :production
        {[::person/id pid] {::person/addresses {:before [[::address/id id1]]
                                                :after  [[::address/id id1], [::address/id id2]]}}})
      => nil
      "Attributes from a different schema are ignored"
      (write/delta->singular-attrs-to-clear
        key->attribute :another-schema
        {[::person/id pid] {::person/id pid, ::person/full-name {:before "Jo" :after "June"}}})
      => nil
      "All singular attrs are included"
      (write/delta->singular-attrs-to-clear
        key->attribute :production
        {[::person/id pid] {::person/id pid,
                            ::person/email           {:after "new@ma.il"}
                            ::person/full-name       {:before "Jo" :after "June"}
                            ::person/primary-address {:before [::address/id id1] :after [::address/id id2]}
                            ::person/role            {:before :cz.holyjak.rad.test-schema.person.role/user
                                                      :after  :cz.holyjak.rad.test-schema.person.role/admin}}})
      => {[::person/id pid] #{::person/email ::person/full-name ::person/primary-address ::person/role}}
      "All updated entities are included"
      (write/delta->singular-attrs-to-clear
        key->attribute :production
        {[::person/id pid]  {::person/id              pid,
                             ::person/email           {:after "new@ma.il"}
                             ::person/full-name       {:before "Jo" :after "June"}
                             ::person/primary-address {:before [::address/id id1] :after [::address/id id2]}
                             ::person/role            {:before :cz.holyjak.rad.test-schema.person.role/user
                                                       :after  :cz.holyjak.rad.test-schema.person.role/admin}}
         [::address/id id2] {::address/id     id2
                             ::address/street {:before "Fake" :after "New Street 1"}}})
      => {[::person/id pid] #{::person/email ::person/full-name ::person/primary-address ::person/role}
          [::address/id id2] #{::address/street}})))


(specification "delta->txn-map-with-retractions"
  ; TODO
  )

(comment
  (do
    (require 'fulcro-spec.reporters.repl)
    (fulcro-spec.reporters.repl/run-tests)))