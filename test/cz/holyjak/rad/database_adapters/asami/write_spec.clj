(ns cz.holyjak.rad.database-adapters.asami.write-spec
  (:require
    [asami.core :as d]
    [cz.holyjak.rad.database-adapters.asami :as asami]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [cz.holyjak.rad.database-adapters.asami.connect :as asami-core]
    [cz.holyjak.rad.database-adapters.asami.util :as util :refer [ensure!]]
    [fulcro-spec.core :refer [specification assertions component behavior when-mocking => =fn=>]]
    [com.fulcrologic.rad.ids :as ids]
    [cz.holyjak.rad.test-schema.person :as person]
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

(specification "ident->props => retractions"
  (let [pid (random-uuid)
        db (->> [{:id [:person/id pid]
                  :person/email "before@email.com"}]
                (d/transact *conn*)
                deref
                :db-after)
        _   (tap> (d/q '[:find ?e ?a ?v :where [?e ?a ?v]] db)) ; FIXME rm
        node-id (ensure! (d/q util/q-ident->node-id db [:person/id pid]) "person should exist in the db")]
    (assertions
      "Returns no retractions for entities that do not exist"
      (write/clear-singular-attributes-txn db {[:no-such-thing/id 1] #{:fake/prop}}) => nil
      "Returns no retractions if the property has no current value"
      (write/clear-singular-attributes-txn db {[:person/id pid] #{:person/full-name}}) => nil
      "Returns retraction of the current value"
      (write/clear-singular-attributes-txn db {[:person/id pid] #{:person/email}})
      => [[:db/retract node-id :person/email "before@email.com"]]
      ; TODO fails if there are multiple values for a presumably singular attr
      ; TODO Test with multiple props/ident and multiple idents in the input
      )))

(comment
  (do
    (require 'fulcro-spec.reporters.repl)
    (fulcro-spec.reporters.repl/run-tests)))