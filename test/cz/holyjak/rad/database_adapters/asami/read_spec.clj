(ns cz.holyjak.rad.database-adapters.asami.read-spec
  (:require
    [fulcro-spec.core :refer [specification assertions component behavior when-mocking => =fn=>]]
    [com.fulcrologic.rad.ids :as ids]
    [cz.holyjak.rad.test-schema.person :as person]
    [cz.holyjak.rad.test-schema.address :as address]
    [cz.holyjak.rad.test-schema.thing :as thing]
    [com.fulcrologic.rad.attributes :as attr]
    [cz.holyjak.rad.database-adapters.asami.read :as common]
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


(defn with-env [tests]
  (binding [*env* {::attr/key->attribute key->attribute}]
    (tests)))

(use-fixtures :each with-env)

;(specification "Pull query transform"
;               (component "pathom-query->asami-query"
;                          (let [incoming-query         [::person/id
;                                                        {::person/addresses [::address/id ::address/street]}
;                                                        {::person/things [::thing/id ::thing/label]}]
;                                expected-asami-query '[{:find [?e ?a ?v],
;                                                        :in [$ [?a ...]],
;                                                        :where [[?e ?a ?v]
;                                                                (or [?e ::person/addresses] ; TODO Perhaps change this and [?e ::person/things] to [?e ::person/id] ?
;                                                                    [_ ::person/addresses ?e]
;                                                                    [?e ::person/things]
;                                                                    [_ ::person/things ?e])]}
;                                                       [::person/id ::address/id ::address/street ::thing/id ::thing/label ::person/addresses ::person/things]]
;                                actual-query           (common/pathom-query->asami-query all-attributes incoming-query)]
;                            (assertions
;                              "can convert a nested pathom query to a proper asami query"
;                              actual-query => expected-asami-query)))
;
;               (component "asami-result->pathom-result"
;                          (behavior "single, flat result"
;                                    (let [pathom-query    [::person/id ::person/email ::person/role]
;                                          asami-result [[:n1 ::person/id 100]
;                                                        [:n1 ::person/role :user]
;                                                        [:n1 ::person/email "u@example.com"]]
;                                          pathom-result   (common/asami-result->pathom-result key->attribute pathom-query asami-result)
;                                          expected-result {::person/id        100
;                                                           ::person/role :user
;                                                           ::person/email "u@example.com"}]
;                                      (assertions
;                                        "can convert a 1-level asami result to a proper Pathom response"
;                                        pathom-result => expected-result)))
;                          (behavior "multiple flat results"
;                                    (let [pathom-query    [::person/id ::person/role]
;                                          asami-result [[:n1 ::person/id 100] [:n1 ::person/role :user]
;                                                        [:n2 ::person/id 200] [:n2 ::person/role :admin]]
;                                          pathom-result   (common/asami-result->pathom-result key->attribute pathom-query asami-result)
;                                          expected-result #{{::person/id 100 ::person/role :user}
;                                                            {::person/id 200 ::person/role :admin}}]
;                                      (assertions
;                                        "can convert a 1-level asami result of multiple entities to a proper Pathom response"
;                                        (set pathom-result) => expected-result
;                                        pathom-result =fn=> vector?)))
;                          (behavior "1-level nested result"
;                                    (let [pathom-query [::person/id
;                                                        {::person/addresses [::address/id ::address/street]}
;                                                        {::person/things [::thing/id ::thing/label]}]
;                                          asami-result [[:n1 ::person/id 100]
;                                                        [:n1 ::person/addresses :n2]
;                                                        [:n1 ::person/things :n3]
;                                                        [:n2 ::address/id (ids/new-uuid 1)]
;                                                        [:n2 ::address/street "111 Main St"]
;                                                        [:n3 ::thing/id 191]
;                                                        [:n3 ::thing/label "ABC"]]
;                                          pathom-result (common/asami-result->pathom-result key->attribute pathom-query asami-result)
;                                          expected-result {::person/id 100
;                                                           ::person/addresses [{::address/id (ids/new-uuid 1)
;                                                                                ::address/street "111 Main St"}]
;                                                           ::person/things [{::thing/id 191
;                                                                             ::thing/label "ABC"}]}]
;                                      (assertions
;                                        "can convert a recursive asami result to a proper Pathom response"
;                                        pathom-result => expected-result)))
;                          (behavior "2-level nested result" ; + a to-one ref attribute
;                                    (let [pathom-query [::person/id
;                                                        {::person/addresses [::address/id ::address/street
;                                                                             {::address/city [::address/city-id ::address/city-name]}]}]
;                                          asami-result [[:n1 ::person/id 100]
;                                                        [:n1 ::person/addresses :n2]
;                                                        [:n2 ::address/id (ids/new-uuid 1)]
;                                                        [:n2 ::address/street "Ukrajinska 13"]
;                                                        [:n2 ::address/city :n3]
;                                                        [:n3 ::address/city-id (ids/new-uuid 2)]
;                                                        [:n3 ::address/city-name "Prague"]]
;                                          pathom-result (common/asami-result->pathom-result key->attribute pathom-query asami-result)
;                                          expected-result {::person/id 100
;                                                           ::person/addresses [{::address/id (ids/new-uuid 1)
;                                                                                ::address/street "Ukrajinska 13"
;                                                                                ::address/city {::address/city-id (ids/new-uuid 2)
;                                                                                                ::address/city-name "Prague"}}]}]
;                                      (assertions
;                                        "can convert a deeply recursive asami result to a proper Pathom response"
;                                        pathom-result => expected-result)))))

(comment
  (do
    (require 'fulcro-spec.reporters.repl)
    (fulcro-spec.reporters.repl/run-tests)))

#_
(specification "intermediate ID generation"
               (let [id1    (tempid/tempid (ids/new-uuid 1))
                     id2    (tempid/tempid (ids/new-uuid 2))
                     delta  {[::person/id id1]  {::person/id        {:after id1}
                                                 ::person/addresses {:after [[::address/id id2]]}}
                             [::address/id id2] {::address/id     {:after id2}
                                                 ::address/street {:after "111 Main St"}}}
                     tmp->m (common/tempid->intermediate-id *env* delta)]
                 (assertions
                   "creates a map from tempid to a string"
                   (get tmp->m id1) => "ffffffff-ffff-ffff-ffff-000000000001"
                   (get tmp->m id2) => "ffffffff-ffff-ffff-ffff-000000000002")))
#_#_
(specification "fail-safe ID"
               (let [id1 (ids/new-uuid 1)
                     tid (tempid/tempid id1)]
                 (assertions
                   "is the asami :db/id when using native IDs"
                   (common/failsafe-id *env* [::person/id 42]) => 42
                   "is the ident when using custom IDs that are not temporary"
                   (common/failsafe-id *env* [::address/id (ids/new-uuid 1)]) => [::address/id (ids/new-uuid 1)]
                   "is a string-version of the tempid when the id of the ident is a tempid"
                   (common/failsafe-id *env* [::person/id tid]) => "ffffffff-ffff-ffff-ffff-000000000001"
                   (common/failsafe-id *env* [::address/id tid]) => "ffffffff-ffff-ffff-ffff-000000000001")))

(specification "delta->txn: simple flat delta, new entity, non-native ID. CREATE"
               (let [id1             (tempid/tempid (ids/new-uuid 1))
                     expected-new-id (ids/new-uuid 2)
                     str-id          (str (:id id1))
                     delta           {[::address/id id1] {::address/id     {:after id1}
                                                          ::address/street {:after "111 Main St"}}}]
                 (when-mocking
                   (common/next-uuid) => expected-new-id

                   (let [{:keys [tempid->string txn]} (common/delta->txn *env* :production delta)]
                     (assertions
                       "includes tempid temporary mapping"
                       (get tempid->string id1) => "ffffffff-ffff-ffff-ffff-000000000001"
                       "Includes an add for the specific facts changed"
                       txn => [[:db/add str-id ::address/id expected-new-id]
                               [:db/add str-id ::address/street "111 Main St"]])))))
