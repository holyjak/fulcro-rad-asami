(ns cz.holyjak.rad.database-adapters.asami-spec
  "High-level 'integration' tests independent of implementation details"
  (:require
    [clojure.test :refer [use-fixtures]]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.form :as form]
    [com.fulcrologic.rad.pathom :as pathom]
    [cz.holyjak.rad.database-adapters.asami :as asami]
    [cz.holyjak.rad.database-adapters.asami.core :as asami-core]
    [cz.holyjak.rad.database-adapters.asami.pathom :as asami-pathom]
    [cz.holyjak.rad.database-adapters.asami.write :as write]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [com.fulcrologic.rad.ids :as ids]
    [cz.holyjak.rad.test-schema.address :as address]
    [cz.holyjak.rad.test-schema.person :as person]
    [cz.holyjak.rad.test-schema.thing :as thing]
    [asami.core :as d]
    [fulcro-spec.core :refer [specification assertions component => =check=> =fn=>]]
    [fulcro-spec.check :as _]
    [cz.holyjak.rad.database-adapters.asami.query :as query]))

(def all-attributes (vec (concat person/attributes address/attributes thing/attributes)))
(def key->attribute (into {}
                          (map (fn [{::attr/keys [qualified-key] :as a}]
                                 [qualified-key a]))
                          all-attributes))

(comment
  (cz.holyjak.rad.database-adapters.asami.util/id? {::attr/key->attribute key->attribute} ::person/id))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save Form Integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(specification "save-form!"
  ;; FIXME Add tests for more variants (2 new, 1 referring to 2.; 1 new + adding ref to it to existing; update singular & multi-val props)
  (let [existing-addr-ident [::address/id (ids/new-uuid 1)]
        _ @(d/transact *conn* (conj (write/new-entity-ident->tx-data existing-addr-ident)
                                    [:db/add [:id existing-addr-ident] ::address/street "A St"]))
        expected-id (ids/new-uuid 100)
        tempid1 (tempid/tempid expected-id)
        delta {[::person/id tempid1] {::person/id tempid1 ; PK value is always as-is, not wrapped in {:after ...}
                                      ::person/full-name {:after "Bob"}
                                      ::person/primary-address {:after [::address/id (ids/new-uuid 1)]}
                                      ::person/role {:after :cz.holyjak.rad.test-schema.person.role/admin}}
               [::address/id (ids/new-uuid 1)] {::address/street {:before "A St" :after "A1 St"}}}]
    (let [{:keys [tempids]} (asami-pathom/save-form! *env* {::form/delta delta})
          real-id (get tempids tempid1)
          person (query/entity-query
                   {::asami/id-attribute {::attr/qualified-key ::person/id}}
                   {::person/id real-id}
                   (d/db *conn*))]
      (assertions
        "Gives a proper remapping"
        (get tempids tempid1) =fn=> uuid?
        "Fulcro tempid is mapped to the uuid it wraps"
        (get tempids tempid1) => expected-id
        "Updates the db with non-ref attributes"
        person =check=> (_/embeds?*
                          {::person/id real-id
                           ::person/full-name "Bob"
                           ::person/role :cz.holyjak.rad.test-schema.person.role/admin
                           ;::person/primary-address {::address/street "A1 St"}
                           #_#_::person/primary-address {:id [::address/id (ids/new-uuid 1)]}})
        "Sets the ref to primary address (and entity-query returns just the ref)"
        (::person/primary-address person) => {:id [::address/id (ids/new-uuid 1)]}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEMPID remapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(specification "save-form! tempid remapping"
               (let [tempid1 (tempid/tempid (ids/new-uuid 101)) ; FIXME Must be unique in DB
                     tempid2 (tempid/tempid (ids/new-uuid 102))
                     delta   {[::person/id tempid1]  {::person/id              tempid1
                                                      ::person/primary-address {:after [::address/id tempid2]}}
                              [::address/id tempid2] {::address/street {:after "A1 St"}}}]
                 (let [{:keys [tempids]} (asami-pathom/save-form! *env* {::form/delta delta})]
                   (assertions
                     ;"Returns a native ID remap for entity using native IDs"
                     ;(pos-int? (get tempids tempid1)) => true
                     "Returns a non-native ID remap for entity using uuid IDs"
                     (uuid? (get tempids tempid2)) => true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME impl test for delete!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Round-trip tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(specification "Pathom parser integration (save + generated resolvers)"
               (let [save-middleware     (asami-pathom/wrap-save)
                     delete-middleware   (asami-pathom/wrap-delete)
                     automatic-resolvers (asami-pathom/generate-resolvers all-attributes :production)
                     parser (pathom/new-parser {}
                                               [(attr/pathom-plugin all-attributes)
                                                (form/pathom-plugin save-middleware delete-middleware)
                                                (asami-pathom/pathom-plugin (fn [env] {:production *conn*}))]
                                               [automatic-resolvers form/resolvers])]
                 #_ ; TODO We do not support native IDs yet (and person has been changed not to have it)
                 (component "Saving new items (native ID)"
                            (let [temp-person-id (tempid/tempid)
                                  delta          {[::person/id temp-person-id] {::person/id        {:after temp-person-id}
                                                                                ::person/full-name {:after "Bob"}}}
                                  {::form/syms [save-form]} (parser {} `[{(form/save-form ~{::form/id        temp-person-id
                                                                                            ::form/master-pk ::person/id
                                                                                            ::form/delta     delta})
                                                                          [:tempids ::person/id ::person/full-name]}])
                                  {:keys [tempids]} save-form
                                  real-id        (get tempids temp-person-id)
                                  entity         (dissoc save-form :tempids)]
                              (assertions
                                "Includes the remapped (native) ID for native id attribute"
                                (pos-int? real-id) => true
                                "Returns the newly-created attributes"
                                entity => {::person/id        real-id
                                           ::person/full-name "Bob"})))
                 (component "Saving new items (generated ID)"
                            (let [temp-address-id (tempid/tempid)
                                  delta           {[::address/id temp-address-id] {::address/id     {:after temp-address-id}
                                                                                   ::address/street {:after "A St"}}}
                                  {::form/syms [save-form]} (parser {} `[{(form/save-form ~{::form/id        temp-address-id
                                                                                            ::form/master-pk ::address/id
                                                                                            ::form/delta     delta}) [:tempids ::address/id ::address/street]}])
                                  {:keys [tempids]} save-form
                                  real-id         (get tempids temp-address-id)
                                  entity          (dissoc save-form :tempids)]
                              (assertions
                                "Includes the remapped (UUID) ID for id attribute"
                                (uuid? real-id) => true
                                "Returns the newly-created attributes"
                                entity => {::address/id     real-id
                                           ::address/street "A St"})))
                 (component "Saving a tree"
                            (let [temp-person-id  (tempid/tempid)
                                  temp-address-id (tempid/tempid)
                                  delta           {[::person/id temp-person-id]
                                                   {::person/id              {:after temp-person-id}
                                                    ::person/role            {:after :cz.holyjak.rad.test-schema.person.role/admin}
                                                    ::person/primary-address {:after [::address/id temp-address-id]}}

                                                   [::address/id temp-address-id]
                                                   {::address/id     {:after temp-address-id}
                                                    ::address/street {:after "A St"}}}
                                  {::form/syms [save-form]} (parser {} `[{(form/save-form ~{::form/id        temp-person-id
                                                                                            ::form/master-pk ::person/id
                                                                                            ::form/delta     delta})
                                                                          [:tempids ::person/id ::person/role
                                                                           {::person/primary-address [::address/id ::address/street]}]}])
                                  {:keys [tempids]} save-form
                                  addr-id         (get tempids temp-address-id)
                                  person-id       (get tempids temp-person-id)
                                  entity          (dissoc save-form :tempids)]
                              (assertions
                                "Returns the newly-created graph"
                                entity => {::person/id              person-id
                                           ::person/role            :cz.holyjak.rad.test-schema.person.role/admin
                                           ::person/primary-address {::address/id     addr-id
                                                                     ::address/street "A St"}})))))

(comment
  (def repl-conn (start-connection))
  (reset-db)
  (let [parser (pathom/new-parser {}
                                  [(attr/pathom-plugin all-attributes)
                                   (asami-pathom/pathom-plugin (fn [env] {:production repl-conn}))]
                                  [(asami-pathom/generate-resolvers all-attributes :production)])]
    ;(parser {} #_asami-pathom/*env [{[::person/id (:id *pid)] [{::person/primary-address [::address/id ::address/street]}]}])
    (parser {} #_asami-pathom/*env [{[::person/id (:id *pid)] [::person/primary-address]}])
    )

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attr Options Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(specification "Attribute Options"
;               (let [person-resolver (first (datomic/generate-resolvers person/attributes :production))]
;                 (component "defattr applies ::pc/transform to the resolver map"
;                            (assertions
;                              "person resolver has been transformed by ::pc/transform"
;                              (do
;                                (log/spy :info person-resolver)
;                                (::person/transform-succeeded person-resolver)) => true
;                              ))))
