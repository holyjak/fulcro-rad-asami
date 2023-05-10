(ns cz.holyjak.rad.database-adapters.asami-spec
  "High-level 'integration' tests independent of implementation details"
  (:require
    [clojure.set :as set]
    [clojure.test :refer [use-fixtures]]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.attributes-options :as ao]
    [com.fulcrologic.rad.form :as form]
    [cz.holyjak.rad.database-adapters.asami :as asami]
    [cz.holyjak.rad.database-adapters.asami.connect :as asami-core]
    [cz.holyjak.rad.database-adapters.asami.pathom-common :as asami-pathom-common]
    [cz.holyjak.rad.database-adapters.asami.write :as write]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [com.fulcrologic.rad.ids :as ids]
    [cz.holyjak.rad.test-schema.address :as address]
    [cz.holyjak.rad.test-schema.person :as person]
    [cz.holyjak.rad.test-schema.person-quality :as person-quality]
    [cz.holyjak.rad.test-schema.thing :as thing]
    [asami.core :as d]
    [fulcro-spec.core :refer [specification assertions component => =check=> =fn=>]]
    [fulcro-spec.check :as _]
    [cz.holyjak.rad.database-adapters.asami.read :as query]))

(def all-attributes (vec (concat person/attributes
                                 person-quality/attributes
                                 address/attributes
                                 thing/attributes)))
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
  (let [url (asami-core/config->url asami-config)]
    (d/delete-database url)
    (swap! d/connections dissoc url)))


(defn with-env [tests]
  (reset-db)
  (let [conn (start-connection)]
    (binding [*conn* conn
              *env* {::attr/key->attribute key->attribute
                     aso/connections {:production conn}
                     #_#_aso/databases {:production (atom (d/db conn))}}]
      (tests))))

(use-fixtures :each with-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-generated resolvers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn universal-parser [all-attributes all-resolvers]
  (if #_pathom3? (try (require 'com.wsscode.pathom3.connect.operation) true (catch Exception _ false))
    (let [env-middleware (-> (attr/wrap-env all-attributes)
                             (form/wrap-env (asami/wrap-save) (asami/wrap-delete))
                             (asami/wrap-env (fn [_env] {:production *conn*})))]
      ((requiring-resolve 'com.fulcrologic.rad.pathom3/new-processor) {} env-middleware [] all-resolvers))
    ((requiring-resolve 'com.fulcrologic.rad.pathom/new-parser)
      {}
      [(attr/pathom-plugin all-attributes)
       (form/pathom-plugin (asami/wrap-save) (asami/wrap-delete))
       #_:clj-kondo/ignore
       (asami/pathom-plugin (fn [_env] {:production *conn*}))]
      all-resolvers)))

;(comment
;  (let [conn (start-connection)
;        temp-address-id (tempid/tempid)
;        delta {[::address/id temp-address-id] {::address/id {:after temp-address-id}
;                                               ::address/street {:after "A St"}}}
;        ]
;    (binding [*conn* conn
;              *env*  {::attr/key->attribute key->attribute
;                      aso/connections       {:production conn}
;                      #_#_aso/databases {:production (atom (d/db conn))}}]
;
;      ((universal-parser all-attributes [#_automatic-resolvers form/resolvers]) {}
;       `[{(form/save-form ~{::form/id        temp-address-id
;                            ::form/master-pk ::address/id
;                            ::form/delta     delta}) [:tempids ::address/id ::address/street]}]))))

(defmacro universal-defresolver [name args config body]
  (if #_pathom3? (try (require 'com.wsscode.pathom3.connect.operation) true (catch Exception _ false))
    `(com.wsscode.pathom3.connect.operation/defresolver ~name ~args ~config ~body)
    `(do (require 'com.wsscode.pathom.connect)
         (com.wsscode.pathom.connect/defresolver
           ~name ~args ~(set/rename-keys config {ao/pathom3-output ao/pc-output}) ~body))))

#_:clj-kondo/ignore
(universal-defresolver cities-resolver [{city-ids ::cities} _]
  {:com.wsscode.pathom3.connect.operation/output [::all-cities]}
  {::all-cities city-ids})

(specification "Auto-generated resolvers"
  (let [;save-middleware (asami/wrap-save)
        ;delete-middleware (asami/wrap-delete)
        automatic-resolvers (asami/generate-resolvers all-attributes :production)
        parser (universal-parser all-attributes [automatic-resolvers form/resolvers cities-resolver])]
    (component "independent entities"
      (let [p1 [::person/id "bob"]
            a1 [::address/id "osl"]
            txn (concat (write/new-entity-ident->tx-data p1)
                        (write/new-entity-ident->tx-data a1)
                        [[:db/add [:id p1] ::person/full-name "Bob"]
                         [:db/add [:id p1] ::person/nicks "Bobby"]
                         [:db/add [:id p1] ::person/primary-address [:id a1]]
                         [:db/add [:id p1] ::person/addresses [:id a1]]
                         [:db/add [:id a1] ::address/street "Oslo St."]])]
        @(d/transact *conn* txn)
        (assertions
          "Looking up something that does not exist"
          (parser {} [{[::address/id "no such id"] [::address/id ::address/street]} ])
          ;; NOTE: No error returned due to RAD plugins; w/o the we would get also
          ;; {[..] {::address/street ::p/not-found}, ::p/errors ::p/not-found}
          => {[::address/id "no such id"] {::address/id "no such id"}} ; b/c Pathom returns just the ident if it cannot find it and our plugins remove errors)
          "An existing entity is returned with the requested referred entity's details (w/ correct singular & multi-valued props)"
          (parser {} [{[::person/id "bob"] [::person/id ::person/nicks ::person/full-name
                                            {::person/addresses [::address/street]}
                                            {::person/primary-address [::address/street]}]} ])
          => {[::person/id "bob"] {::person/id "bob"
                                   ::person/nicks ["Bobby"],
                                   ::person/full-name "Bob",
                                   ::person/addresses [{::address/street "Oslo St."}]
                                   ::person/primary-address {::address/street "Oslo St."}}}
          "An existing entity referring to a deleted entity"
          (let [_ (asami-pathom-common/delete-entity! *env* (apply hash-map a1))]
            (parser {} [{[::person/id "bob"] [::person/id ::person/full-name
                                              {::person/addresses [::address/street]}
                                              {::person/primary-address [::address/street]}]} ])
            ) => {[::person/id "bob"] {::person/id "bob"
                                                 ::person/full-name "Bob",
                                                 ::person/addresses []
                                                 ::person/primary-address nil}})))
    (component "child entities"
      ;; Use the entity form of tx, which permits specifying nested, dependent entities:
      @(d/transact *conn* {:tx-data [{:id [::address/id "a-one"]
                                      ::address/id "a-one"
                                      ::address/street "First St."}
                                     {:id [::address/id "a-two"]
                                      ::address/id "a-two"
                                      ::address/street "Second St."}
                                     {:id [::person/id "ann"]
                                      ::person/id "ann"
                                      ::person/addresses [{:id [::address/id "a-one"]} {:id [::address/id "a-two"]}]}]})
      (assertions
        "The dependent child entities are returned with the parent entity"
        (parser {} [{[::person/id "ann"] [{::person/addresses [::address/street]}]}])
        => {[::person/id "ann"] {::person/addresses [{::address/street "First St."}
                                                     {::address/street "Second St."}]}}))
    (component "child entities with nested references"
      ;; Use the entity form of tx, which permits specifying nested, dependent entities:
      (d/transact *conn* {:tx-data [{:id [::address/city-id (ids/new-uuid 1)]
                                     ::address/city-id (ids/new-uuid 1)
                                     ::address/city-name "Oslo"}]})
      @(d/transact *conn* {:tx-data [{:id [::address/id "a-one"]
                                      ::address/id "a-one"
                                      ::address/street "First St."
                                      ::address/city [:id [::address/city-id (ids/new-uuid 1)]]}
                                     {:id [::person/id "garry"]
                                      ::person/id "garry"
                                      ::person/addresses [{:id [::address/id "a-one"]}]}]})
      (assertions
        "The dependent child entities have their references resolved"
        (parser {} [{[::person/id "garry"] [{::person/addresses [::address/street
                                                                 {::address/city [::address/city-name]}]}]} ])
        => {[::person/id "garry"] {::person/addresses [{::address/street "First St."
                                                        ::address/city {::address/city-name "Oslo"}}]}}))
    (component "multiple entities"
      (let [id1 (ids/new-uuid 3), id2 (ids/new-uuid 4), id3 (ids/new-uuid 5)
            cities {id1 "Oslo", id2 "Praha", id3 "Ash"}
            cities-backwards (reverse (seq cities))
            city (fn [[id name]] {:id [::address/city-id id] ::address/city-id id ::address/city-name name})
            _address (fn [idx city-id] {:id [::address/id (str "x" idx)]
                                        ::address/id (str "x" idx)
                                        ::address/city [:id [::address/city-id city-id]]})]
        @(d/transact *conn* {:tx-data (mapv city cities)})
        (assertions
          "Output and input returned in the same order"
          (parser {} (mapv (fn [[id]] {[::address/city-id id] [::address/city-name]}) cities))
          => {[::address/city-id id1] {::address/city-name "Oslo"}
              [::address/city-id id2] {::address/city-name "Praha"}
              [::address/city-id id3] {::address/city-name "Ash"}}
          "Output and input returned in the same order even if we reorder the input"
          (parser {} (mapv (fn [[id]] {[::address/city-id id] [::address/city-name]}) cities-backwards))
          => {[::address/city-id id3] {::address/city-name "Ash"}
              [::address/city-id id2] {::address/city-name "Praha"}
              [::address/city-id id1] {::address/city-name "Oslo"}}
          "Correct order even when fetched via a list-returning resolver"
          (parser {::cities [{::address/city-id id1} {::address/city-id id2} {::address/city-id id3}]}
                  [{::all-cities [::address/city-id ::address/city-name]}])
          => {::all-cities [{::address/city-id id1 ::address/city-name "Oslo"}
                            {::address/city-id id2 ::address/city-name "Praha"}
                            {::address/city-id id3 ::address/city-name "Ash"}]})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save Form Integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(specification "save-form!"
  (let [existing-addr-ident [::address/id (ids/new-uuid 1)]
        _ @(d/transact *conn* (conj (write/new-entity-ident->tx-data existing-addr-ident)
                                    [:db/add [:id existing-addr-ident] ::address/street "A St"]))
        id2 (ids/new-uuid 200)
        id3 (ids/new-uuid 300)]
    (component "A new entity referring to an existing one + update existing"
      (let [expected-id (ids/new-uuid 100)
            tempid1 (tempid/tempid expected-id)
            delta {[::person/id tempid1] {::person/id tempid1 ; PK value is always as-is, not wrapped in {:after ...}
                                          ::person/full-name {:after "Bob"}
                                          ::person/primary-address {:after existing-addr-ident}
                                          ::person/role {:after :cz.holyjak.rad.test-schema.person.role/admin}}
                   existing-addr-ident {::address/id (second existing-addr-ident)
                                        ::address/street {:before "A St" :after "A1 St"}}}
            {:keys [tempids]} (asami-pathom-common/save-form! *env* {::form/delta delta})
            real-id (get tempids tempid1)
            person (query/entities
                     {::attr/key->attribute key->attribute, ::asami/id-attribute {::attr/qualified-key ::person/id}}
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
                             #_#_::person/primary-address {:id existing-addr-ident}})
          "Sets the ref to primary address (and entity-query returns it as the entity id)"
          (::person/primary-address person) => (apply hash-map existing-addr-ident))))

    (component "Two new entities, one referring to another"
               (let [tempid2 (tempid/tempid id2)
                     tempid3 (tempid/tempid id3)
                     delta {[::person/id tempid2] {::person/id tempid2
                                                   ::person/full-name {:after "Jo"}
                                                   ::person/primary-address {:after [::address/id tempid3]}
                                                   ::person/addresses {:after [existing-addr-ident [::address/id tempid3]]}
                                                   ::person/role {:after :cz.holyjak.rad.test-schema.person.role/user}}
                            [::address/id tempid3] {::address/id tempid3
                                                    ::address/street {:after "B St"}}}
                     {:keys [tempids]} (asami-pathom-common/save-form! *env* {::form/delta delta})
                     person (query/entities
                              {::attr/key->attribute key->attribute, ::asami/id-attribute {::attr/qualified-key ::person/id}}
                              {::person/id (get tempids tempid2)}
                              (d/db *conn*))]
                 (assertions
                   "Person tempid is mapped to the uuid it wraps"
                   (get tempids tempid2) => id2
                   "Address tempid is mapped to the uuid it wraps"
                   (get tempids tempid3) => id3
                   "Updates the db with non-ref attributes"
                   person =check=> (_/embeds?*
                                     {::person/id id2
                                      ::person/full-name "Jo"
                                      ::person/role :cz.holyjak.rad.test-schema.person.role/user
                                      ;::person/primary-address {::address/street "A1 St"}
                                      #_#_::person/primary-address {:id existing-addr-ident}})
                   "Sets the ref to primary address (and entity-query returns the ::address/id)"
                   (::person/primary-address person) => {::address/id id3}
                   "Sets refs to addresses (and returns them as entity IDs)"
                   (::person/addresses person) => [(apply hash-map existing-addr-ident) {::address/id id3}])))

    (component "Update props and refs in entity (both to new & existing)"
               (let [id4 (ids/new-uuid 400), tempid4 (tempid/tempid id4)
                     delta {[::person/id id2] {::person/id id2
                                               ::person/full-name {:before "Jo" :after "June"}
                                               ::person/primary-address {:before [::address/id id3] :after existing-addr-ident}
                                               ::person/addresses {:before [existing-addr-ident [::address/id id3]]
                                                                   :after [existing-addr-ident [::address/id id4]]}
                                               ::person/role {:before :cz.holyjak.rad.test-schema.person.role/user
                                                              :after :cz.holyjak.rad.test-schema.person.role/admin}}
                            [::address/id tempid4] {::address/id tempid4
                                                    ::address/street {:after "C St"}}}
                     _ (asami-pathom-common/save-form! *env* {::form/delta delta})
                     person (query/entities
                              {::attr/key->attribute key->attribute, ::asami/id-attribute {::attr/qualified-key ::person/id}}
                              {::person/id id2}
                              (d/db *conn*))]
                 (assertions
                   "Updates all singular, multi-valued and ref props as expected"
                   person =check=> (_/embeds?*
                                     {::person/id id2
                                      ::person/full-name "June"
                                      ::person/role :cz.holyjak.rad.test-schema.person.role/admin
                                      ::person/primary-address (apply hash-map existing-addr-ident)
                                      ::person/addresses [(apply hash-map existing-addr-ident) {::address/id id4}]}))))
    (component "owned entity"
      (let [pid (ids/new-uuid 1)
            qid (ids/new-uuid 2)
            tpid (tempid/tempid pid)
            tqid (tempid/tempid qid)
            delta {[::person/id tpid] #::person{:id tpid
                                                :qualities {:after [[::person-quality/id tqid]]}}
                   [::person-quality/id tqid] #::person-quality{:id tqid
                                                                :name {:after "bravery"}}}
            _ (asami-pathom-common/save-form! *env* {::form/delta delta})
            q-nodeid (ffirst (d/q '[:find ?q
                                    :in $ ?qid
                                    :where [?q ::person-quality/id ?qid]]
                                  (d/db *conn*) qid))]

        (assertions
          "the owner has a/owns pointed to the owned entity"
          (d/q '[:find ?quality .
                 :in $ ?pid
                 :where
                 [?person ::person/id ?pid]
                 [?person :a/owns ?quality]] (d/db *conn*) pid) => q-nodeid
          "an owned entity does not have a/entity"
          (d/q '[:find [?name ?is-entity]
                 :in $ ?qid
                 :where
                 [?quality ::person-quality/id ?qid]
                 [?quality ::person-quality/name ?name]
                 (optional [?quality :a/entity ?is-entity])]
               (d/db *conn*) qid) => ["bravery" nil]
          "An owned entity is created so that d/entity will include it in its parent"
          (d/entity (d/db *conn*) [::person/id pid]) =check=> (_/embeds?*
                                                                {::person/id pid
                                                                 ::person/qualities
                                                                 ;; FIXME I get 1 even though it is to-many; bug?!
                                                                 {:id [::person-quality/id qid]
                                                                  ::person-quality/id qid
                                                                  ::person-quality/name "bravery"}})
          "TMP: Low-level check of owned entity retraction"
          (set (write/orphaned-owned-entities-retraction-txn
                 (d/db *conn*) key->attribute
                 {[::person/id pid] #::person{:id pid
                                              :qualities {:before [[::person-quality/id qid]] :after nil}}}))
          => #{[:db/retract q-nodeid :id [::person-quality/id qid]]
               [:db/retract q-nodeid ::person-quality/id qid]
               [:db/retract q-nodeid ::person-quality/name "bravery"]}

          "owned entity is deleted when dropped from the owning attribute"
          (do
            (asami-pathom-common/save-form!
              *env*
              {::form/delta {[::person/id pid] #::person{:id pid
                                                         :qualities {:before [[::person-quality/id qid]] :after nil}}}})
            (d/q '[:find ?quality .
                   :in $ ?qid
                   :where [?quality ::person-quality/id ?qid]]
                 (d/db *conn*) qid)) => nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEMPID remapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(specification "save-form! tempid remapping"
               (let [tempid1 (tempid/tempid (ids/new-uuid 101)) ; NOTE These IDs must be unique in DB across tests, it seems
                     id2   (ids/new-uuid 102)
                     tempid2 (tempid/tempid id2)
                     delta   {[::person/id tempid1]  {::person/id              tempid1
                                                      ::person/primary-address {:after [::address/id tempid2]}}
                              [::address/id tempid2] {::address/street {:after "A1 St"}}}
                     {:keys [tempids]} (asami-pathom-common/save-form! *env* {::form/delta delta})]
                 (assertions
                   "Returns tempid remappings"
                   tempids =fn=> not-empty
                   ;"Returns a native ID remap for entity using native IDs"
                   ;(pos-int? (get tempids tempid1)) => true
                   "Returns a non-native ID remap for entity using uuid IDs"
                   (uuid? (get tempids tempid2)) => true
                   "Tempid mapped correctly"
                   (get tempids tempid2) => id2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delete!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(specification "delete!"
  (let [addr-id (ids/new-uuid 101)
        addr-ident [::address/id addr-id]
        {db0 :db-after}
        @(d/transact *conn* (conj (write/new-entity-ident->tx-data addr-ident)
                                    [:db/add [:id addr-ident] ::address/street "X St"]))
        _ (asami-pathom-common/delete-entity! *env* {::address/id addr-id})
        db (d/db *conn*)]
    (assertions
      "Entity existed"
      (query/entities
        {::attr/key->attribute key->attribute, ::asami/id-attribute {::attr/qualified-key ::address/id}}
        {::address/id addr-id}
        db0) => #::address{:id addr-id :street "X St"} ; Note `:id <ident>` b/c query/entities drops it
      "Entity exists no more"
      (query/entities
        {::attr/key->attribute key->attribute, ::asami/id-attribute {::attr/qualified-key ::address/id}}
        {::address/id addr-id}
        db) => nil
      "The address exists no more"
      (d/q '[:find ?e :where [?e ::address/street "X St"]] db) => '()
      "The PK exists no more"
      (d/q '[:find ?e :where [?e ::address/id addr-id]] db) => '()
      "The :id exists no more"
      (d/q [:find '?e :where ['?e :id [::address/id addr-id]]] db) => '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Round-trip tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(specification "Pathom parser integration (save + generated resolvers)"
               (let [_save-middleware     (asami/wrap-save)
                     _delete-middleware   (asami/wrap-delete)
                     automatic-resolvers (asami/generate-resolvers all-attributes :production)
                     parser (universal-parser all-attributes [automatic-resolvers form/resolvers])]
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
                                  delta {[::address/id temp-address-id] {::address/id {:after temp-address-id}
                                                                         ::address/street {:after "A St"}}}
                                  {::form/syms [save-form]} (parser {} `[{(form/save-form ~{::form/id temp-address-id
                                                                                            ::form/master-pk ::address/id
                                                                                            ::form/delta delta}) [:tempids ::address/id ::address/street]}])
                                  {:keys [tempids]} save-form
                                  real-id (get tempids temp-address-id)
                                  entity (dissoc save-form :tempids)]
                              (assertions
                                "Includes the remapped (UUID) ID for id attribute"
                                (uuid? real-id) => true
                                "Returns the newly-created attributes"
                                entity => {::address/id real-id
                                           ::address/street "A St"})

                              (component "Deleting a saved item"
                                         (assertions
                                           "The deletion succeeds"
                                           (parser {} `[{(form/delete-entity ~{::address/id real-id}) []}])
                                           => {`form/delete-entity {}}
                                           "The entity is deleted" ; i.e. Pathom returns just the ident as-is
                                           (parser {} [{[::address/id real-id] [::address/id ::address/street]}])
                                           => {[::address/id real-id] {::address/id real-id}}))))
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
  (let [_parser (universal-parser all-attributes [(asami/generate-resolvers all-attributes :production)])]
    ;(_parser {} #_asami-pathom/*env [{[::person/id (:id *pid)] [{::person/primary-address [::address/id ::address/street]}]}])
    ;(parser {} #_asami-pathom/*env [{[::person/id (:id *pid)] [::person/primary-address]}])
    (_parser {} [[:no-such-thing :ident]])
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

(comment
  (do
    (require 'fulcro-spec.reporters.repl)
    (fulcro-spec.reporters.repl/run-tests)))