(ns cz.holyjak.rad.database-adapters.asami.core
  (:require
    [asami.core :as d]
    [asami.graph :as graph]
    ;[com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [taoensso.timbre :as log]))

(defn config->url [{:asami/keys [driver] :as config}]
  (case driver
    :mem (str "asami:mem://" (:asami/database config))
    :local (str "asami:local://" (:asami/database config))
    (throw (ex-info "Unsupported Asami driver." {:driver driver}))))

(defn start-connections
  "Return a map schema->connection based on the config.

  The `aso/databases` entry in the config is a map with the following form:

   ```clojure
   {:production ; the 'schema', i.e. any custom keyword
     {:asami/driver   :mem ; or :local for file-storage
      :asami/database \"my-db-name\"}
   }
   ```"
  ([config]
   ;; FIXME Create db if not exists, as datomic plugin does
   (reduce-kv
     (fn [m k v]
       (log/info "Starting database " k)
       (assoc m k (d/connect (config->url v))))
     {}
     (aso/databases config))))

(comment
  ;(set! *data-readers* graph/node-reader)
  (d/create-database (config->url {:asami/driver :local, :asami/database "playground3"}))
  @(def conn (start-connections {aso/databases {:main {:asami/driver :local, :asami/database "playground3"}}}))
  @(def conn (start-connections {aso/databases {:main {:asami/driver :mem, :asami/database "fulcro-rad-demo"}}}))
  (def dbm (:main conn))

  (d/delete-database "asami:mem://test123")
  (d/create-database "asami:mem://test123")
  (def conn (d/connect "asami:mem://test123"))
  (->
    @(d/transact conn {:tx-data [{:id [:person/id "person2"]}
                                 [:db/add [:id [:person/id "person2"]] :person/full-name "Bob"]]})
    :tx-data)
  ;; FIXME: The :db/add does NOT resolve `[:id ...]`. Relevant:
  ;; you try to reference it with:
  ;[:id [:cz.holyjak.rad.test-schema.address/id #uuid "ffffffff-ffff-ffff-ffff-000000000001"]]
  ;That tells the transactor to look up a node that has that as an id. But... there IS no node that has that as an ID.
  ; You're still creating it, so it doesn't exist yet. It won't exist until the end of the transaction

  (let [conn ...]
    @(d/transact conn {:tx-data [{:db/id -1 :id [:duplicate-test/id :SAME]}]})
    ; :tempids {-1 #a/n[24], [:duplicate-test/id :SAME] #a/n[24]}}
    @(d/transact conn {:tx-data [{:db/id -1 :id [:duplicate-test/id :SAME]}]})
    ;  :tempids {-1 #a/n[25], [:duplicate-test/id :SAME] #a/n[25]}}
    (d/q '[:find ?e :where [?e :id [:duplicate-test/id :SAME]]] conn))
  ; => ([#a/n[24]] [#a/n[25]])

  (d/entity dbm [:order/id 3])

  (d/q '[:find ?e ?v
         :where [?e :account/id ?v] [?e ?a ?v]]
       dbm)
  (d/q '[:find ?e ?a ?v :where [?e :id [:order/id 2]] [?e ?a ?v]] dbm)
  ;(d/q '[:find ?a ?v :where [#a/n [16] ?a ?v]] (d/db (:main conn)))
  ;; order 6, OL 15, 16
  ; OK: @(d/transact (:main conn) {:tx-data [[:db/add [:id [:order/id 2]] :order/exp-add-via-id true]]})
  (def tmp @(d/transact (:main conn) {:tx-data [[:db/add [:id [:order/id 2]] :order/extra-ol-ref [:id [:order-line/id 2]]]]}))
  (:tx-data tmp)
  (d/entity dbm [:order/id 2])

  ;; *** NOTES ***
  ;; FKs: {:db/ident <val>} - retrieved as ref (unless nested? true) via d/entity for top-level ones and as data for nested entities

  ;; insert
  ;; Q: use :db/ident or :db/id ??? What's the diff?
  (def tx1 (d/transact dbm {:tx-data [{:id [:test/id 2] :b false, :i 1 :f 1.0}]}))
  (:tempids @tx1)
  (require '[asami.graph :as graph])
  (def tx2 (d/transact dbm {:tx-data [{:id [:order/id 1]
                                       :order/descr "Order XYZ"
                                       :order/product {:id [:product/id 11]
                                                       :product/name "Bread"
                                                       :product/maker "Unitied Bakeries"}}]}))
  (def tx3 (d/transact dbm {:tx-data [{:id [:order/id 2]
                                       :order/descr "Order ABC"
                                       :order/product {:id [:product/id 11]
                                                       :product/name "Bread"
                                                       :product/maker "Unitied Bakeries"}
                                       :order/line [{:order-line/id [:order-line/id 1]
                                                     :order-line/descr "Line 1"}
                                                    {:order-line/id [:order-line/id 2]
                                                     :order-line/descr "Line 2"}]}]}))


  (d/q '[:find ?e ?a ?v
         :in $ ?e
         :where [?e ?a ?v]]
       dbm
       (graph/new-node 17) #_[:tst/id 1])

  (d/entity dbm (graph/new-node 21))

  (d/q '[:find ?e ?a ?v
         :in $ [?a ...]
         :where [?e ?a ?v]
         (or [?e :order/product]
             [_ :order/product ?e]
             [_ :order/line ?e])]
       dbm
       [:order/descr :order/product :product/name :order/line :id :order-line/descr])

  (d/q '[:find ?e ?a ?v
         :where [?e ?a ?v]
         ;; say that the output ?e is either of ?o, ?p - how to?
         [?o :order/product ?p]
         (or [?o :order/descr ?v]
             [?p :product/name ?v])]
       dbm
       [:order/descr :product/name])

  ;; retract
  (retract-entity dbm [:test/id 2])

  (d/entity (d/db dbm) (graph/new-node 2))
  (d/q '[:find ?e ?v . :where [?e :db/ident ?v]] dbm)

  (def tx
    (d/transact dbm {:tx-data [{:db/ident 3 :dog/name "Dogie" :dog/race :poodle}
                               {:db/ident 4 :dog/name "Doggie 2" :dog/race :poodle}]}))
  ;; -> :tempids is map from tmp :db/id to actual eid and any ident -> eid

  (let [[q attr-filter] [{:find '[?e ?a ?v], :in '[$ [?a ...]], :where '[[?e ?a ?v] (or [?e :order/product] [_ :order/product ?e])]}
                         [:order/descr :product/name :order/product]]]
    (d/q q dbm attr-filter))

  ;; Get entity
  ;; Q: Can I pull a subtree? Can I update w/o knowing eid, just with db/ident?
  ;; => A pseudo property called :db/id is used to represent the ID of an entity, i.e. === ?e; use
  ;;    inside a tx to refer to entities being created, eg `:db/id -1`
  ;; AND, ident:
  ;;    Entities can have a value that is associated with their identity via the :db/ident property.
  ;;    Unlike :db/id this is a real attribute that is placed on the entity, although it is hidden from d/entity.
  ;;    Can be of any data type.
  ;; AND, :id
  ;;    Just like ident but not hidden from d/entity.
  ;; All 3 can be used to identify an entity in a tx or to retrieve it.
  (def dog1-eid (get (:tempids @tx) 3))
  (d/entity (d/db dbm) dog1-eid)
  (d/entity (d/db dbm) 3)                                   ; works too; presumably id val must be uniq across all entities?!

  ;; select
  (d/q '[:find ?name :where [?e :dog/id] [?e :dog/name ?name]] dbm)
  ;; see also (d/entity)

  ;; update
  (def e (d/q '[:find ?e . :where [?e :dog/id 1]] dbm))
  @(d/transact dbm {:tx-data [{:db/id e, :dog/name' "Doggie 1"}]})
  ;; Also supported - explicit :db/add, :db/retract; ex.: [:db/retract node-1 edge node-2]

  )