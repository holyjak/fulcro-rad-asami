(ns cz.holyjak.rad.database-adapters.asami-basics-test
  (:require [clojure.test :refer :all]
            [asami.core :as d]))

(def ^:dynamic *conn* nil)

(def uri "asami:mem://asami-basics-test")

(defn with-reset-database [tests]
  (d/delete-database uri)
  (tests))

(defn with-conn [tests]
  (d/create-database uri)
  (let [conn (d/connect uri)]
    (binding [*conn* conn]
      (tests))))

(use-fixtures :once with-reset-database)
(use-fixtures :each with-conn)

(defn db []
  (d/db *conn*))

(deftest insert-new-entity-via-map
  @(d/transact *conn* {:tx-data [{:id [:x 1],
                                  :k1 :v1}]})
  (is (= {:id [:x 1],
          :k1 :v1}
         (d/entity (db) [:x 1]))))

; BROKEN: Using the lookup ref as entity ID does NOT work for a new entity
(deftest insert-new-entity-via-quadruplets
  @(d/transact *conn* {:tx-data [#_{:id :XXX #_[:x 1]}, ; NOTE: Tmp id lookup in same tx only works for :db/add, no {} form
                                 [:db/add [:id [:x 1]]:id [:x 1]]
                                 [:db/add [:id [:x 1]] :k1 :v1]]})
  (is (= {:id [:x 1] #_[:x 1],
          :k1 :v1}
         (d/entity (db) [:x 1] #_[:x 1]))))


(deftest existing-add-ref
  @(d/transact *conn* {:tx-data [{:id "existing1"}
                                 {:id "existing2"}]})
  @(d/transact *conn* {:tx-data [[:db/add [:id "existing1"] :ref [:id "existing2"]]]})
  (is (= {:id "existing1",
          :ref {:id "existing2"}}
         (d/entity (d/db *conn*) "existing1"))))

(deftest existing-add-ref-to-new
  @(d/transact *conn* {:tx-data [{:id "existing"}]}) ; we can use {} here since we only ref to it in another tx
  @(d/transact *conn* {:tx-data [[:db/add [:id "new"] :id "new"]
                                 [:db/add [:id "existing"] :ref [:id "new"]]]})
  ; BROKEN: Resolving the lookup ref does not work, it is stored as-is - likely b/c the entity cannot be looked up
  ;         since we are only just creating it
  (is (= {:id "existing" :ref {:id "new"}}
         (d/entity (d/db *conn*) "existing"))))

