(ns cz.holyjak.rad.test-schema.person-quality
  (:require
    [com.fulcrologic.rad.attributes :as attr :refer [defattr]]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [cz.holyjak.rad.database-adapters.asami :as-alias asami]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [taoensso.timbre :as log]))

(defattr id ::id :uuid ;:long
  {::attr/identity? true
   ::attr/schema :production
   ::asami/owned-entity? true})

(defattr name ::name :string
  {::attr/schema     :production
   ::attr/identities #{::id}
   ::attr/required?  true})

(def attributes [id name])