(ns cz.holyjak.rad.test-schema.person-quality
  (:refer-clojure :exclude [name])
  (:require
    [com.fulcrologic.rad.attributes :as attr :refer [defattr]]
    [cz.holyjak.rad.database-adapters.asami :as-alias asami]))

(defattr id ::id :uuid
  {::attr/identity? true
   ::attr/schema :production
   ::asami/owned-entity? true})

(defattr name ::name :string
  {::attr/schema     :production
   ::attr/identities #{::id}
   ::attr/required?  true})

(def attributes [id name])