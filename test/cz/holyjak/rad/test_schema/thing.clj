(ns cz.holyjak.rad.test-schema.thing
  (:require
    [com.fulcrologic.rad.attributes :as attr :refer [defattr]]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]))

(defattr id ::id :long
  {::attr/identity? true
   ;aso/native-id?    true
   ::attr/schema    :production})

(defattr label ::label :string
  {::attr/schema     :production
   ::attr/identities #{::id}
   ::attr/required?  true})

(def attributes [id label])