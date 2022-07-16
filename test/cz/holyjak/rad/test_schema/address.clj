(ns cz.holyjak.rad.test-schema.address
  (:require
    [com.fulcrologic.rad.attributes :as attr :refer [defattr]]))

(defattr id ::id :uuid
  {::attr/identity? true
   ::attr/schema    :production})

(defattr enabled? ::enabled? :boolean
  {::attr/identities #{::id}
   ::attr/schema     :production})

(defattr street ::street :string
  {::attr/identities #{::id}
   ::attr/required?  true
   ::attr/schema     :production})

(defattr city ::city :ref
  {::attr/target ::city-id
   ::attr/identities #{::id}
   ::attr/required?  true
   ::attr/schema     :production})

(defattr city-id ::city-id :uuid
  {::attr/identity?  true
   ::attr/required?  true
   ::attr/schema     :production})

(defattr city-name ::city-name :string
  {::attr/identities #{::city-id}
   ::attr/required?  true
   ::attr/schema     :production})

(def attributes [id enabled? street city city-id city-name])