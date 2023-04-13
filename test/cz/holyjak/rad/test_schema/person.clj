(ns cz.holyjak.rad.test-schema.person
  (:require
    [com.fulcrologic.rad.attributes :as attr :refer [defattr]]
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [taoensso.timbre :as log]))

(defattr id ::id :uuid ;:long
  {::attr/identity?                                         true
   ;aso/native-id?    true
   ::attr/schema                                            :production
   ;; Hopefully a future version of Pathom will behave better
   ;ao/pathom3-resolve (fn [resolver]
   ;                     (assoc resolver ::transform-succeeded true))
   ;ao/pc-resolve      (fn [resolver]
   ;                     (assoc resolver ::transform-succeeded true))
   })

(defattr full-name ::full-name :string
  {::attr/schema     :production
   ::attr/identities #{::id}
   ::attr/required?  true})

(defattr role ::role :enum
  {::attr/schema            :production
   ::attr/identities        #{::id}
   ::attr/enumerated-values #{:user :admin}
   ::attr/cardinality       :one})

(defattr permissions ::permissions :enum
  {::attr/schema            :production
   ::attr/identities        #{::id}
   ::attr/enumerated-values #{:read :write :execute}
   ::attr/cardinality       :many})

(defattr email ::email :string
  {::attr/schema     :production
   ::attr/identities #{::id}
   ::attr/required?  true})

(defattr account-balance ::account-balance :long            ; added by JH
  {::attr/schema     :production
   ::attr/identities #{::id}})

(defattr primary-address ::primary-address :ref
  {::attr/target     :cz.holyjak.rad.test-schema.address/id
   ::attr/schema     :production
   ::attr/identities #{::id}})

(defattr addresses ::addresses :ref
  {::attr/target      :cz.holyjak.rad.test-schema.address/id
   ::attr/cardinality :many
   ::attr/schema      :production
   ::attr/identities  #{::id}})

(defattr qualities ::qualities :ref
  {::attr/target      :cz.holyjak.rad.test-schema.person-quality/id
   ;::asami/ownership? true
   ::attr/cardinality :many
   ::attr/schema      :production
   ::attr/identities  #{::id}})

(defattr things ::things :ref
  {::attr/target      :cz.holyjak.rad.test-schema.thing/id
   ::attr/cardinality :many
   ::attr/schema      :production
   ::attr/identities  #{::id}})

(defattr nicks ::nicks :string
  {::attr/cardinality :many
   ::attr/schema      :production
   ::attr/identities  #{::id}})

(def attributes [id full-name email nicks primary-address addresses qualities role permissions things account-balance])

(comment
  (defn entries->map
    "Merges a sequence of `[k v]` pairs, merging multiple values for the same key with `into`.
    Prerequisite: Values in to-many attributes are wrapped in extra [] so that merging works."
    [entries]
    (->> entries
         (map #(apply hash-map %))
         (apply (partial merge-with into))))

  (require '[cz.holyjak.rad.database-adapters.asami.write :as w] '[com.fulcrologic.fulcro.algorithms.tempid :as tempid])
  (let [env {::attr/key->attribute (-> (group-by ::attr/qualified-key attributes) (update-vals first))}]
    (->
      (->> {::full-name "Bob"
            ::permissions [:read :write]
            ::things [[:thing/id :existing1]
                      [:thing/id (tempid/tempid)]]}         ; FIXME the tempid is not moved out to the `true` part !!!
           (mapcat (fn [[k vs :as entry]] (if (w/to-one? env k) [entry] (map #(vector k [%]) vs))))
           (group-by (fn new-entity-ref? [[prop maybe-ident]]
                       (boolean (and (w/ref? env prop) (tempid/tempid? (second maybe-ident)))))))
      (update-vals entries->map)))

  )