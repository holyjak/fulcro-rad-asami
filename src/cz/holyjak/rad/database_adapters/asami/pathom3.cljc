(ns cz.holyjak.rad.database-adapters.asami.pathom3
  "Pathom integration for Asami for Pathom v3"
  (:require
    [asami.core :as d]
    [com.fulcrologic.rad.attributes :as attr]
    [com.wsscode.pathom3.connect.operation :as pco]
    [cz.holyjak.rad.database-adapters.asami :as-alias asami]
    [cz.holyjak.rad.database-adapters.asami.connect :as asami-core]
    [cz.holyjak.rad.database-adapters.asami.duplicates :as dups]
    [cz.holyjak.rad.database-adapters.asami.pathom-common :as apc]
    [cz.holyjak.rad.database-adapters.asami.read :as query]
    [cz.holyjak.rad.database-adapters.asami.util :as util]))

(defn make-pathom3-resolver
  "Creates a pathom3 resolver, skipping the macro"
  [{:keys [batch?] :as _opts} resolve-sym qualified-key outputs resolve-fn transform-fn]
  ; Note: rad-datomic uses macro lazy-invoke, because Pathom3 bottoms out on a defrecord
  ; called com.wsscode.pathom3.connect.operation/Resolver
  ; This requires invoking the resolver function.
  ; Doing a lazy-invoke prevents pathom3 from being a hard-dependency for other users of this
  (pco/resolver
    (merge
      {::pco/op-name resolve-sym
       ::pco/batch?  batch?
       ::pco/input   [qualified-key]
       ::pco/output  outputs
       ::pco/resolve resolve-fn}
      (when transform-fn
        {::pco/transform transform-fn}))))

(defn generate-resolvers [attributes schema]
  (dups/generate-resolvers (partial apc/id-resolver* make-pathom3-resolver) attributes schema))

(comment
  (def *env (cz.holyjak.rad.database-adapters.asami/mock-resolver-env :production (d/connect (asami-core/config->url {:asami/driver :local, :asami/database "playground3"}))))

  (query/entities
    (assoc *env ::asami/id-attribute {::attr/qualified-key :order/id})
    {:order/id 1}
    (util/env->asami *env))

  (apc/delete-entity! (assoc *env
                    ::attr/key->attribute
                    {:order/id {::attr/schema :production}})
                  {:order/id 1})
  ,)