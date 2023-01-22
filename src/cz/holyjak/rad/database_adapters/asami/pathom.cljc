(ns cz.holyjak.rad.database-adapters.asami.pathom
  "Pathom integration for Asami for Pathom v2"
  (:require
    [com.wsscode.pathom.core :as p]
    [cz.holyjak.rad.database-adapters.asami :as-alias asami]
    [cz.holyjak.rad.database-adapters.asami.duplicates :as dups]
    [cz.holyjak.rad.database-adapters.asami.pathom-common :as apc]
    [cz.holyjak.rad.database-adapters.asami.util :as util]))

(def env->asami util/env->asami)
(def delete-entity! apc/delete-entity!)

(defn make-pathom2-resolver
  "Creates a pathom2 resolver, skipping the macro"
  [resolve-sym qualified-key outputs resolve-fn transform-fn]
  (let [with-resolve-sym (fn [r]
                           (fn [env input]
                             (r (assoc env :com.wsscode.pathom.connect/sym resolve-sym) input)))]
    (cond-> {:com.wsscode.pathom.connect/sym     resolve-sym
             :com.wsscode.pathom.connect/output  outputs
             :com.wsscode.pathom.connect/batch?  true
             :com.wsscode.pathom.connect/resolve (with-resolve-sym resolve-fn)
             :com.wsscode.pathom.connect/input   #{qualified-key}}
            transform-fn transform-fn)))

(defn generate-resolvers [attributes schema]
  (dups/generate-resolvers (partial apc/id-resolver* make-pathom2-resolver) attributes schema))

(defn pathom-plugin
  "Instantiate a pathom plugin for Asami connections and DB.
  - `database-mapper` - a `(fn [env]) -> map from a schema to an Asami connection

  NOTE: If you get the error 'form/pathom-plugin is not installed on the parser' then
        you forgot to install the ...rad.form/pathom-plugin with the save/delete middlewares."
  [database-mapper]
  (p/env-wrap-plugin (apc/wrap-env nil database-mapper)))
