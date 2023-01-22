(ns cz.holyjak.rad.database-adapters.asami
  "Entry point into the RAD Asami adapter, collecting all the important functions at a single place."
  (:require
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [cz.holyjak.rad.database-adapters.asami.connect :as connect]
    [cz.holyjak.rad.database-adapters.asami.pathom-common :as apc]
    [asami.core :as d]))

(def start-connections connect/start-connections)

(defn mock-resolver-env
  "Returns a mock env that has the do/connections and do/databases keys that would be present in
  a properly-set-up pathom resolver `env` for a given single schema. This should be called *after*
  you have seeded data against a `connection` that goes with the given schema.

  * `schema` - A schema name
  * `connection` - A database connection that is connected to a database with that schema."
  [schema connection]
  {aso/connections {schema connection}
   aso/databases {schema (atom (d/db connection))}})

(def wrap-delete apc/wrap-delete)
(def wrap-env apc/wrap-env)
(def wrap-save apc/wrap-save)

(defn- pathom-version []
  (if (try (do (require 'com.wsscode.pathom.connect) true) (catch #?(:clj Exception :cljs :default) _ false))
    2
    3))

#?(:clj
   (def generate-resolvers
     (case (pathom-version)
       2 (requiring-resolve 'cz.holyjak.rad.database-adapters.asami.pathom/generate-resolvers)
       3 (requiring-resolve 'cz.holyjak.rad.database-adapters.asami.pathom3/generate-resolvers)))
   :cljs (defn generate-resolvers []
           (throw (ex-info (str `generate-resolvers "is only available in Clojure; in cljs, require it directly from cz.holyjak.rad.database-adapters.asami.pathom[3]")
                           {}))))

#?(:clj
   (def ^:deprecated pathom-plugin
     "Use wrap-env instead for Pathom3 or cz.holyjak.rad.database-adapters.asami.pathom/pathom-plugin for v2"
     (case (pathom-version)
       2 (requiring-resolve 'cz.holyjak.rad.database-adapters.asami.pathom/pathom-plugin)
       3 (fn [& _] (throw (ex-info (format "Use %s instead; see docs (notice the different way of use!)" `wrap-env) {})))))
   :cljs (defn pathom-plugin []
           (throw (ex-info (str `pathom-plugin "is only available in Clojure; in cljs, use wrap-env / require it directly from cz.holyjak.rad.database-adapters.asami.pathom")
                           {}))))