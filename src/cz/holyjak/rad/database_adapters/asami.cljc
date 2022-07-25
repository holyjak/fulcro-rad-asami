(ns cz.holyjak.rad.database-adapters.asami
  "Entry point into the RAD Asami adapter, collecting all the important functions at a single place."
  (:require
    [cz.holyjak.rad.database-adapters.asami-options :as aso]
    [cz.holyjak.rad.database-adapters.asami.connect :as connect]
    [cz.holyjak.rad.database-adapters.asami.pathom :as asami.pathom]
    [asami.core :as d]))

(def start-connections connect/start-connections)

(def pathom-plugin asami.pathom/pathom-plugin)

(defn mock-resolver-env
  "Returns a mock env that has the do/connections and do/databases keys that would be present in
  a properly-set-up pathom resolver `env` for a given single schema. This should be called *after*
  you have seeded data against a `connection` that goes with the given schema.

  * `schema` - A schema name
  * `connection` - A database connection that is connected to a database with that schema."
  [schema connection]
  {aso/connections {schema connection}
   aso/databases {schema (atom (d/db connection))}})

(def generate-resolvers asami.pathom/generate-resolvers)

(def wrap-save asami.pathom/wrap-save)

(def wrap-delete asami.pathom/wrap-delete)
