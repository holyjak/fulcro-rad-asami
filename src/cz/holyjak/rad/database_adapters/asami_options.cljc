(ns cz.holyjak.rad.database-adapters.asami-options)

(def connections
  "If using the Asami pathom-plugin, the resulting patohm-env will contain
    a map from schema->connection at this key path."
  :cz.holyjak.rad.database-adapters.asami/connections)

(def databases
  "If using the Asami pathom-plugin, the resulting patohm-env will contain
   a map from schema->database atom (i.e. (atom (d/db <connection>))) at this key path."
  :cz.holyjak.rad.database-adapters.asami/databases)

(def wrap-resolve
  "Identity Attribute option. A `(fn [resolve])` that must return a `(fn [env input])`. The `resolve` is the core
  resolving logic (a function of env/input), so the returned function can manipulate the resolver inputs and outputs.
  This only affects Asami autogenerated resolvers.
  Ex.: `aso/wrap-resolve (fn wrap-log [resolve] (fn [env input] (log/spy :info (resolve env input))))`"
  :cz.holyjak.rad.database-adapters.asami/wrap-resolve)

;(def fetch-nested?
;  "Attribute option. Defaults to false. Set true for the generated id-resolvers to fetch all referenced
;  entities, not just their IDs. You normally do not want that. See `asami.core/entity`"
;  :cz.holyjak.rad.database-adapters.asami/fetch-nested?)