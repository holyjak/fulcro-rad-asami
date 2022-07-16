(ns cz.holyjak.rad.database-adapters.asami-options)

(def connections
  "If using the Asami pathom-plugin, the resulting patohm-env will contain
    a map from schema->connection at this key path."
  :cz.holyjak.rad.database-adapters.asami/connections)

(def databases
  "If using the Asami pathom-plugin, the resulting patohm-env will contain
   a map from schema->database (i.e. (d/db <connection>)) at this key path."
  :cz.holyjak.rad.database-adapters.asami/databases)