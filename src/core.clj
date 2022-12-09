(ns core
  (:require [zen.core]
            [clojure.pprint]
            [clojure.string]
            [clojure.edn]
            [zen.cli-tools :as ct]
            [clojure.stacktrace]
            [clojure.java.shell :as shell])
  (:gen-class))

(defmethod ct/command 'my-cli/identity [_ {:keys [value]}]
  value)


(defmethod ct/command 'my-cli/+ [_ args]
  (apply + args))


(defmethod ct/command 'my-cli/throw-exception [_ args]
  (throw (Exception. "some exception during command evaluation")))


(defn -main [& args]
  (let [ztx (zen.core/new-context)]
    (zen.core/load-ns
      ztx
      '{:ns my-cli
        :import #{zen.cli-tools}

        identity
        {:zen/tags #{zen.cli-tools/command}
         :zen/desc "returns its arg"
         :args-style :named
         :args {:type zen/map
                :require #{:value}
                :keys {:value {:type zen/string
                               :zen/desc "value that will be returned by this fn"}}}}

        +
        {:zen/tags #{zen.cli-tools/command}
         :zen/desc "calculates sum of passed arguments"
         :args-style :positional
         :args {:type zen/vector
                :zen/desc "numbers that will be summed together"
                :every {:type zen/number}}}

        no-implementation
        {:zen/tags #{zen.cli-tools/command}
         :zen/desc "no implementation should be defined. Needed for implementation missing error handling"
         :args {:type zen/vector
                :maxItems 0}}

        throw-exception
        {:zen/tags #{zen.cli-tools/command}
         :zen/desc "Throws an exception. Needed for testing exception handling"
         :args {:type zen/vector
                :maxItems 0}}

        my-config
        {:zen/tags #{zen.cli-tools/config}
         :commands {:identity  {:command identity}
                    :+         {:command +}
                    :undefined {:command undefined}
                    :no-impl   {:command no-implementation}
                    :fail      {:command throw-exception}}}})
    (ct/cli-main ztx 'my-cli/my-config args)))


(comment
  (let [sh-fn shell/sh]
    (clojure.string/trim-newline (:out (sh-fn "pwd"))))

  )
