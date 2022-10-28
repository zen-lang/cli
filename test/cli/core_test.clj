(ns cli.core-test
  (:require [zen.cli :as sut]
            [zen.core :as z]
            [clojure.string :as str]
            [clojure.edn]
            [clojure.test :as t]
            [zen.test-utils]
            [matcho.core :as matcho]))


(t/deftest cli-usecases-test
  (def test-dir-path "/tmp/zen-cli-test")
  (def my-package-dir-path (str test-dir-path "/my-package/"))
  (def dependency-dir-path (str test-dir-path "/my-dep/"))
  (def dependency-fork-dir-path (str test-dir-path "/my-dep-fork/"))

  (zen.test-utils/rm-fixtures test-dir-path)
  (zen.test-utils/mk-fixtures test-dir-path zen-packages-fixtures)

  (t/testing "wrong command"
    (matcho/match (sut-cmd "AAAAAAAAAAAAAA" {:pwd test-dir-path})
                  {:status :error})

    (matcho/match (sut-cmd "init" "a" "b" "c" "d" "too many args" {:pwd test-dir-path})
                  {:status :error}))

  (t/testing "create template"
    (zen.test-utils/mkdir my-package-dir-path)

    (matcho/match (sut-cmd "init" "my-package" {:pwd my-package-dir-path})
                  {:code :initted-new, :status :ok})

    (matcho/match (zen.test-utils/fs-tree->tree-map my-package-dir-path)
                  {"zen-package.edn" some?
                   "zrc"             {"my-package.edn" some?}
                   ".git"            {}
                   ".gitignore"      some?}))


  (t/testing "try to create new template over existing directory, get error that repo already exists"
    (matcho/match (sut-cmd "init" "my-package" {:pwd my-package-dir-path})
                  {:status :ok, :code :already-exists}))


  (t/testing "check new template no errors"
    (matcho/match (sut-cmd "errors" {:pwd my-package-dir-path})
                  empty?))


  (zen.test-utils/git-init-commit my-package-dir-path)


  (t/testing "declare a symbol with tag and import ns from a dependency"
    (t/testing "no changes are made yet"
      (matcho/match (sut-cmd "changes" {:pwd my-package-dir-path})
                    {:status :unchanged}))

    (t/testing "the symbol doesn't exist before update"
      (t/is (nil? (sut-cmd "get-symbol" "my-package/sym" {:pwd my-package-dir-path})))

      (t/is (empty? (sut-cmd "get-tag" "my-dep/tag" {:pwd my-package-dir-path}))))

    (zen.test-utils/update-zen-file (str my-package-dir-path "/zrc/my-package.edn")
                     #(assoc %
                             :import #{'my-dep}
                             'sym {:zen/tags #{'my-dep/tag}
                                   :a "a"}))

    (t/testing "get the symbol"
      (matcho/match (sut-cmd "get-symbol" "my-package/sym" {:pwd my-package-dir-path})
                    {:zen/tags #{'my-dep/tag}
                     :a "a"}))

    (t/testing "get the symbol by the tag"
      (matcho/match (sut-cmd "get-tag" "my-dep/tag" {:pwd my-package-dir-path})
                    #{'my-package/sym}))

    (t/testing "see changes"
      (matcho/match (sut-cmd "changes" {:pwd my-package-dir-path})
                    {:status :changed
                     :changes [{} nil]})

      (zen.test-utils/git-commit my-package-dir-path "zrc/" "Add my-dep/new-sym")

      (matcho/match (sut-cmd "changes" {:pwd my-package-dir-path})
                    {:status :unchanged})))


  (t/testing "specify a dependency in zen-package.edn"
    (t/testing "check errors, see that namespace the dependency ns is missing"

      (matcho/match (sut-cmd "errors" {:pwd my-package-dir-path})
                    [{:missing-ns 'my-dep}
                     {:unresolved-symbol 'my-dep/tag}
                     nil]))

    (t/testing "can safely pull deps without deps specified"
      (matcho/match (sut-cmd "pull-deps" {:pwd my-package-dir-path})
                    {:status :ok, :code :nothing-to-pull, :deps empty?}))

    (zen.test-utils/update-zen-file (str my-package-dir-path "/zen-package.edn")
                                    #(assoc % :deps {'my-dep dependency-dir-path}))

    (t/testing "do pull-deps & check for errors, should be no errors"
      (matcho/match (sut-cmd "pull-deps" {:pwd my-package-dir-path})
                    {:status :ok, :code :pulled, :deps #{'my-dep}})

      (matcho/match (sut-cmd "errors" {:pwd my-package-dir-path})
                    empty?))

    (t/testing "do pull-deps again should be no errors and no changes"

      (matcho/match (sut-cmd "pull-deps" {:pwd my-package-dir-path})
                    {:status :ok})

      (matcho/match (sut-cmd "errors" {:pwd my-package-dir-path})
                    empty?))


    (t/testing "change repo url, pull"
      (zen.test-utils/update-zen-file (str my-package-dir-path "/zen-package.edn")
                                      #(assoc % :deps {'my-dep dependency-fork-dir-path}))

      (matcho/match (sut-cmd "pull-deps" {:pwd my-package-dir-path})
                    {:status :ok, :code :pulled, :deps #{'my-dep}})

      (matcho/match (sut-cmd "errors" {:pwd my-package-dir-path})
                    empty?)

      (matcho/match (sut-cmd "get-symbol" "my-dep/new-sym" {:pwd my-package-dir-path})
                    {:i-am-forked :not-the-original-repo})))


  (t/testing "commit update to the dependency"
    (zen.test-utils/update-zen-file (str dependency-fork-dir-path "/zrc/my-dep.edn")
                                    #(assoc-in % ['new-sym :i-am-forked] :fork-updated))

    (zen.test-utils/git-commit dependency-fork-dir-path "zrc/" "Update my-dep/new-sym")

    (t/testing "do pull-deps and see the update"
      (matcho/match (sut-cmd "pull-deps" {:pwd my-package-dir-path})
                    {:status :ok, :code :pulled, :deps #{'my-dep}})

      (matcho/match (sut-cmd "errors" {:pwd my-package-dir-path})
                    empty?)

      (matcho/match (sut-cmd "get-symbol" "my-dep/new-sym" {:pwd my-package-dir-path})
                    {:i-am-forked :fork-updated})))


  (t/testing "use validate command to validate some data"
    (matcho/match (sut-cmd "validate" "#{my-dep/tag}" "{}" {:pwd my-package-dir-path})
                  {:errors [{} nil]})))
