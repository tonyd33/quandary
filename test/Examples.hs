module Examples (examples) where

import BruteForce qualified
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Fixtures (listed, option, other, question, spaceOf)
import Quandary.Core.Compile (compile)
import Quandary.Core.Solver
import Quandary.Core.Types
import Test.Tasty
import Test.Tasty.HUnit

examples :: TestTree
examples =
  testGroup
    "examples"
    [ testCase "a root pick-one question starts open" $
        verdicts [how] [] @?= Just [(listed "sync/git", Possible), (listed "sync/lww", Possible), (other "sync/how", Possible)]
    , testCase "closing and defeating all but one forces it" $
        verdictOf [how] [Refuse (other "sync/how"), Refuse (listed "sync/lww")] (listed "sync/git") @?= Just Forced
    , testCase "refusing every answer of a live pick-one question is stale" $
        verdicts [how] [Refuse (other "sync/how"), Refuse (listed "sync/lww"), Refuse (listed "sync/git")] @?= Nothing
    , testCase "a pick-any question takes none" $
        verdicts [extras] [Refuse (other "sync/extras"), Refuse (listed "sync/push")]
          @?= Just [(listed "sync/push", Impossible), (other "sync/extras", Impossible)]
    , testCase "a question not live refuses every option" $
        verdictOf [how, merge] [Refuse (listed "sync/git")] (listed "merge/manual") @?= Just Impossible
    , testCase "a live question with no listed options takes other" $
        verdictOf [how, merge, empty] [Take (listed "sync/git")] (other "merge/empty") @?= Just Forced
    , testCase "a question raised by options together is not live with one" $
        verdictOf [how, extras, both] [Take (listed "sync/git"), Refuse (listed "sync/push")] (listed "merge/rebase")
          @?= Just Impossible
    , testCase "a question raised by options together is live with both" $
        verdictOf
          [how, extras, both]
          [Take (listed "sync/git"), Take (listed "sync/push"), Refuse (other "merge/both")]
          (listed "merge/rebase")
          @?= Just Forced
    , testCase "a live pick-some question takes at least one" $
        verdictOf [some] [Refuse (other "sync/channels"), Refuse (listed "sync/lan")] (listed "sync/cloud") @?= Just Forced
    , testCase "needs forces its target" $
        verdictOf [how, merge] [Take (listed "merge/auto")] (listed "sync/git") @?= Just Forced
    , testCase "why names the ruling that rules an option out" $
        bruteForceOn
          [how]
          ( \q ->
              core q (Set.fromList [Take (listed "sync/git"), Refuse (other "sync/how")]) (Set.singleton (Take (listed "sync/lww")))
          )
          @?= Just (Set.fromList [Take (listed "sync/git")])
    , testCase "why is empty when no design takes the option" $
        bruteForceOn [how, merge] (\q -> core q Set.empty (Set.singleton (Take (listed "merge/never"))))
          @?= Just Set.empty
    ]
  where
    how = question "sync/how" One [] [option "sync/git", option "sync/lww"]
    extras = question "sync/extras" Any [] [option "sync/push"]
    some = question "sync/channels" Some [] [option "sync/lan", option "sync/cloud"]
    merge =
      question
        "merge/how"
        One
        [listed "sync/git" :| []]
        [ option "merge/manual"
        , (option "merge/auto") {needs = [listed "sync/git"]}
        , (option "merge/never") {excludes = [listed "sync/git"]}
        ]
    empty = question "merge/empty" One [listed "sync/git" :| []] []
    both = question "merge/both" One [listed "sync/git" :| [listed "sync/push"]] [option "merge/rebase"]

verdicts :: [Question] -> [Literal] -> Maybe [(OptionId, Verdict)]
verdicts qs p = case bruteForceOn qs (\q -> consequences q (Set.fromList p)) of
  Stale -> Nothing
  Reaches vs -> Just (Map.toList vs)

bruteForceOn :: [Question] -> (Query OptionId Identity -> Identity a) -> a
bruteForceOn qs = BruteForce.bruteForceOn (compile (spaceOf qs))

verdictOf :: [Question] -> [Literal] -> OptionId -> Maybe Verdict
verdictOf qs p o = verdicts qs p >>= lookup o
