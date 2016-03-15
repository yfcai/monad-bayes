import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified TestWeighted
import qualified TestDist
import qualified TestEmpirical
import qualified TestParticle
import qualified TestTrace
import qualified TestInference
import qualified TestSMCObservations

import qualified Dice
import qualified Gamma
import qualified BetaBin
import qualified HMM
import qualified DPmixture

main :: IO ()
main = hspec $ do
  describe "Weighted" $ do
    it "accumulates likelihood correctly" $ do
      TestWeighted.passed `shouldBe` True
  describe "Dist" $ do
    it "normalizes categorical" $ do
      TestDist.passed1 `shouldBe` True
    it "sorts samples and aggregates weights" $ do
      TestDist.passed2 `shouldBe` True
    it "gives correct answer for the sprinkler model" $ do
      TestDist.passed3 `shouldBe` True
  describe "Empirical" $ do
    context "controlling population" $ do
      it "preserves the population when not expicitly altered" $ do
        TestEmpirical.pop_size `shouldBe` 5
      it "multiplies the number of samples when spawn invoked twice" $ do
        TestEmpirical.many_size `shouldBe` 15
    context "checking properties of samples" $ do
      it "correctly checks if all particles satisfy a property" $ do
        TestEmpirical.all_check `shouldBe` True
    context "distribution-preserving transformations" $ do
      it "transform preserves the distribution" $ do
        TestEmpirical.trans_check1 `shouldBe` True
        TestEmpirical.trans_check2 `shouldBe` True
      it "resample preserves the distribution" $ do
        TestEmpirical.resample_check 1 `shouldBe` True
        TestEmpirical.resample_check 2 `shouldBe` True
  describe "Particle" $ do
    it "stops at every factor" $ do
      TestParticle.check_two_sync 0 `shouldBe` True
      TestParticle.check_two_sync 1 `shouldBe` True
      TestParticle.check_two_sync 2 `shouldBe` True
    it "preserves the distribution" $ do
      TestParticle.check_preserve `shouldBe` True
    it "produces correct intermediate weights" $ do
      TestParticle.check_sync 0 `shouldBe` True
      TestParticle.check_sync 1 `shouldBe` True
      TestParticle.check_sync 2 `shouldBe` True
  describe "Trace" $ do
    context "RandomDB = [Cache]" $ do
      it "correctly records values" $ do
        TestTrace.check_writing `shouldBe` True
      it "correctly reuses values" $ do
        TestTrace.check_reading `shouldBe` True
  describe "SMC" $ do
    it "terminates" $ do
      seq TestInference.check_terminate_smc () `shouldBe` ()
    it "preserves the distribution on the sprinkler model" $ do
      TestInference.check_preserve_smc `shouldBe` True
    prop "number of samples is equal to the number of particles" $
      \observations particles ->
        observations >= 0 && particles >= 1 ==>
          TestInference.check_particles observations particles == particles
  describe "MH" $ do
    it "MH from prior leaves posterior invariant" $ do
      TestInference.check_prior_trans `shouldBe` True
    it "Trace MH leaves posterior invariant" $ do
      TestInference.check_trace_trans `shouldBe` True
    -- too large to execute
    -- it "PIMH leaves posterior invariant" $ do
    --   TestInference.check_pimh_trans `shouldBe` True
  describe "Number of observations for models" $ do
    it "4 observations for Gamma.model" $ do
      TestSMCObservations.check_smc_weight 5 20 Gamma.model `shouldBe` True
    it "0 observations for Gamma.exact" $ do
      TestSMCObservations.check_smc_weight 0 20 Gamma.exact `shouldBe` True
    it "0 observations for Dice.dice" $ do
      TestSMCObservations.check_smc_weight 0 20 (Dice.dice 4) `shouldBe` True
    it "1 observation for Dice.dice_soft" $ do
      TestSMCObservations.check_smc_weight 1 20 Dice.dice_soft `shouldBe` True
    it "1 observation for Dice.dice_hard" $ do
      TestSMCObservations.check_smc_weight 1 20 Dice.dice_hard `shouldBe` True
    it "0 observations for BetaBin.latent" $ do
      TestSMCObservations.check_smc_weight 0 20 (BetaBin.latent 5) `shouldBe` True
    it "0 observations for BetaBin.urn" $ do
      TestSMCObservations.check_smc_weight 0 20 (BetaBin.urn 5) `shouldBe` True
    it "15 observations for HMM.hmm" $ do
      TestSMCObservations.check_smc_weight 16 20 HMM.hmm `shouldBe` True
    it "10 observations for DPmixture.dpMem" $ do
      TestSMCObservations.check_smc_weight 10 20 DPmixture.dpMem `shouldBe` True
    it "10 observations for DPmixture.dpMem" $ do
      TestSMCObservations.check_smc_weight 10 20 DPmixture.dpMemClusters `shouldBe` True
