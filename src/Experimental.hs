{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
module Experimental
    (
    ) where

import qualified GDP

import Data.Foldable
import Data.Monoid (Sum(..))


newtype DfTransFor t1 t2 name = DfTransFor GDP.Defn
type Df t1 t2 = (Double GDP.?DfTransFor t1 t2)

newtype PvTo t name = PvTo GDP.Defn
type Cf t = (Double GDP.?PvTo t)

newtype SomeDfTo t2 = SomeDfTo { unSomeDfTo :: forall t1. Df t1 t2 }
newtype SomeDfFrom t1 = SomeDfFrom { unSomeDfFrom :: forall t2. Df t1 t2}

newtype SomeCf = SomeCf { unSomeCf :: forall t. Cf t }

newtype SomeCfWithDfTo t2 =
  SomeCfWithDfTo { unSomeCfWithDfTo :: forall t1. (Cf t1, Df t1 t2)}


newtype SomeTPair f g = SomeTPair { unEPair :: forall s. (f s, g s) }
type BigFlip f a b = f b a

--TODO new file with all newtye
--test :: EPair t Cf (Df t0) -> Cf t0
--test = _



--TODO how to write generically
--TODO almost certainly pieces exist, need to understand Satisfies
liftPvToByAssert :: (a -> b) -> (a GDP.?PvTo t) -> (b GDP.?PvTo t)
liftPvToByAssert f (GDP.The x) = GDP.assert $ f x


liftPvToByAssert2 :: (a -> b -> c)
  -> (a GDP.?PvTo t)
  -> (b GDP.?PvTo t)
  -> (c GDP.?PvTo t)
liftPvToByAssert2 f (GDP.The x) (GDP.The y)
  = GDP.assert $ f x y



instance (Num a) => Num ((a GDP.?PvTo t)) where
  (+) = liftPvToByAssert2 (+)
  (-) = liftPvToByAssert2 (-)
  (*) = liftPvToByAssert2 (*)
  negate = liftPvToByAssert negate
  abs = liftPvToByAssert abs
  signum = liftPvToByAssert signum
  fromInteger = GDP.assert . fromInteger

--TODO theorems about discounting
--TODO injectivity, if PvTo t1 and PvTo t2 hold, they must be equal (and converse)


--TODO could make vector space for discount and value
--make type specifically for discount rather than Double
--make type specifically for value rather than value
--could use coerce in multiplying

--Dont need Double GDP.~~ t
changePvTo :: Df t0 t -> Cf t0 -> Cf t
changePvTo (GDP.The df) (GDP.The amt) = GDP.assert $ df * amt

getDf :: (Double -> Double -> Double)
  -> (Double GDP.~~ t1)
  -> (Double GDP.~~ t2)
  -> Df t1 t2
getDf dff (GDP.The time1) (GDP.The time2) = GDP.assert $ dff time1 time2


changeFlowPvTo :: [SomeCfWithDfTo t] -> [Cf t]
changeFlowPvTo = map $ ((uncurry . flip $ changePvTo) . unSomeCfWithDfTo)


sumCfs :: [Cf t] -> Cf t
sumCfs = getSum . foldMap Sum


--TODO flaw, only dividends between the times matter!
forwardFromFlow :: (Cf t0, Df t0 t) -> [SomeCfWithDfTo t] -> Cf t
forwardFromFlow spot divFlow = sumCfs
  $ (changePvTo' spot) : map negate (changeFlowPvTo divFlow)
  where changePvTo' = uncurry . flip $ changePvTo


--TODO now need vesions tupling CF with time, etc



data CashFlow tm = CashFlow { amount :: Double, time :: tm }


pv :: Num tm => (tm -> tm -> Double) -> tm -> CashFlow tm -> Double
pv forwardRate presentTime (CashFlow amt cfTime) = discount * amt
  where discount = forwardRate presentTime cfTime

--TODO shoud cashflows implement discountable typeclass? more structured
--should forwardPrice take already discounted flows and sum? less structured

--TODO could be generically as a swap of
--Pay S now, get dividends at each time, with cost of carry and interest
forwardPrice :: (Num tm, Foldable t)
  => (tm -> tm -> Double) -> tm -> Double -> t (CashFlow tm) -> Double
forwardPrice forwardRate deliveryTime spotPrice dividends =
  spotDeliveryValue - dividendsDeliveryValue
  where
    spotDeliveryValue =
      deliveryTimeValue CashFlow { amount = spotPrice, time = 0}
    dividendsDeliveryValue =
      getSum $ foldMap (Sum . deliveryTimeValue) dividends
    deliveryTimeValue = pv forwardRate deliveryTime



