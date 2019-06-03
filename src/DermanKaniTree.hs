{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
module DermanKaniTree
    (
    ) where

import Data.Foldable
import Data.Monoid (Sum(..))



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




