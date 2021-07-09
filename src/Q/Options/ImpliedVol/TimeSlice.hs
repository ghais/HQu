{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes#-}
{-# LANGUAGE FunctionalDependencies #-}
module Q.Options.ImpliedVol.TimeSlice
  (
      module Q.Types
    , module Q.Options.ImpliedVol
    , TimeSlice(..)
  )
where

import Q.Types
import Q.Options.ImpliedVol
import Q.Options.Black76

class TimeSlice v k where
  totalVar :: v -> k -> TotalVar

instance TimeSlice (k -> TotalVar) k where
  totalVar f = f

instance TimeSlice Black76 k where
  totalVar Black76{..} _ = TotalVar $ vol * vol * t
    where (Vol vol) = b76Vol
          (YearFrac t) = b76T

