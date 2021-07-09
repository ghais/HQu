module Q.Options.ImpliedVol.TimeInterpolation where

import Q.Options.ImpliedVol.TimeSlice
data TimeInterpolation = LinearInVol | LinearInTotalVar | Gatheral
data TimeExtrapolation = TerminalMoneyness


