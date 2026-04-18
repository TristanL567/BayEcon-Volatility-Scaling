# CAVEATS

- CAVEAT: Initial train/test split uses training data <= 2018-12-31 and test data >= 2019-01-01.
- CAVEAT: Expanding-window FSV refits use only data available up to each rebalance date (no future leakage).
- DEVIATION: Part 2 uses one-step-ahead (t+1) FSV covariance forecasts at rebalance dates.
- DEVIATION: Part 1 uses a 22-day horizon-average FSV variance forecast for MM scaling.
- DEVIATION: Expected returns for multi-asset optimization use an expanding historical mean up to each rebalance date.
- DEVIATION: borrowing rate proxy uses SHY daily return + 30 bps annual spread, floored at zero.
- DEVIATION: AR(1) SV parameters are estimated by OLS on posterior-mean log-variance paths for tractability.
