# Bootstrap Financial Calculations - Mathematical Explanation

This document explains the step-by-step mathematical calculations used to estimate the monetary impact of dock expansion at BlueBikes stations.

## Financial Parameters

The analysis uses these fixed parameters:

- **Revenue per trip**: $2.95 USD per ride
- **Capital expenditure per dock**: $500 USD per dock
- **Capital expenditure annualization period**: 10 years

## Step-by-Step Calculation Process

### Step 1: Calculate Average Daily Trips per Station

For each station, we calculate the average number of trips per day:

```
avg_daily_trips = total_checkouts / n_days
```

Where:
- `total_checkouts` = Sum of all rides starting from this station (weekdays, non-winter)
- `n_days` = Number of distinct days in the dataset for this station

**Example**: If a station has 1,000 total checkouts over 200 days:
- `avg_daily_trips = 1,000 / 200 = 5 trips/day`

---

### Step 2: Estimate Current Dock Capacity

We estimate the current number of docks at each station:

```
estimated_docks = max(10, min(50, round(avg_daily_trips × 5)))
```

This formula:
- Multiplies average daily trips by 5 (assumption: ~5 trips per dock per day)
- Caps the result between 10 and 50 docks (reasonable range for bike share stations)
- Rounds to nearest integer

**Example**: If `avg_daily_trips = 5`:
- `estimated_docks = max(10, min(50, round(5 × 5))) = max(10, min(50, 25)) = 25 docks`

---

### Step 3: Calculate Proposed Dock Expansion

We propose adding 50% more docks:

```
added_docks = round(estimated_docks × 0.5)
new_capacity = estimated_docks + added_docks
```

**Example**: If `estimated_docks = 25`:
- `added_docks = round(25 × 0.5) = 13 docks`
- `new_capacity = 25 + 13 = 38 docks`

---

### Step 4: Estimate Recovered Trips

This is where we estimate how many additional trips we can capture by expanding capacity. The calculation assumes:

- **Imbalance reduction factor**: 0.9 (90% reduction in capacity-related issues)
- **Lost trip rate**: 0.75 (75% of potential trips are currently lost due to capacity constraints)

```
est_recovered_trips = avg_daily_trips × 365 × imbalance_reduction × lost_trip_rate
```

Breaking this down:
- `avg_daily_trips × 365` = Annual trips at current capacity
- `× imbalance_reduction` = Accounts for the fact that expansion won't solve 100% of problems
- `× lost_trip_rate` = Estimates what percentage of trips are currently being lost

**Example**: If `avg_daily_trips = 5`:
- `est_recovered_trips = 5 × 365 × 0.9 × 0.75 = 1,231.875 trips/year`

**Interpretation**: We estimate that by expanding this station, we can recover approximately 1,232 additional trips per year that are currently being lost due to capacity constraints.

---

### Step 5: Calculate Revenue Gain

Multiply recovered trips by revenue per trip:

```
est_revenue_gain = est_recovered_trips × revenue_per_trip
```

**Example**: If `est_recovered_trips = 1,232` and `revenue_per_trip = $2.95`:
- `est_revenue_gain = 1,232 × $2.95 = $3,634.40/year`

---

### Step 6: Calculate Capital Expenditure (CAPEX) Cost

Calculate the one-time cost of adding docks:

```
capex_cost = added_docks × capex_per_dock
```

**Example**: If `added_docks = 13` and `capex_per_dock = $500`:
- `capex_cost = 13 × $500 = $6,500` (one-time cost)

---

### Step 7: Annualize the Capital Cost

Spread the one-time cost over the useful life of the docks:

```
annualized_cost = capex_cost / capex_annualization_years
```

**Example**: If `capex_cost = $6,500` and `capex_annualization_years = 10`:
- `annualized_cost = $6,500 / 10 = $650/year`

This represents the annual "cost" of the capital investment.

---

### Step 8: Calculate Net Gain (Profit)

Subtract annualized cost from revenue gain:

```
net_gain = est_revenue_gain - annualized_cost
```

**Example**: 
- `est_revenue_gain = $3,634.40/year`
- `annualized_cost = $650/year`
- `net_gain = $3,634.40 - $650 = $2,984.40/year`

**Interpretation**: After accounting for the cost of adding docks, this station expansion would generate approximately $2,984 in additional annual profit.

---

## Complete Example Calculation

Let's walk through a complete example for a hypothetical station:

**Inputs:**
- Total checkouts: 2,000 rides
- Days tracked: 250 days
- Average CV: 60% (high variability)

**Step 1: Average Daily Trips**
```
avg_daily_trips = 2,000 / 250 = 8 trips/day
```

**Step 2: Estimated Current Docks**
```
estimated_docks = max(10, min(50, round(8 × 5))) = max(10, min(50, 40)) = 40 docks
```

**Step 3: Proposed Expansion**
```
added_docks = round(40 × 0.5) = 20 docks
new_capacity = 40 + 20 = 60 docks
```

**Step 4: Recovered Trips**
```
est_recovered_trips = 8 × 365 × 0.9 × 0.75 = 1,971 trips/year
```

**Step 5: Revenue Gain**
```
est_revenue_gain = 1,971 × $2.95 = $5,814.45/year
```

**Step 6: Capital Cost**
```
capex_cost = 20 × $500 = $10,000 (one-time)
```

**Step 7: Annualized Cost**
```
annualized_cost = $10,000 / 10 = $1,000/year
```

**Step 8: Net Gain**
```
net_gain = $5,814.45 - $1,000 = $4,814.45/year
```

---

## Bootstrap Confidence Intervals

The bootstrap analysis runs 1,000 iterations where:

1. For each iteration, we randomly resample (with replacement) the candidate stations
2. Calculate the total net gain across all sampled stations
3. This gives us 1,000 different estimates of total impact

The 95% confidence interval represents:
- **Lower bound**: 2.5th percentile of the 1,000 estimates
- **Upper bound**: 97.5th percentile of the 1,000 estimates

This tells us: "We are 95% confident that the true annual net impact lies between [lower] and [upper] dollars."

---

## Key Assumptions

The calculations rely on several assumptions:

1. **Revenue per trip ($2.95)**: Based on average BlueBikes pricing
2. **Cost per dock ($500)**: Estimated installation and equipment cost
3. **Dock lifespan (10 years)**: Assumed useful life for annualization
4. **Imbalance reduction (90%)**: Assumes expansion solves most capacity issues
5. **Lost trip rate (75%)**: Estimates that 75% of potential trips are lost due to capacity constraints
6. **Dock estimation formula**: Uses 5 trips per dock per day as a proxy

These assumptions can be adjusted based on:
- Actual BlueBikes financial data
- Industry benchmarks
- Station-specific characteristics
- Historical expansion project data

---

## Limitations

1. **Simplified dock estimation**: The formula `avg_daily_trips × 5` is a proxy. Real dock counts would be more accurate.

2. **Lost trip estimation**: The 75% lost trip rate is an assumption. Actual lost trips could be measured through:
   - User surveys
   - App data showing "station full" errors
   - Demand modeling

3. **Revenue assumption**: $2.95 per trip is an average. Actual revenue varies by:
   - Membership vs. single-ride pricing
   - Trip duration (overtime fees)
   - Promotional pricing

4. **No operational costs**: The calculation doesn't account for:
   - Maintenance costs
   - Rebalancing operations
   - Station management overhead

5. **Linear scaling**: Assumes recovered trips scale linearly with capacity, which may not hold for very large expansions.

---

## Summary

The financial analysis provides a **conservative estimate** of the potential return on investment for dock expansion. It focuses on:
- **Revenue opportunity**: Additional trips that could be captured
- **Capital investment**: Cost of adding docks
- **Net benefit**: Annual profit after accounting for capital costs

The bootstrap method adds statistical rigor by providing confidence intervals, acknowledging uncertainty in the estimates.

