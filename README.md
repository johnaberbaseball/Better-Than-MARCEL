# Better Than MARCEL

A projection model in R that beats MARCEL by ~1% on RMSE, using defensive data to project offensive output.

## Results

The model is tested on 154 players with 100+ plate appearances projecting 2021 OPS. MARCEL's RMSE and MAE were .0899 and .0699 respectively, as compared to this model's .0890 and .0688.

This model particularly excels in projecting catchers (9% better), first basemen (5%), and second basemen (3%), while third basemen fare slightly worse (down 4%). Catchers are treated too generously by MARCEL (by regressing hitters toward a .720 league-average OPS), and this model pulls them down accordingly by 32 points of OPS to improve accuracy.

## Background

I'm very interested in the relationship between a player's defensive ability and his offensive output, and wanted to see if it could be exploited to beat MARCEL. WAR, of course, includes Tom Tango's positional adjustment into its calculations—the reasoning being that players at harder positions (like catcher and shortstop) should not be penalized as highly as players at easier positions (like DH or first base) for poor offense. Following that logic/correlation, I wanted to see if I could beat MARCEL's offensive projections by supplementing it with only defensive data/positional context. So I built a simple "starter" model using the most basic piece of information about a player's defensive ability: their primary position.

## Method

First, I recreated MARCEL from scratch in R (5/4/3 weighting across last three seasons, regression toward league average based on plate appearances, and age adjustment), then downloaded a CSV containing both Baseball Reference IDs and primary positions by season from TheBaseballScholar.com. I then computed their primary positions in R, aggregating each player's position that they played most, and inserted it into my primary dataset.

Then, I created a positional adjustment, taking the mean of the MARCEL_OPS column I previously calculated as a "league-average" OPS. Finally, I computed the adjusted BETTER_THAN_MARCEL projections (MARCEL_OPS + (positional-average OPS - league-average OPS)) to create an offensive projection (using only the most basic defensive data/positional context) to beat MARCEL.

## Data

The hitting data I used included plate appearances and OPS for 175 players from 2018-2020 and came with the SABR Analytics Certification Level 3 coursework. The validation compares against actual 2021 OPS, with 154 players reaching 100+ plate appearances.

## Limitations

Limitations include the 2020 season's 60-game sample size and no park/league adjustments. Of course, primary position/most playing time is a crude stand-in for true defensive ability. This model was built purely as an exercise in discovering the smallest possible link between defensive ability and offensive output, but it does open up the door to many more layers of refinement.

## Next Steps

Future iterations of this model will include regressing toward each position's mean from the start rather than shifting after the fact (i.e. bringing third basemen's adjustment back to MARCEL's already close one).

## Running It

The script (Better_Than_MARCEL.R) needs R with dplyr, and was built upon datasets included with the SABR Level 3 coursework + the position CSV.
