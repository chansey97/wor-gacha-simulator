## Count the number of pulls per 5-stars hero (sample size: 10000)

|                                | average | median | max    | stddev | hard pity % |
| -----------------------------  | ------- | ------ | ------ | ------ | ------- |
| Invocation of Spirits          | 121.0   | 139.0  | 195.0  | 65.9   | 0%      |
| Crazy Invocation of Spirits    | 85.5    | 70.0   | 195.0  | 62.8   | 0%      |
| Limited Invocation of Spirits  | 86.8    | 100.0  | 194.0  | 35.8   | -       |
| Surprise Invocation of Spirits | 60.4    | 69.5   | 97.5   | 33.1   | -       |
| Ancient Summoning              | 49.2    | 35.0   | 194.0  | 44.7   | -       |
| Divine Summoning               | 10.8    | 12.0   | 20.0   | 6.0    | 5.65%   |
| Crazy Divine Summoning         | 7.3     | 6.0    | 20.0   | 5.3    | 1.44%   |

**Surprise > Crazy > Limited > Regular Spirits**

Limited Invocation of Spirits > Regular Invocation of Spirits: 

The Limited Invocation of Spirits maintains an independent pity system.  When a 5-stars hero is obtained, the shared pity resets, but the  independent pity remains intact. This mechanism makes it superior to the regular Invocation of Spirits.

Crazy Invocation of Spirits > Limited Invocation of Spirits: 

Even with the independent pity system of Limited Invocation of Spirits, it still cannot surpass the Crazy Invocation of Spirits' doubled probability boost for obtaining 5-stars heroes.

## Count the number of pulls per 5-stars lord hero (sample size: 10000)

|                               | average | median | max     | stddev | hard pity % |
| ----------------------------- | ------- | ------ | ------- | ------ | ----------- |
| Invocation of Spirits         | 1510.3  | 1062.0 | 13900.0 | 1450.0 | -           |
| Crazy Invocation of Spirits   | 1034.5  | 733.0  | 11208.0 | 1007.3 | -           |
| Ancient Summoning             | 97.8617 | 87.0   | 196.0   | 66.6   | 0%          |
| Divine Summoning              | 160.8   | 114.0  | 1533.0  | 154.8  | -           |
| Crazy Divine Summoning        | 109.7   | 78.0   | 919.0   | 106.4  | -           |

## **Optimal Dual-Pool Strategy**

Generally, the Limited Invocation of Spirits is accompanied by another summoning pool (e.g., Invocation of Spirits or Crazy Invocation of Spirits), allowing players to employ the "Dual-Pool Strategy".

Using the Invocation of Spirits as the **secondary pool** example:

Begin summoning in Limited Invocation of Spirits (initial shared pity: 0).

If no 5-stars hero is obtained within X pulls:

- Switch to Invocation of Spirits until obtaining a 5-stars hero
- Return to Limited Invocation of Spirits until obtaining the first limited 5-stars hero

If a 5-stars hero is obtained within X pulls:

- If it's the limited 5-stars hero: Stop
- Otherwise: Continue summoning until obtaining the first limited 5-stars hero

**Choosing X:**

- Low X (early switching):

  Excessive use of the secondary pool fails to leverage the Limited Pool's independent pity, reducing success rate.

- High X (delayed switching): 

  Improves success rate but risks significant losses in worst-case scenarios (e.g., obtaining the limited hero on the 190th pull).

**Optimal X = 133 minimizes the maximum required pulls per 5-stars hero while avoiding worst-case scenarios.**

*P.S. Mathematical derivation:*

Given constraints:

- $X > 100$

- $\frac{X + (200 - X) + (200 - X)}{2} \geq X$

Minimizing  $f(x) = \frac{X + (200 - X) + (200 - X)}{2}$:

- Solution range:  $100 < x \leq \frac{400}{3}$
- As $f(x)$  is a linearly decreasing function, minimum occurs at $X = \frac{400}{3} ≈ 133.333$.

**Q:** What if the Limited Invocation of Spirits is accompanied by two pools (e.g., Special Invocation and Crazy Invocation)?

**A:** If the Limited Pool features good heroes, prioritize Special Invocation as the secondary pool. The Crazy Pool's doubled probability boost provides diminishing returns under Dual-Pool Strategy.

| Limited Invocation of Spirits + Special Invocation of Spirits | average | max    |
| ------------------------------------------------------------- | ------- | ------ |
| X = 133                                                       | 94.2    | 133.3  |
| X = 180                                                       | 88.6    | 180.0  |

| Limited Invocation of Spirits + Crazy Invocation of Spirits | average | max    |
| ----------------------------------------------------------- | ------- | ------ |
| X = 133                                                     | 92.3    | 133.3  |
| X = 180                                                     | 88.5    | 197.0  |

**Q:** What if a shared pity exists before executing the Dual-Pool Strategy?

**A:** Keep the shared pity zero before executing the Dual-Pool Strategy!
