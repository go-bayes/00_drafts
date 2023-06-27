---
title: "Causal effects of religious de-identification on multi-dimensional well-being"
subtitle: "An outcome-wide study"
abstract: |
  Max Weber described the loss of religion as "the disenchantment of the world." However, quantifying the mangitude of disenchantment from the loss of religion has yet to be attempted. Indeed, whether religious disaffiliation causally affects people at all is unclear and cannot be determined from cross-sectional data.  Here, we use a potential outcomes framework from causal inference to contrast the expected effects of religious disaffilation accross a wide range of multi-dimensional wellbeing, one year after religious change. 
author: 
  - name: Joseph A. Bulbulia
    affiliation: Victoria University of Wellington, New Zealand
    orcid_id: 0000-0002-5861-2056
    email: joseph.bulbulia@vuw.ac.nz
    corresponding: yes
  - name: Don E Davis
    affiliation: Georgia State University
    orcid_id: 0000-0003-3169-6576 
  - name: Ken Rice
    affiliation: Georgia State University 
  - name: Geoffrey Troughton
    affiliation: Victoria University of Wellington
  - name: Daryl Van Tongeren
    affiliation: Hope College
  - name: Chris G. Sibley
    affiliation: School of Psychology, University of Auckland
    orcid_id: 0000-0002-4064-8800
keywords:
  - Author order TBA.
format: latex
keep-md: true
execute:
  warning: false
  eval: false
  echo: false
  include: false
---













## Introduction

Max Weber famously described the loss of religion as "the disenchantment of the world" [@wilson2014]. However, it is unclear whether, and in which ways, disaffiliation causes disenchantment.

Although cross-sectional data may be suggestive of relationships, cross-sectional data are essentially useless for inferring causality. For example, it is possible that people who score low in meaning of life tend to shed their religious affiliation at a higher rate than people who score high in meaning of life. The associations in cross-sectional data cannot rule out this possibility. It is plausible that individuals with low life meaning are more likely to distance themselves from religious institutions, leading to an over-representation of disenchantment among non-religious individuals in cross-sectional surveys. If we were to intervene to make the disenchanted re-affiliate this might lead to greater loss of meaning. 

Here, we use longitudinal data to emulate an idealised experiment, thus providing a clearer window into the direction of causality [@hernán2016; @bulbulia2022; @hernan2023]

### Sample





### Eligibility criteria

To emulate randomisation with observational data, we selected (1) those participants who were identified as religious at the baseline wave (NZAVS wave 2018) and who (2) reported a religious identification one year later. There were 12,600 participants who met these criteria.

### Disaffiation rates





As indicated in @tbl-transition, of the 12600 people who were religious at baseline, 1977 people dis-affiliated participants one year later (NZAVS wave 2019), at the measurement year.[^1]

[^1]: Cite our Markov paper showing that much religious change is probably artifactual.

|     From      | religious yes | religious not |
|:-------------:|:-------------:|:-------------:|
| religious yes |     10623     |     1977      |

: Transition matrix {#tbl-transition}

### Assumptions for causal inference

To assess a causal effect we must contrast how the world would turn out if we were to intervene. Generally, we cannot observe individual causal effects because for any individual case, we only observe the intervention or its absence. We cannot both intervene and not-intervene at once. This is called the fundamental problem of causal inference [@bulbulia2022]. Although we cannot generally observe unit-level causal effects, it may be possible to estimate average causal effects. We do this by contrasting the average effect in the exposed group with the average effect in the unexposed unexposed group. For example, average of the contrast (or equivalently the contrast of the the averages)[^2] on the difference scale may be expressed:

[^2]: Note that mathematically, the difference in the average expectation is equivalent to the average of the differences in expectation.



```{=tex}
\begin{alignat*}{2}
ATE & = E[Y(1)) - E(Y(0)]\\
& = E=[Y(1) - Y(0)]
\end{alignat*}
```


Estimating the average treatment effects (ATE) of binary exposures or contrasts between different exposure levels involves understanding causal inference as counterfactual data science. The ATE is expressed as:



```{=tex}
   \begin{align*}
    ATE = E[Y(a) - Y(a*)]
    \end{align*}
```


Our causal inference is grounded on three critical assumptions:

#### Identification assumption 1: Causal consistency

Causal consistency assumes the observed outcome aligns with the potential outcome for a given exposure level:

$$Y^{observed} = AY(a=1) + (1-A)Y(a=0)$$

Observed outcomes can represent counterfactual outcomes under certain exposures, such that:

$$
Y^{observed}_i = 
\begin{cases} 
Y_i(~a^*) & \text{if } A_i = a* \\
Y_i(~a~) & \text{if } A_i = a
\end{cases}
$$

Causal consistency also assumes no interference between unit treatments, allowing potential outcomes to be set to the observed outcomes. For this assumption to hold, we require "treatment variation irrelevance." If there are (1) well-defined outcomes for each treatment version, and (2) no confounding effects, the multiple versions of treatments can be used to estimate the causal effect:

$$K \coprod Y(k) | L$$ or equivalently $$Y(k) \coprod K | L$$

Here, the treatement $A$ is essentially a function of $K$ treatments, $A = f(k_1...k_v)$ versions

Limitations exist, however, when interventions are ill-defined, or the causal effect's interpretation is ambiguous. Put simply, given there are unknown ways of becoming religiously disaffiliated the interpretation of "disaffiliation" may be strained. It is strained in the sense that we would not know how to intervene to *make* a religiously affiliated person disaffiliate. We will return to this question in the discussion.

#### Identification assumption 2: Exchangability

Exchangability assumes treatment assignment is independent of potential outcomes, given observed covariates. This is the "no-confounding" assumption that many psychologists have learned in association with experimental design. In the setting of obervational data, we emulate randomisation by conditioning on indicators that may lead to an association of the exposure $A$ and the outcome $Y$ in the absence of causation.

$$Y(a)\coprod  A|L$$ or $$A \coprod  Y(a)|L$$

Where exchangability holds, we calculate the Average Treatment Effect (ATE)

$$
ATE = E[Y(a*)|L = l] - E[Y(a)|L = l] 
$$

Put differently, conditioning on confounders ensures *balance* in their distribution across exposures.

#### Identification assumption 3: Positivity

Positivity is satisfied if there's a positive probability of receiving or not receiving exposure at all covariate levels. Expressed as:



```{=tex}
\begin{equation}
0 < \Pr(A=a|L)<1, ~ \forall a \in A, ~ \forall a \in L
\end{equation}
```


There are two types of positivity violation.

-   **Random non-positivity**: Occurs when the causal effect of a missing observation is presumed to exist. This violation is the only one verifiable by data. Here, we check and report it.

-   **Deterministic non-positivity**: Occurs when the causal effect is inconceivable. For example, the causal effect of hysterectomy in biological males violates deterministic non-positivity.

### Identification Strategy

Effects must follow causes. To avoid the problems of reverse causation, we measured outcomes during the year following the exposure (NZAVS wave 2020). We used doubly robust methods. These combine inverse probability of treatment weights (propensity scores) with regression stratification. There are two models at work in a doubly robust estimator.

### Unconditional Doubly Robust Estimation for Causal Effect Estimation

We use a Doubly Robust Estimation method, which effectively combines the strengths of the IPTW and G-computation methods (see: [here](https://go-bayes.github.io/psych-434-2023/content/09-content.html#comprehensive-checklist-for-detailed-reporting-of-a-causal-inferenctial-study-e.g.-assessment-3-option-2). The technique utilises both the propensity score and the outcome model, making it "doubly robust." This implies that if either of these models is correctly specified, the estimation will not be biased.

**Step 1** The first step is to estimate the propensity score. The propensity score, denoted as $e(L)$, is the conditional probability of the exposure $A = 1$ given the covariates $L$. The appropriate model to estimate this can be chosen based on the nature of the data and the exposure.

$$e = P(A = 1 | L) = f_A(L; \theta_A)$$

In this equation, $f_A(L; \theta_A)$ is a function that estimates the probability of the exposure $A = 1$ given covariates $L$. Here, we use the `ebalance` method from the `clarify` package, which we have found to ensure good balance on the confounders (see fig below). We then calculate the weights for each individual, denoted as $v$, using the estimated propensity score:

$$
v = 
\begin{cases} 
\frac{1}{e} & \text{if } A = 1 \\
\frac{1}{1-e} & \text{if } A = 0 
\end{cases}
$$

Here, $v$ depends on $A$, and is calculated as the inverse of the propensity score for exposed individuals and as the inverse of $1-e$ for unexposed individuals.

**Step 2** The next step involves fitting a weighted outcome model. Using the weights computed from the estimated propensity scores, a model for the outcome $Y$, conditional on the exposure $A$, is fitted.

$$ \hat{E}(Y|A, L; V) = f_Y(A, L ; \theta_Y, V) $$

In this model, $f_Y$ is a function (in our case a weighted regression model) with parameters $θ_Y$. The weights $V$ are incorporated into the estimation process, affecting the contribution of each observation to the estimation of $θ_Y$, but they are not an additional variable in the model. Additionally, following @agnostic, we take the interaction of the exposure and baseline covariates when estimating our regression model. For binary outcomes we model the rate ratio using Poisson regression. Although binomial regression is acceptable when the outcome is rare (less than 10%), non-collapsability leads means that we cannot interpret results as marginal causal effects. For consistency we use the Poisson model with robust standard errors.

**Step 3** The third step is to simulate the potential outcome for each individual under the hypothetical scenario where everyone is exposed to the intervention $A=a$, irrespective of their actual exposure level:

$$\hat{E}(a) = \hat{E}[Y_i|A=a; L,\hat{\theta}_Y, v_i]$$

This expectation is calculated for each individual $i$, with individual-specific weights $v_i$.

**Step 4** Finally, we estimate the average causal effect. We compute the estimated expected value of the potential outcomes under each intervention level:

$$\hat{\delta} = \hat{E}[Y(a)] - \hat{E}[Y(a')]$$

The difference $\delta$ represents the average causal effect of changing the exposure from level $a'$ to level $a$.

For standard errors and confidence intervals, we use simulation-based inference methods [@greifer2023].

### Baseline confounders, exposure, and outcome measures

See Appendix 1













## Results

### Effects on health





Figure @fig-results-health-propensity-scores reveals strong imbalance prior to propensity score estimation.



::: {.cell}
::: {.cell-output-display}
![Love plot for propensity score analysis: Health outcomes.](target-trial-religion-loss_files/figure-latex/fig-results-health-propensity-scores-1.pdf){#fig-results-health-propensity-scores width=100%}
:::
:::




As indicate in \@ fig-results-health, the expected + one-year effect of religious disaffiliation does not reliably affect health as subjectively reported across NZAVS domains. However, we find that disaffilation is causally associated with increases in both the average intensity and frequency of alcohol consumption.

It has long been known that alcohol consumption can have damaging health and social effects. However, it is unclear whether the changes we detect here translate to other domains.


::: {.cell}
::: {.cell-output-display}
![Causal effects of religious loss on reported physical health](target-trial-religion-loss_files/figure-latex/fig-results-health-1.pdf){#fig-results-health}
:::
:::

::: {.cell}
::: {.cell-output-display}
![Causal effects of religious loss on smoking (risk ratio)](target-trial-religion-loss_files/figure-latex/fig-results-health-rr-1.pdf){#fig-results-health-rr}
:::
:::


### Effects on embodied well-being




::: {.cell}
::: {.cell-output-display}
![Causal effects of religious loss on embodied well-being](target-trial-religion-loss_files/figure-latex/fig-results-embodied-1.pdf){#fig-results-embodied}
:::
:::


### Effects on practical well-being




::: {.cell}
::: {.cell-output-display}
![Causal effects of religious loss on practical well-being](target-trial-religion-loss_files/figure-latex/fig-results-practical-well-being-1.pdf){#fig-results-practical-well-being}
:::
:::


### Effects on reflective well-being




::: {.cell}
::: {.cell-output-display}
![Causal effects of religious loss on reflective well-being](target-trial-religion-loss_files/figure-latex/fig-results-reflective-well-being-1.pdf){#fig-results-reflective-well-being}
:::
:::


### Effects social well-being




::: {.cell}
::: {.cell-output-display}
![Causal effects of religious loss on reflective well-being](target-trial-religion-loss_files/figure-latex/fig-results-social-well-being-1.pdf){#fig-results-social-well-being}
:::
:::

::: {.cell}
::: {.cell-output-display}
![Causal effects of religious loss on volunteering.](target-trial-religion-loss_files/figure-latex/fig-models-social-risk-ratio-1.pdf){#fig-models-social-risk-ratio}
:::
:::


## Discussion

Here, we combined rigorous methods from causal epidemiology with national scale time-series data to estimate the causal effects of religious disaffiliation on multidimensional well-being. We used doubly robust methods that combine propensity score weights with regression stratification. By controlling for measures of all outcomes at baseline we reduce the probability of unmeasured confounding. Because this cannot be ensured, we report E-values, a sensitivity analysis that clarifies the "worst case" scenario for an unmeasured confounder to explain away the results.

**Health domain**: The expected +1 year effect of religious disaffiliation is to increase both the average intensity and frequency of alcohol consumption. We do not find reliable results on other health domains.

**Embodied well-being domains**: We do not find reliable evidence for a +1 year effect of religious disaffiliation on embodied will being (i.e. distress, fatigue)

**Practical well-being**: The expected +1 year effect of religious disaffiliation is to diminish wishes for more self control. That is good. However the one-year effect of disaffiliation is to increase vengeful rumination. That is not good.

**Reflective well-being**: The expected +1 year effect of religious disaffiliation is increase life satisfaction. That is good. However the one-year effect of disaffiliation is to decrease a sense of purpose in life. That is not good.

**Social well-being-being**: Disaffiliation is expected to cause reduction in charitable giving and volunteering (consistent \[cite a different study\]). However we do not find that disaffiliation as such reduces other aspects of social well-being.

### Generalisability and Transportability

-   We can generalise to the religious population of New Zealand. Whether results transport elsewhere is unclear.

### Assumptions and Limitations

1.  Consistency...
2.  Positivity...
3.  Exhangeability...

Also

1.  Measurement of religious change -- Markov models show less
2.  Measurement error
3.  Loss to follow up attrition (requires modelling assumptions.)

### Theoretical Relevance

This study is important both for its methods and findings.

1.  The bar for causality in this study very high.
2.  It would generally be unexpected that in a country such as New Zealand, which is a highly secular, a change in one's religious affiliation would induce measurable effects on people within only one-year.

### Future Research

1.  We did not compare religious disaffiliates to people who are secular. Loss of religion suggests a loss of charity. However, secular people might be less charitable still. Future research...
2.  Previous research shows differences in these comparison groups [@vantongeren2020; @sibley2012a].

### Real-world Implications

In practical terms, the real-world implications of the findings, are ...



{{< pagebreak >}}





## Appendix A. Measures

### Baseline confounding control

#### Age (waves: 1-15)

We asked participants' age in an open-ended question ("What is your age?" or "What is your date of birth").

#### Disability (waves: 5-15)

We assessed disability with a one item indicator adapted from @verbrugge1997, that asks "Do you have a health condition or disability that limits you, and that has lasted for 6+ months?" (1 = Yes, 0 = No).

#### Education Attainment (waves: 1, 4-15)

Participants were asked "What is your highest level of qualification?". We coded participans highest finished degree according to the New Zealand Qualifications Authority. Ordinal-Rank 0-10 NZREG codes (with overseas school quals coded as Level 3, and all other ancillary categories coded as missing) See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf

#### Employment (waves: 1-3, 4-11)

We asked participants "Are you currently employed? (This includes self-employed or casual work)". \* note: This question disappeared in the updated NZAVS Technical documents (Data Dictionary).

#### European (waves: 1-15)

Participants were asked "Which ethnic group do you belong to (NZ census question)?" or "Which ethnic group(s) do you belong to? (Open-ended)" (wave: 3). Europeans were coded as 1, whereas other ethnicities were coded as 0.

#### Ethnicity (waves: 3)

Based on the New Zealand Cencus, we asked participants "Which ethnic group(s) do you belong to?". The responses were: (1) New Zealand European; (2) Māori; (3) Samoan; (4) Cook Island Māori; (5) Tongan; (6) Niuean; (7) Chinese; (8) Indian; (9) Other such as DUTCH, JAPANESE, TOKELAUAN. Please state:. We coded their answers into four groups: Maori, Pacific, Asian, and Euro (except for Time 3, which used an open-ended measure).

#### Gender (waves: 1-15)

We asked participants' gender in an open-ended question: "what is your gender?" or "Are you male or female?" (waves: 1-5). Female was coded as 0, Male was coded as 1, and gender diverse coded as 3 [@fraser_coding_2020]. (or 0.5 = neither female nor male)

#### Income (waves: 1-3, 4-15)

Participants were asked "Please estimate your total household income (before tax) for the year XXXX". To stablise this indicator, we first took the natural log of the response + 1, and then centred and standardised the log-transformed indicator.

#### Job Security (waves: 1-3,4-7,9-15)

Participants indicated their feeling of job security by answering "How secure do you feel in your current job?" on a scale from 1 (not secure) to 7 (very secure).

#### Parent (waves: 5-15)

Participants were asked "If you are a parent, what is the birth date of your eldest child?" or "If you are a parent, in which year was your eldest child born?" (waves: 10-15). Parents were coded as 1, while the others were coded as 0.

#### Number of Children (waves: 1-3, 4-15)

We measured number of children using one item from @bulbulia2015. We asked participants "How many children have you given birth to, fathered, or adopted. How many children have you given birth to, fathered, or adopted?" or ""How many children have you given birth to, fathered, or adopted. How many children have you given birth to, fathered, and/or parented?" (waves: 12-15).

#### Political Orientation

We measured participants' political orientation using a single item adapted from @jost_end_2006-1.

"Please rate how politically liberal versus conservative you see yourself as being."

(1 = Extremely Liberal to 7 = Extremely Conservative)

#### NZSEI-13 (waves: 8-15)

We assessed occupational prestige and status using the New Zealand Socio-economic Index 13 (NZSEI-13) [@fahy2017]. This index uses the income, age, and education of a reference group, in this case the 2013 New Zealand census, to calculate an score for each occupational group. Scores range from 10 (Lowest) to 90 (Highest). This list of index scores for occupational groups was used to assign each participant a NZSEI-13 score based on their occupation.

Participants were asked "If you are a parent, what is the birth date of your eldest child?".

#### Living with Partner

Participants were asekd "Do you live with your partner?" (1 = Yes, 0 = No).

#### Living in an Urban Area (waves: 1-15)

We coded whether they are living in an urban or rural area (1 = Urban, 0 = Rural) based on the addresses provided.

We coded whether they are living in an urban or rural area (1 = Urban, 0 = Rural) based on the addresses provided.

#### NZ Deprivation Index (waves: 1-15)

We used the NZ Deprivation Index to assign each participant a score based on where they live [@atkinson2019]. This score combines data such as income, home ownership, employment, qualifications, family structure, housing, and access to transport and communication for an area into one deprivation score.

#### NZ-Born (waves: 1-2,4-15)

We asked participants "Which country were you born in?" or "Where were you born? (please be specific, e.g., which town/city?)" (waves: 6-15).

#### Mini-IPIP 6 (waves: 1-3,4-15)

We measured participants personality with the Mini International Personality Item Pool 6 (Mini-IPIP6) [@sibley2011] which consists of six dimensions and each dimensions is measured with four items:

1.  agreeableness,

    i.  I sympathize with others' feelings.
    ii. I am not interested in other people's problems. (r)
    iii. I feel others' emotions.
    iv. I am not really interested in others. (r)

2.  conscientiousness,

    i.  I get chores done right away.
    ii. I like order.
    iii. I make a mess of things. (r)
    iv. I ften forget to put things back in their proper place. (r)

3.  extraversion,

    i.  I am the life of the party.
    ii. I don't talk a lot. (r)
    iii. I keep in the background. (r)
    iv. I talk to a lot of different people at parties.

4.  honesty-humility,

    i.  I feel entitled to more of everything. (r)
    ii. I deserve more things in life. (r)
    iii. I would like to be seen driving around in a very expensive car. (r)
    iv. I would get a lot of pleasure from owning expensive luxury goods. (r)

5.  neuroticism, and

    i.  I have frequent mood swings.
    ii. I am relaxed most of the time. (r)
    iii. I get upset easily.
    iv. I seldom feel blue. (r)

6.  openness to experience

    i.  I have a vivid imagination.
    ii. I have difficulty understanding abstract ideas. (r)
    iii. I do not have a good imagination. (r)
    iv. I am not interested in abstract ideas. (r)

Each dimension was assessed with four items and participants rated the accuracy of each item as it applies to them from 1 (Very Inaccurate) to 7 (Very Accurate). Items marked with (r) are reverse coded.

#### Honesty-Humility-Modesty Facet (waves: 10-14)

Participants indicated the extent to which they agree with the following four statements from , @campbell2004, and @sibley2011 (1 = Strongly Disagree to 7 = Strongly Agree)

```         
i.  I want people to know that I am an important person of high status, (Waves: 1, 10-14)
ii. I am an ordinary person who is no better than others.
iii. I wouldn't want people to treat me as though I were superior to them.
iv. I think that I am entitled to more respect than the average person is.
```

### Exposure variable

#### Religious Identification (waves: 1-15)

If participants answered *yes* to "Do you identify with a religion and/or spiritual group? we asked"How important is your religion to how you see yourself?" (1 = Not important, 7 = Very important), using one item from @hoverd2010. Those participants who were not religious were imputed a score of "1".

### Health well-being outcomes

#### Alcohol Frequency (waves: 6-15)

We measured participants' frequency of drinking alcohol using one item adapted from @ministryofhealth2013 . Participants were asked "How often do you have a drink containing alcohol?" (1 = Never - I don't drink, 2 = Monthly or less, 3 = Up to 4 times a month, 4 = Up to 3 times a week, 5 = 4 or more times a week, 6 = Don't know).

#### Alcohol Intensity (waves: 6-15)

We measured participants' intensity of drinking alcohol using one item adapted from [@ministryofhealth2013]. Participants were asked "How many drinks containing alcohol do you have on a typical day when drinking alcohol? (number of drinks on a typical day when drinking)"

#### Body Mass Index (waves: 2-3, 4-15)

Participants were asked "What is your height? (metres)" and "What is your weight? (kg)". Based on participants indication of their height and weight we calculated the BMI by dividing the weight in kilograms by the square of the height in meters.

#### Short-Form Subjective Health (waves: 5-15)

Participants' subjective health was assessed by three items selected from the MOS 36-item short-form health survey [@warejr1992]. The items were

```         
1.  "In general, would you say your health is...";
2.  "I seem to get sick a little easier than most people.";
3.  "I expect my health to get worse." Participants responded to those items on a scale (1 = Poor to 7 = Excellent).
```

The second and third items were negatively-worded, so we reversed the responses.

#### Hours of Exercise (waves: 1, 4-15)

We measured hours of exercising using one item from @sibley2011. We asked participants to estimate and report how many hours they spend in exercise/physical activity last week. To stablise this indicator, we first took the natural log of the response + 1, and then centred and standardised the log-transformed indicator.

#### Hours of Sleep (waves: 5-15)

Participants were asked "During the past month, on average, how many hours of *actual sleep* did you get per night".

#### Smoker (waves: 4-15)

We asked participants whether they are currently smoking or not (1 = Yes or 0 = No), using a single item: "Do you currently smoke?" or "Do you currently smoke tobacco cigarettes?" (waves: 10-15) from @muriwai_looking_2018.

### Embodied well-being outcomes

#### Kessler-6 (waves: 2-3,4-15)

We measured psychological distress using the Kessler-6 scale [@kessler2002], which exhibits strong diagnostic concordance for moderate and severe psychological distress in large, cross-cultural samples [@kessler2010; @prochaska2012]. Participants rated during the past 30 days, how often did... (

```         
1.  "... you feel hopeless";
2.  "... you feel so depressed that nothing could cheer you up";
3.  "... you feel restless or fidgety";
4.  "... you feel that everything was an effort";
5.  "... you feel worthless";
6.  " you feel nervous?"
```

Ordinal response options for the Kessler-6 are: "None of the time"; "A little of the time"; "Some of the time"; "Most of the time"; "All of the time."

#### Fatigue (waves: 5-15)

We assessed subjective fatigue by asking participants, "During the last 30 days, how often did ... you feel exhausted?" Responses were collected on an ordinal scale (0 = None of The Time, 1 = A little of The Time, 2 = Some of The Time, 3 = Most of The Time, 4 = All of The Time).

#### Rumination

"During the last 30 days, how often did.... you have negative thoughts that repeated over and over?"

Ordinal response options for the Kessler-6 are: "None of the time"; "A little of the time"; "Some of the time"; "Most of the time"; "All of the time."

### Practical well-being outcomes

#### Body Satisfaction (waves: 2-3, 4-15)

We measured body satisfaction with one item from @stronge_facebook_2015: "I am satisfied with the appearance, size and shape of my body", which participants rated from 1 (very inaccurate) to 7 (very accurate).

#### Emotional Regulation (waves: 10-13)

We measured participants' levels of emotional regulation using three items adpated from @gratz_multidimensional_2004 and @gross_individual_2003:

```         
1.  "When I feel negative emotions, my emotions feel out of control.";
2.  "When I feel negative emotions, I suppress or hide my emotions.";
3.  "When I feel negative emotions, I change the way I think to help me stay calm."
```

Participants were asked to indicate the extent to which they agree with these items (1 = Strongly Disagree to 7 = Strongly Agree).

#### Perfectionism (waves: 10-15)

We assessed participants' perfectionism using three items from @rice_short_2014: (1) Doing my best never seems to be enough; (2) My performance rarely measures up to my standards; (3) I am hardly ever satisfied with my performance. Participants indicated the extent to which they agree with these items (1 = Strongly Disagree to 7 = Strongly Agree).

#### Power Dependence

Participants' Power dependence was measured using two items:

```         
1." I do not have enough power or control over important parts of my life."
2". Other people have too much power or control over important parts of my life. 
```

Participants indicated their agreement with these items" (1 = Strongly Disagree to 7 = Strongly Agree).

#### Self-Respect (waves: 3, 4-11, 15)

We assessed participants' levels of self-respect using an item adapted from @tyler_understanding_1996. Participant indicated the extent to which they agree with the statement ("If they knew me, most NZers would respect what I have accomplished in life") on a likert scale (1 = Strongly Disagree to 7 = Strongly Agree)

#### Self-Control (waves: 5-15)

Participants were asked to indicate the extent to which they endorse the two items

```         
1.  "In general, I have a lot of self-control"
2.  "I wish I had more self-discipline"
```

The scale is from @tangney_high_2004. The responses to the items ranged from 1 (Strongly Disagree) to 7 (Strongly Agree).

#### Self-Esteem (waves: 1-3, 4-15)

We measured participants' self-esteem using three items adapted from @Rosenberg1965. Participants were instructed to circle the number that best represents how accurately each statement describes them. Participants responded to the items

```         
1.  "On the whole am satisfied with myself"
2.  "Take a positive attitude toward myself"
3.  "Am inclined to feel that I am a failure") on a likert-type scale (1 = Very inaccurate to 7 = Very accurate).
```

#### Sexual Satisfaction (waves: 10-15)

Participants were asked "How satisfied are you with your sex life?" (1 = Not satisfied to 7 = Very satisfied).

#### Vengeful Rumination (waves: 10-15)

We assessed participants' vengeful rumination using three items, respectively adapted from @caprara_indicators_1986 and @berry_forgivingness_2005, and developed for NZAVS: (1) Sometimes I can't sleep because of thinking about past wrongs I have suffered; (2) I can usually forgive and forget when someone does me wrong; (3) I find myself regularly thinking about past times that I have been wronged. Participants indicated their agreement with these items (1 = Strongly Disagree to 7 = Strongly Agree). The values for the second item were reversely coded.

### Reflective well-being

#### Meaning of Life (waves: 10-15)

We assessed participants' levels of life meaning using two items from @steger_meaning_2006:

```         
1.  My life has a clear sense of purpose;
2.  I have a good sense of what makes my life meaningful.
```

Participants indicated their agreement with these items (1 = Strongly Disagree to 7 = Strongly Agree).

#### Satisfaction with Life (waves: 1-3,4-15)

We measured life satisfaction with two items adapted from the Satisfaction with Life Scale [@diener1985]:

```         
1.  "I am satisfied with my life" and
2.  "In most ways my life is close to ideal".
```

Participants responded on a scale from 1 (Strongly Disagree) to 7 (Strongly Agree).

#### Personal Wellbeing (waves: 1-3, 4-15)

We measured participants' subjective wellbeing using three items from the Australian Unity Wellbeing Index [@cummins_developing_2003]:

```         
1.  your health;
2.  Your standard of living;
3.  Your future security; 4 Your personal relationships.
```

Participants read an instruction ("The following items assess your current satisfaction with different aspects of your life and aspects of New Zealand more generally") and indicated their satisfaction with those items (0 = Completely Dissatisfied to 10 = Completely Satisfied).

#### Standard Living

We measured participants' satisfaction with their standard of living using an item from the Australian Unity Wellbeing Index [@cummins_developing_2003]. Participants read an instruction ("Please rate your level of satisfaction with the following aspects of your life and New Zealand.") and responded to an item

```         
- "Your standard of living"
```

on a 10-point scale (0 = completely dissatisfied to 10 = completely satisfied).

### Social well-being outcomes

#### Charity Donation (waves: 1-3, 4-15)

Using one item from @hoverd2010, we asked participants "How much money have you donated to charity in the last year?". To stablise this indicator, we first took the natural log of the response + 1, and then centred and standardised the log-transformed indicator.

#### Felt Belongingness (waves: 1-3, 4-15)

We assessed felt belongingness with three items adapted from the Sense of Belonging Instrument [@hagerty1995]:

```         
1.  "Know that people in my life accept and value me";
2.  "Feel like an outsider";
```

3.  "Know that people around me share my attitudes and beliefs".

Participants responded on a scale from 1 (Very Inaccurate) to 7 (Very Accurate). The second item was reversely coded.

#### Ethnic group impermeability (waves: 9-13)

The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.

#### Individual Permeability (waves: 9-13)

Participants indicated the extent to which they agree with the statement, "I believe I am capable, as an individual of improving my status in society.", from @tausch2015 (1 = Strongly Disagree to 7 = Strongly Agree).

#### Sense of Community (waves: 6-15)

We measured sense of community with a single item from @sengupta2013: "I feel a sense of community with others in my local neighbourhood." Participants answered on a scale of 1 (strongly disagree) to 7 (strongly agree).

#### Support (waves: 1-3, 4-15)

Participants' perceived social support was measured using three items from @cutrona1987 and @williams_cyberostracism_2000:

```         
1.  "There are people I can depend on to help me if I really need it";
2.  "There is no one I can turn to for guidance in times of stress";
3.  "I know there are people I can turn to when I need help." 
```

Participants indicated the extent to which they agree with those items (1 = Strongly Disagree to 7 = Strongly Agree).

The second item was negatively-worded, so we reversely recorded the responses to this item.

#### Volunteers (waves: 1, 4-15)

Participants were asked,"Please estimate how many hours you spent doing each of the following things last week" and responded to an item ("voluntary/charitable work"), from [@sibley2011].



{{< pagebreak >}}





## APPENDIX B. Multiple comparisons in outcomewide studies

The concern for multiple comparisons is legitimate in many research settings. However, there are compelling reasons not to adjust for it in the case of outcome-wide science, as proposed by Tyler VanderWeele [@vanderweele2020].

1.  **Nature of the analysis:** Outcome-wide studies are inherently exploratory. They aim to generate hypotheses rather than testing pre-specified ones. In such a scenario, adjusting for multiple comparisons is out of place. Such testing might limit our ability to discover.

2.  **False negatives vs. false positives:** Adjusting for multiple comparisons often results in an increased risk of Type II errors (false negatives). In the context of public health, false negatives could be more problematic than false positives. We might overlook potentially significant associations that could lead to beneficial interventions.

3.  **Independence of outcomes:** The standard corrections for multiple comparisons, such as the Bonferroni or the Holm method, assume that tests are independent. In an outcome-wide study, outcomes are likely to be correlated, so these corrections could be overly conservative.

4.  **Magnitude of effects:** Outcome-wide studies do not only focus on p-values, but also the magnitude of effects, confidence intervals, and their scientific or clinical significance. We advocate assessing E-values, or unmeasured confounding, in place of assessing p-values. Adjusting for multiple comparisons focuses primarily on p-values, potentially undermining the importance of effect sizes. P-values are often a measure of sample size.

5.  **Replication and robustness:** Findings from outcome-wide studies are not intended to be conclusive, but rather to guide further research. Consequently, potential false positives should be addressed in future replication studies and through robustness checks.

In short, controlling for multiple comparisons makes sense in many research settings. However, for outcome-wide studies, it may limit the capacity to generate new hypotheses, increase the risk of missing potential public health interventions, and over-emphasize p-values at the expense of sensitivity analyses and E-values. In causal inference, the main worry is assessing the robustness of results to unmeasured confounding.

## References
