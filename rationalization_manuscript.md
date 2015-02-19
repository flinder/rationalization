% Misreporting of ideological self placement to rationalize party preferences 
% Fridolin Linder[^contact]

[^contact]: Fridolin Linder is a Ph.D. student in political science at Pennsylvania State University ([fridolin.linder@gmail.com](mailto:fridolin.linder@gmail.com)).

\begin{abstract}

\end{abstract}

## Introduction

Classical theories of rational or economic voting assume that
individuals' candidate or party preferences are a function of the
distance between their ideal point and the candidate's ideal point in an
issue space [@downs1957economic; @enelow1989general]. However, research suggests that
many other factors such as party identification [@campbell1960american] and
other heuristics to simplify the complex political reality [@lau2006voters],
personality traits of candidates [@huddy1993gender; @king2002leaders] or non-verbal
cues like physical appearance, voice frequency, or charisma
[@olivola2010elected for an overview] influence vote choice.

Empirical support for issue based proximity preferences often relies on
high correlations between issue proximity and vote choice in
observational survey studies. Several authors have argued that these
correlations are in part due to projection or rationalization:
preferences for candidates are a causal precedent to the issue positions
[@campbell1983candidate; @rahn1994rationalization; @lenz2009learning]. In analyses of mostly panel
data, those studies found a reciprocal causal relationship between issue
proximity and candidate evaluation. 

This relationship can be seen as a form of rationalization to decrease
cognitive dissonance. Issue proximity is a reasonable logical criterion
to use when deciding which candidate to support. As research in social
psychology shows, people prefer to view themselves as logical and
coherent once they are forced to reflect on their preferences (Wilson
1989). If contemplation about the candidate leads to a seemingly
incoherent preference (i.e. the preference for a candidate is based on
different criteria then the issue proximity), cognitive dissonance
arises [@festinger1957prophecy; @brehm1962explorations]. To mitigate this dissonance,
individuals might rationalize their candidate preference by changing
their own position or their perception of the candidate's issue
position.

I propose a design consisting of two experiments to test whether this
process of rationalization occurs. I find strong support for the hypothesized effect.

## Theoretical Background

## Study Design

### Research Design

I define the individual’s true position ($S$) as
the position the respondent would report when asked to place only
herself on the ideological dimension and the party's true position ($P$)
as the position the respondent would assign if asked to rate only the
party. I denote the reported positions as $\hat{S}$ and $\hat{P}$ for
individual and party positions respectively. Figure \ref{fig:DGP} displays
graphically the two scenarios how rationalization can occur. If the
question for $S$ is asked first and the question for $P$ is asked
second, $\hat{S}$ should equal $S$ (on average) and $\hat{P}$ should be closer to $S$
than $P$. 

![Illustration of the proposed Data Generating Mechanism: S 'True' self position, C 'True' perceived candidate position, ^ S reported self position, ^ C reported candidate position.
In red the convergence bias.\label{fig:DGP}](figures/research_design_figure.png)

If the order of the question is reversed, $\hat{P}$ should
equal $P$ and $\hat{S}$ should be biased towards $P$. In other words,
depending on what question they are asked first, I expect that subject’s
bias their second answer in order to decrease the distance between
themselves and the preferred party. Each experiment is designed to
estimate one of these two forms of rationalization bias. In the following sections I will 
describe the two experiments in more detail. 

*Experiment 1, Bias in $\hat{S}$*: 
The goal of this experiment is to obtain an estimate of the bias that is introduced into the measurement of ideological self placement as displayed in scenario 2 in Figure \ref{fig:DGP}. In the 'control group' the question for $S$ is asked first, therefore the answer is unbiased, according to the definition above[^assumptions]. In the 'treatment group' the question for $P$ is asked first. According to the theory above, I expect that respondents $i = 1, ... , n_{t}$ in the treatment group will bias their $\hat{S}_i$ towards their $\hat{P}_i$. A straight forward test would be to compare the average $\bar{\hat{S}}$ in the two groups and take the difference as the treatment effect. However, since we don't know a priori, if $P_i$ lies to the left or right of $S_i$, and it might lie on different sides for different respondents, the treatment effect might be lost when just taking the average. 

To avoid this problem, I will estimate the unbiased $\hat{S}$ in the treatment group from observables. Additionally to the placement questions, all respondents are asked for their opinions on a set of recent political issue questions. Since $\hat{S}$ is unbiased in the control group, this data can be used to train a predictive model, that then can be used to estimate the 'true' self placement in the treatment group. I will denote this estimate by $S^*$. The details on the model used for prediction are described in Section [REFERENCE TO SECTION: PREDICTIVE MODEL]. 

With this estimate of the true $S$, the treatment effect can be estimated by comparing the average distance between the prediction and the reported position in the treatment and control group. Since I'm only interested larger differences in direction of the preferred party's position $P$ the outcome of interest is the deviation in direction of $P$. This measure is calculated by first shifting each respondents scores, such that $S^*$ is the zero point of the scale and multiplying this distance by $-1$ if $\hat{S}$ and $\hat{P}$ are on opposite sides of $S^*$ and by $1$ if they are on the same side. More formally, the signed distance for each respondent $i$ is:

$$ D(S^*_i,\hat{S}_i, \hat{P}_i) = (\hat{S}_i - S^*_i) * sng(\hat{P}_i - S^*_i) $$.

Where $sng(.)$ is the sign function. I define the treatment effect as:

$$ T_1 = \frac{\bar{X}}{\bar{Y}} $$.

Where,

$$ \bar{X} = \frac{1}{n_c}\sum_{i \in \mathcal{C}}D(S^*_i,\hat{S}_i, \hat{P}_i) $$
$$ \bar{Y} = \frac{1}{n_t}\sum_{i \in \mathcal{T}}D(S^*_i,\hat{S}_i, \hat{P}_i) $$.

Where $\mathcal{T}$ is the set of all $i$ that are in the treatment group, $\mathcal{C}$ is the set of all $i$ in the control group, $n_t = |\mathcal{T}|$ and $n_c = |\mathcal{C}|$. I express $T$ as the ratio of means rather then the difference in means to get an easily interpretable estimate that does not depend on the unit of the ideological scale. The hypothesis for Experiment 1 is then,

$$ H_1: T_1 < 1 $$

because $Y$ is expected to be larger, because the treatment effect 'pulls away' $\hat{S}$ from $S$ therefore $Y$ is larger than $X$.

[^assumptions]: This is, assuming that the questions about issue positions asked before the placement questions do not influence the reported position.

*Experiment 2, Bias in $\hat{P}$*:
The goal of the second experiment is to obtain an estimate of the bias in the party placement due to rationalization described in scenario 1 in Figure \ref{fig:DGP}. Respondents in the control group are asked to report $P$ first and $S$ second, for respondents in the treatment group the order is reversed. I assume that $\hat{P}$ in the control group is an unbiased estimate (as defined above) of $P$. According to the theory, respondents in the treatment group will bias $\hat{P}$ towards $S$. When trying to obtain an estimate of the treatment effect, the same problem as in Experiment 1 arises: since $P_i$ and $S_i$ can be arbitrarily ordered for each respondent, the treatment effect might be lost when averaging over the $\hat{P}_i$. My solution is the same as above. Because, $\hat{P}$ and $S^*$ are unbiased in the control group, the distance between them should be larger than the distance between $\hat{P}$ and $\hat{S}$ in the treatment group[^robustness]. I therefore define the treatment effect for Experiment 2 as:

$$ T_2 = \frac{\bar{Z}}{\bar{W}} $$

Where,

$$ \bar{Z} = \frac{1}{n_t}\sum_{i \in \mathcal{T}}(\hat{P}_i - \hat{S}_i)^2 $$
$$ \bar{W} = \frac{1}{n_c}\sum_{i \in \mathcal{C}}(\hat{P}_i - S^*_i)^2 $$

Where $\mathcal{T}$ is the set of all $i$ that are in the treatment group, $\mathcal{C}$ is the set of all $i$ in the control group, $n_t = |\mathcal{T}|$ and $n_c = |\mathcal{C}|$. The hypothesis for Experiment 2 is then:

$$ H_2: T_2 < 1 $$ 

[^robustness]: Since$\hat{S} = S^*$, on average, in the treatment group comparison could be made to the distance between $\hat{P}$ and $S^*$, see the appendix for this robustness check. 

### Preregistration

The study has been preregistered. The preregistration is in form of a frozen branch of the github repository that contains all work related to this project. It can be viewed under <https://github.com/flinder/rationalization/tree/pre_registration> It contains all public files pertaining to the study, before the main data collection began. Although not all details have been fixed in the pre-registration, the general research design, variables of interest and statistics are the same. [Justify all deviations here]

- different outcome measure for experiment 1
- bayesian framework for the whole thing
- different test for experiment 2 (gamma distribution)


In preparation for the main data collection, I conducted a test run of
the experiment with 50 participants. Mostly to test the procedures to connect Qualtrics and MTurk.

### Data Collection and Description

- Sample size - power analysis
- Explain that both groups are experimental/treatment group depending on experiments (introduce group numbering)

-----------------------------------------------------------------------------------
           &nbsp;              Experiment 1 (Bias in S)   Experiment 2 (Bias in P) 
----------------------------- -------------------------- --------------------------
Group 1 (S asked first)                Control             		 Treatment             

Group 2 (P asked first)               Treatment                   Control             
-----------------------------------------------------------------------------------

The data was collected using Amazon's Mechanical Turk service (MTurk). Several studies showed that MTurk provides data of reasonable quality for research purposes [@mason2012conducting; @berinsky2012evaluating; @buhrmester2011amazon]. MTurk workers are redirected to a Qualtrics survey to complete the experiment. The specific survey questions are available on github[^surveys].

[^surveys]:<https://github.com/flinder/rationalization/tree/master/surveys>

![Balance statistics for and preference distributions in the sample. \label{fig:bal}](figures/main/randomization.png)

Figure \ref{fig:bal} displays balance statistics for a sample of variables, as well as the distribution of party preferences in the sample. It is apparent, that the sample is very scewed towards democratic and alternative parties. More information on the survey can be found in Appendix [REFERENCE TO SURVEY APPENDIX]

### Analysis and Statistical Model

Figure \ref{fig:d_dist} displays the distribution of the distances in both groups and both experiments. The panels correspond to the experiment and the colored lines to the experimental groups. The density lines correspond to the random variables defined in Section [RESEARCH DESIGN]. 

![Distribution of distances. Distances are defined as described in the research design.\label{fig:d_dist}](figures/main/dist_dista_dens.png)

The blue line in the left panel corresponds to the control group in Experiment 1 (X) which was asked to report $S$ first. The green line corresponds to the treatment group (Y). In the right panel the blue line corresponds to the treatment group in Experiment 2 (Z), while the green line displays the density for the control group ($W$). It is obvious that the distributions in the two experiments have to be modelled with different distributions[^transforamtion]. I conduct all statistical analyses in a Bayesian framework. 

For Experiment 1 I follow @kruschke2013bayesian and model the data with a t-distribution:

$$ X \sim T(\nu, \mu_1, \sigma_1) $$
$$ Y \sim T(\nu, \mu_2, \sigma_2) $$

Where $\nu$ is the parameter for the degrees of freedom (Kruschke refers to it as the normality parameter, the higher $\nu$ the closer is the distribution to a normal distribution). I use flat priors since there is no prior research that would give me information on the relevant distribution. I follow @kruschke2013bayesian in the choice of priors[^sensitivity]:

$$ \nu \sim exp(29^{-1}) $$,
$$ \mu_i \sim N(0, 100) $$, 
$$ \sigma_i \sim cauchy(0, 5)I(0, +\inf) $$

Where $i = 1, 2$ $exp$ is the exponential distribution in scale parametrization, $N$ is the normal distribution, and $cauchy(0, 5)I(0, +\inf)$ is the cauchy distribution, left-truncated at 0 (I refer the reader to @kruschke2013bayesian for a more thorough discussion of these choices).

For Experiment 2 I model the data using gamma distributions:

$$ Z \sim gamma(\alpha_1, \beta_1) $$
$$ W \sim gamma(\alpha_2, \beta_2) $$
 
$\alpha$ and $\beta$ are the shape and rate parameters respectively. I will interpret results later in terms of the expectation of the distribution $\theta = \frac{\alpha}{\beta}$. The following non-informative priors are used:

$$ \alpha_i \sim gamma(1, 0.5)$$,
$$ \beta_i \sim gamma(1, 0.5)$$,

for $i = 1,2$.

[^sensitivity]: The sensitivity of the results to the prior specification is reported in Appendix [REFERENCE].




[^transforamtion]: A logarithmic transformation of the data in Experiment 2 still produces non-normal data. Additionally to the less intuitive interpretation and the problem of handling the many zeros. See Appendix [REFERENCE] for the density of the transformations.

## Results

### Predictive Model

![Predicted self positions (out-of-bag for Group 1) from the random forest against observed values by experimental group. \label{fig:pred}](figures/main/prediction.png)

### Model Fit

![Posterior predictive check for assumed model. The red represents the density estimate of the observed data. The black lines are 20 replications from the assumed model using the estimated parameters. \label{fig:post_pred}](figures/main/post_pred.png)

### Hypotheses

Estimated probability that there is a treatment effect:

Estimated probability that there is a substantively interesting (depends of course, I say at least 10\%) treatment effect:

![Posterior distribution of the difference in means and the ratio of means in both experiments. \label{fig:post_pred}](figures/main/mean_diff_ratio.png)

\input{res_table.tex}

## Appendix

### Prior Sensitivity

\input{res_r_table.tex}

### MCMC Diagnostics

![Traceplots for parameters in Experiment 1. \label{fig:trace_1}](figures/main/trace_1.png)

![Traceplots for parameters in Experiment 2. \label{fig:trace_1}](figures/main/trace_2.png)

### Additional descriptive plots

![Distribution of self and candidate placement by preferred party. \label{fig:d_pos}](figures/main/dist_party.png)

![Distribution of self and candidate placement by preferred party. \label{fig:d_pos}](figures/main/dist_self.png)

![Distribution of distances. Distances are defined as described in the research design.\label{fig:d_dist_box}](figures/main/dist_dista_box.png)

![Distribution of logarithmic transformation of distances for Experiment 2. The following transformations have been applies: $Z_t = ln(Z + 1)$ (Blue line) and $W_t = ln(W + 1)$ (Green line)\label{fig:d_dist_log}](figures/main/dist_dista_box.png)

### Frequentist Parametric Tests

### Non-parametric Tests