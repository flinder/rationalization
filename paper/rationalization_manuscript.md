% Misreporting of Ideological Placement to Rationalize Party Preferences[^conf]
% Fridolin Linder[^contact]

[^conf]: Prepared for the 73rd annual MPSA conference, April 16-19, 2015.
[^contact]: Fridolin Linder is a Ph.D. student in political science at Pennsylvania State University ([fridolin.linder@gmail.com](mailto:fridolin.linder@gmail.com)). All supplementary information, data and analysis scripts can be foud at <https://github.com/flinder/rationalization>. 

\begin{abstract}
The literatures on source cues and party identification have delivered considerable evidence, that citizens' positions on issues, values, and evaluations of policy are influenced by the positions political elites hold on them and the affect the individual has towards these elites. These effects have been mostly attributed to the heuristic nature in which most citizens process political information. In this study, I hypothesize, that they are in part also due to consistency bias. I argue that due to the multidimensional nature of the space in which citizens' decisions on parties and candidates take place, when asked to evaluate a parties on only one dimension, discrepancies between self position and reported party position have to be revealed. If the subject has positive affect towards this party, cognitive dissonance arises, which can be mitigated by missreporting either the own or the party's position. In two experiments I find evidence, that citizens do bias their own and their preferred party's position towards each other, when asked to report them together.
\end{abstract}

\clearpage

# Introduction

The connection between individuals' political preferences and individuals' group or party preferences has gained enormous attention from scholars in political science and other social
sciences. It is one basic question that, due to it's high normative relevance, is debated for decades now in political science: Do people have exogenous preferences and form elite/group preferences accordingly or do individuals change their political preferences to match their group identities.

Political scientists have accumulated considerable evidence that elite positions influence individual preferences. The literature on source cues has produced overwhelming experimental evidence of such effects and the conditions  under which they work [@goren2009source; @nicholson2012polarizing; @lau2006voters; @mondak1993source; @mondak1993public; @rahn1993role; @kam2005toes; @arceneaux2008can; @boudreau2009closing]. Other studies have focused on group effects in issue voting and reporting of ideology with large scale survey and panel studies [@achen2006feels; @page1972policy; @bartels2002beyond; @lenz2009learning; @rahn1994rationalization; @brady1985attitude].

Most theoretical explanations of these effects are based on some form of the systematic - heuristic cognitive processing model [@chen1999heuristic]. If an individual doesn't have information about an issue, the issue is very complicated or cognitive resources are to be saved, source cues serve as a heuristic to determine the own position on an issue from the affect towards the cue and its position. Other explanations draw on group identity theory [@nicholson2012polarizing] or differing factual attribution and moral criteria [@cohen2003party]. However, little attention has been focused on differentiating between these theoretical mechanisms. In most of the previous research, the subjects where given information on the position of a party and a change in opinion was detected. It is not clear however, how much of this effect is due to the information and therefore real opinion change, and how much is due to other biases, for example the need of an individual to be consistent or negative feelings evoked by supporting an issue that is supported by a disliked party or not supporting an issue of a preferred party (independent of the issue itself).

In this study I concentrate on consistency bias that is introduced, so I hypothesize, through cognitive dissonance that is invoked by the process of having to report opinions that differ from the stance of the preferred party or are close to a disliked party. I argue that due to the multidimensionality of the space in which political choices take place and due to the small number of options a citizen can choose from, there will almost always be discrepancies between a hypothetical personal ideal point and the perceived position of the preferred party. When asked to report opinions on an issue, and supplied with information about the party's stance on that issue (or asked to report the perceived stance of the party on that issue first) these discrepancies have to be revealed and openly admitted. Since individuals have a need to appear consistent [@cialdini1995preference] this fact will produce cognitive dissonance.

I hypothesize, that in order to reduce this dissonance, subjects will try to move either their own position or (if no external information on the party stance is provided) the position they report for the party, depending on their affect towards the party.

Using an experimental design, I will isolate and estimate the magnitude of the consistency bias that is evoked when asking subjects to state their party preference, that party's ideological position and their own ideological position on a liberal conservative scale. This design allows to estimate the consistency effect in isolation because no external information is provided. With the use of a machine learning algorithm to predict individual level self reported ideology from political issue questions I am able to estimate average individual biases in order to quantify the magnitude of the consistency bias.

My findings make two major contributions. First, they shed light on the different mechanisms that can lead an individual to change her position towards her preferred party. More precisely they hint at and estimate the size of consistency bias. And second, they show the potential and magnitude of bias in the measurement of self reported ideology and perceived party ideology when measured in conjunction with party preferences.

# Source Cues and Information Effects

Political science research has focused much attention on the formation, expression and effects of political attitudes. In classical rational choice theory preferences are conceptualized as exogenous caused of choice, and the space in which preferences exist is an issue space [@downs1957economic; @enelow1989general]. Especially in the field of political psychology the understanding of preferences has been extended in two ways. First, the definition of the relevant space has been extended. It has been found that in relation to choice more dimensions are relevant than just the "rational" issue dimensions. Research suggests that many other factors such as party identification [@campbell1960american] and other heuristics to simplify the complex political reality [@lau2006voters], personality traits of candidates [@huddy1993gender; @king2002leaders] or non-verbal cues like physical appearance, voice frequency, or charisma [@olivola2010elected for an overview] influence preference or vote choice. Second, the theoretical "degrees of freedom" have been extended. In the classical rational chioce setting both the individual preferences and the position of a candidate or party are fixed, the only free parameter is the choice (which is assumed to be found by choosing the candidate or party that minimizes that distance). In the more psychological literature, the individuals position in the relevant space is not fixed, but can be a function of other factors. 

A well established example of this comes from the literature on source cues [e.g. @nicholson2012polarizing; @goren2009source; @cohen2003party]. Individuals, when asked about their opinion on an issue report differing preferences when presented with information about what political actor holds this preference. In most of the experiments, individuals in the control group are presented with the 'neutral' issue and are asked to report their opinion or position on this issue. Subjects in the control group are presented with a cue, for example, 'Democrats think that ... ', followed by the issue. It has been consistently found that these cues influence, what subjects report to think about the issue. The direction of the effect, that is does a cue by a certain group make the subject agree more or less, depends on the affect towards the cue giver. A democrat, for example, would be more likely to agree to the issue if it contains the cue that Democrats hold that opinion, while she would be less likely to agree if a republican cue was given. 

It has been consistently found, that individuals with higher levels of political information are less likely to rely on source cues when reporting their positions, and that the effect of source cues is stronger on issues that are complex or not well known [@kam2005toes; @lupia1994shortcuts; @mondak1993public; @arceneaux2009educating]. While the empirical evidence for an effect of source cues on expressed opinion is unanbiguous, several theoretical explanations have been proposed for it. The fact that source cues show their strongest effect in low information groups and on new and unestablished issues, is often interpreted as evidence for heuristic processing [@lau2006voters; @chaiken1980heuristic, @lenz2009learning]. Most individuals are not very involved in politics and do not want to spend coginitive ressources to systematically process political information about parties, candidates and issues. Party cues offer a shortcut, where the subject simply relies on her affect towards the source cue to make up her mind about the issue, assuming that the own preferences are aligned with prefered groups and disalignes with disliked groups [@lau2001advantages].

However, even on well known and established issues and among politically informed subjects source cue effets have been shown [@nicholson2012polarizing; @lau2001advantages]. @nicholson2012polarizing proposes group identity theory as an explanation. Individuals want a positive in-group identity while keeping the own in group distinct from others. So just the fact that an issue or position is held by the in-group evokes more positive feelings for it and therefore produces biases.

In another explanation for the effect of source cues, the perception of the issue itself is influenced by the group membership. The argument is, that most political issues do not have inherent objective value to the subject, but derive their value to the subject from their social meaning instead. The process of defining social meaning is crucially influenced by group membership [@cohen2003party; @robinson1995actual; @verplanken2002motivated]. It is assumed that other members of the same social group share similar values, which again influences how the social menaing of the issue is defined. Source cues affect this definitional process in two ways: First, they can influence the factual attriburtions made to the issue. An example might be that if presented with the fact that republicans want to increase taxes, a democratic subjet might connect this with increased military spending, so the issue would be opposed. On the other hand if a democratic cue was given, the subject might attribute, social welfare to the issue and judge it in a positive light. Second, depending on the source cue, different moral dimensions can be activated to make a judgement. For instance, in the case of abortion, either the sanctity of life, or the rights of the mother can be invoked to justify either position towards the issue. 
However, in this theoretical explanation information plays an important role, too. The possibility for a subject to change the factual attribution or different moral dimensions depends on her information on the issue. The effect of a source cue is probably much lower for a very old and well known issue compared to a new or complicated issue.

## Consistency Bias

An explanation that has received much less attention and which is theoretically unrelated to the information processing model, is the consistency effect. In the this section I lay out a theoretical framework that explains bias in reporting of political positions, not for the reasons mentioned in the previous section but for the fact that reporting positions in only one dimension forces the subject to admit necessary discrepancies between the own position and the preferred party's position. These discrepancies cause cognitive dissonance, which the subject tries to reduce by biasing her reported position.

For each individual, preferences for candidates or parties or other political elites (henceforth just parties) can be conceptualized in a $D$ dimensional space, where $D$ can vary between individuals. As discussed above, these dimensions could represent anything that is relevant in the subject's evaluation of parties. It might be positions on concrete issues, personal characteristics of candidates of that party, or might be conceptualized as aggregated ideological dimensions like economic left - right or liberal - conservative. Let the citizen have position $\mathbf{S} = [S_1, S_2, ... ,S_D]$, and each of the parties, $j = 1, 2, ..., K$ is perceived by the citizen to have position $\mathbf{P_j} = [P_{j1}, P_{j2}, ..., P_{jK}]$. If the space is exhaustive of all dimensions that are relevant for the citizen, then the preferred party is $p^* = \operatorname{argmin}_p ||\mathbf{S} - \mathbf{P_p}||$. However, the solution to this multidimensional problem is not necessaruly the optimum in each dimension. In other words the party closest to the position of the citizen in the $D$ - dimensional space, is not necessarily the closest party on each relevant dimension. This is illustrated in two dimensions in Figure \ref{fig:theo}. Party A is the closest, considering all dimensions together (the distance vector between the points is shorther), but party B is closer when considering the projections onto the x - axis. Furthermore, due to the discrete nature of the optimization, the distance between the ideal point of the citizen and the ideal point of the party will almost never be zero in any single dimension even if the preferred party is closest to the subject's ideal point.

\input{theory_figure.tex}

If a researcher wants to measure the position of a subject in a specific dimension, her perception of the positions of political parties on that dimension and the general party preference, the subject is forced to report these discrepancies. Research in social psychology showed that people have a need to appear consistent [e.g. @cialdini1995preference]. I therefore expect that depending on its size, this discrepancy will cause cognitive dissonance for the individual [@festinger1957prophecy; @brehm1962explorations]. 

There are several different possibilities to react to this dissonance. First, the subject might be aware of the multidimensionality and therefore accept the discrepancy in a single dimension. In this case no consistency bias would be expected. Second, if party different from the preferred party appears to be closer to her own ideal point, the subject might change her expressed preference to decrease the distance. However, there might still be a discrepancy between the new party and the ideal point. Third, the subject can move their own reported position towards the perceived party position. Or fourth, the reported perceived party position can be moved towards the individual ideal point. In the latter two cases, the individual rationalizes the party preference by reporting the preferred party's perceived position and the own ideal point to be closer together than the individual would report if not all the questions are asked together [@rahn1994rationalization]. 
In this paper I will analyze the latter two options. I choose a general liberal - conservative scale as the dimension of interest (the dimension, subjects will be asked to evaluate themselfs and their preferred party on). The liberal - conservative scale is still widely used in political science as a general measure of ideology, it is therefore of special interest, to check for potential biases occuring in the measurement. I furthermore expect, that most respondents have an idea of where to locate themselfs and their preferred party on this scale. 

If the party preference and either the own or the preferred party's position is fixed, the other position should be changed to mitigate dissonance if a consistency bias exists. Since I analyze consistency bias here, I define the unbiased position to be the position the subject would report without bias introduced through consistency effects as described above. This does not mean the unbiased position is a measurement of the 'true' ideological position. There are several other biases an problems with the measurement and conceptualization of self reported ideology (see for example the literature on anchoring [@king2007comparing]). 
 
The occurence of such a consistency bias has implication for our theoretical understanding of the effects of party preferrence on issue preferences (in particular for the source cues literature) as well as for the measurement of self reported ideology and perceived party positions. The implications for the theoretical understanding of source cue effects are threefold: First, the consistency bias shown in this work is solely stemming from the measurement of party preference, self and party position. This would imply, that the mesurement procedure of the source cue literature alone might invoke a consistency bias that explains part of the found effects. Second, the result would hint at consitency bias as another potential mechanism that might explain part of the source cue effects, in a theoretical sense, hence separate from the effect that is invoked by the measurement. In other words, the existence of the measurement effect, demonstrates the basic psychological mechanism that drives subjects to move their own position or preference towards a party exist. However the experiment in this study gives no direct evidence for source cue effects 'in the real world', i.e. not just in the survey. Third, source cue effects, in all studies that I am aware of, just concentrate on the biases in the *subjects'* reported positions, while party positions are fixed by the researcher. The theory laid out here, however, would suggest that cognitive biases also operate on the perceptions of parties if the self position is fixed or sufficiently constrained. 

# Research Design

I use an experimental design to evaluate the existence of consistency bias in the measurement of ideology in surveys. In order to test if subjects decrease dissonance by biasing either their self reported position or the perceived party position on a liberal - conservative scale, I fix their party preference and one of the two reported positions. I achieve this by asking the subjects to answer the questions for the measures that are to be fixed first. Since I want to asses the bias in reported self position and reported party position the party preference is allways asked first. This means I assume that reporting of the party preference does not influence (in the sense of consistency bias) the reported self or party position by itself. I argue that this assumption is justified because the discrepancy is only revealed after the second position is asked, too.  After the pary preference has been assesed, individuals are asked to place themselfs and their preferred party on a continuous liberal - conservative scale, where the order depends on which position is to be fixed. 

I define the individual's unbiased position ($S_i$) as the position that respondent $i$ would report when asked to place only herself on the ideological dimension and the party's unbiased position ($P_i$; as perceived by respondent $i$) as the position the respondent would assign if asked to rate only the party. Note that this does not mean that I assume these positions are unbiased in the sense of the 'true' ideology value. Unbiased in this design means just free of consistency bias introduced through the mechanisms described in the theory section. I denote the reported positions as $\hat{S}_i$ and $\hat{P}_i$ for individual and party positions respectively. Figure \ref{fig:DGP} displays graphically the two scenarios how consistency bias in one dimension can occur. If the question for $S$ is asked first and the question for $P$ is asked second, $\hat{S}$ should equal $S$ and $\hat{P}$ should be closer to $S$ than $P$. 

\input{research_design_figure.tex}

If the order of the question is reversed, $\hat{P}$ should equal $P$ and $\hat{S}$ should be biased towards $P$. In other words, depending on what question they are asked first, I expect that subjects bias their second answer in order to decrease the distance between themselves and the preferred party. I conduct two experiments, each is designed to estimate one of these two forms of consistency bias.

## Experiment 1, Bias in $\hat{S}$

The goal of this experiment is to obtain an estimate of the bias that is introduced into the measurement of ideological self placement as displayed in scenario 2 in Figure \ref{fig:DGP}. In the 'control group' the question for $S$ is asked first, therefore the answer is unbiased according to the definition above. In the 'treatment group' the question for $P$ is asked first. I expect that respondents in the treatment group will bias their $\hat{S}_i$ towards their $\hat{P}_i$. A straight forward test would be to compare the average $\bar{\hat{S}}$ in the two groups and take the difference as the treatment effect. However, since we don't know a priori, if $P_i$ lies to the left or right of $S_i$, and it might lie on different sides for different respondents, the treatment effect might be lost when just taking the average. 

To avoid this problem, I will estimate the unbiased $\hat{S}$ in the treatment group from observables: Additionally to the placement questions, all respondents are asked for their opinions on a set of recent political issue questions. Since $\hat{S}$ is unbiased in the control group, this data can be used to train a predictive model, that then can be used to estimate the unbiased self placement in the treatment group. I will denote this estimate by $S^*$ (see the section on the predictive model for more details). 

With this estimate of the true $S$, the treatment effect can be estimated by comparing the average distance between the prediction and the reported position in the treatment and control group. Since I am only interested in differences in direction of the preferred party's position $P$, the outcome of interest is the distance between $\hat{S}$ and $S^*$ in direction of $P$:

\begin{equation}
  \label{eq:s_dist}
  D(S^*_i,\hat{S}_i, \hat{P}_i) = (\hat{S}_i - S^*_i) \operatorname{sgn}(\hat{P}_i - S^*_i). 
\end{equation}

Where $\operatorname{sgn}(.)$ is the sign function. I define the treatment effect as:

\begin{equation}
  \label{eq:t_1}
  T_1 = \frac{\bar{X}}{\bar{Y}}.
\end{equation}

Where,
\begin{equation}
  \label{eq:x_y}
  \begin{split}
     \bar{X} = \frac{1}{n_c}\sum_{i \in \mathcal{C}}D(S^*_i,\hat{S}_i, \hat{P}_i) \\
     \bar{Y} = \frac{1}{n_t}\sum_{i \in \mathcal{T}}D(S^*_i,\hat{S}_i, \hat{P}_i). 
  \end{split}
\end{equation}

Where $\mathcal{T}$ is the set of all $i$ that are in the treatment group, $\mathcal{C}$ is the set of all $i$ in the control group, $n_t = |\mathcal{T}|$ and $n_c = |\mathcal{C}|$. I express $T$ as the ratio of means rather then the difference in means to get an easily interpretable estimate that does not depend on the unit of the ideological scale. The hypothesis for Experiment 1 is then: $H_1: T_1 < 1$.

## Experiment 2, Bias in $\hat{P}$

The goal of the second experiment is to obtain an estimate of the bias in the party placement due to rationalization described in Scenario 1 in Figure \ref{fig:DGP}. Respondents in the control group are asked to report $P$ first and $S$ second, for respondents in the treatment group the order is reversed. I assume that $\hat{P}$ in the control group is an unbiased estimate (as defined above) of $P$. According to the theory, respondents in the treatment group will bias $\hat{P}$ towards $S$. When trying to obtain an estimate of the treatment effect, the same problem as in Experiment 1 arises: since $P_i$ and $S_i$ can be arbitrarily ordered for each respondent, the treatment effect might be lost when averaging over the $\hat{P}_i$. My solution is the same as above. Because, $\hat{P}$ and $S^*$ are unbiased in the control group, the distance between them should be larger than the distance between $\hat{P}$ and $\hat{S}$ in the treatment group. I therefore define the treatment effect for Experiment 2 as:

\begin{equation}
  \label{eq:t_2}
  T_2 = \frac{\bar{Z}}{\bar{W}}.
\end{equation}

Where,
\begin{equation}
  \label{eq:z_w}
  \begin{split}
      \bar{Z} = \frac{1}{n_t}\sum_{i \in \mathcal{T}}(\hat{P}_i - \hat{S}_i)^2 \\
      \bar{W} = \frac{1}{n_c}\sum_{i \in \mathcal{C}}(\hat{P}_i - S^*_i)^2.
  \end{split}
\end{equation}

Where $\mathcal{T}$ is the set of all $i$ that are in the treatment group, $\mathcal{C}$ is the set of all $i$ in the control group, $n_t = |\mathcal{T}|$ and $n_c = |\mathcal{C}|$. The hypothesis for Experiment 2 is then: $H_2: T_2 < 1$. 

## Data Collection and Description

The sample size was determined with a simulation approach (see the online appendix for details). It was determined that at least 250 respondents per group are necessary to detect a substantively significant effect size (at least 10% reduction in distance between the reported positions). 
The data for both experiments is collected in only two groups as laid out in Table 1. In Experiment 1 $S$ is asked first in the control group and $P$ is asked first in the treatment group. In Experiment 2 the order is reversed. Therefore, I collect the data only in two 'survey groups' and use them as treatment and control according to Table 1. I label these survey groups as Group 1 (where $S$ is asked first) and Group 2 (where $P$ is asked first). Because $\hat{S}$ in Group 1 is unbiased, the data collected in this group is also used to train the predictive model.

-----------------------------------------------------------------------------------
           &nbsp;              Experiment 1 (Bias in S)   Experiment 2 (Bias in P) 
----------------------------- -------------------------- --------------------------
Group 1 (S asked first)                Control             		 Treatment             

Group 2 (P asked first)               Treatment                   Control             
-----------------------------------------------------------------------------------
Table: Relation of survey groups to experimental groups in Experiments 1 and 2.

The data was collected using Amazon's Mechanical Turk (MTurk)[^preregistration]. Several studies showed that MTurk provides data of reasonable quality for research purposes [@mason2012conducting; @berinsky2012evaluating; @buhrmester2011amazon]. MTurk workers are redirected to a Qualtrics survey to complete the experiment. The specific survey questions are available online[^surveys]. Respondents were given equal probability to fall into Group 1 or Group 2. Respondents were required to be older than 18 years and to live in US (some workers had US accounts but foreign IP addresses, those users where excluded). Furthermore, respondents that reported to have no party preference were excluded. The final sample used for analysis consists of 508 respondents (Group 1: 251, Group 2: 257), the mean age is 33.4 years (33.7, 33.3) and 64.5% of the respondents are male (64.1, 65.0). Figure \ref{fig:pref} displays the distribution of party preferences (before respondents without preference have been excluded). It is apparent that the sample is scewed towards the democrats and other parties and away from republicans. More description of the sample and information on the balance of the groups can be found in the Appendix (Figures \ref{fig:time}, \ref{fig:map} and \ref{fig:bal}).

In order to assure good quality of the data I included two attention checks throughout the survey [@berinsky2012evaluating]. Less then 5% of the respondent failed to pass the attention checks. I furthermore tracked the time spent on each question. Overall I conclude that most respondents took the task seriously and answerd the questions carefully.  

[^preregistration]: The study has been preregistered. The preregistration is in form of a frozen branch of the github repository that contains all work related to this project. It can be viewed under <https://github.com/flinder/rationalization/tree/pre_registration> It contains all public files pertaining to the study, before the main data collection began. Although not all details have been fixed in the pre-registration, the general hypotheses and the research design are unchanged. Due to mistakes in the pre registration I had to change the measure of distance in Experiment 1. I also conducted the analyses in a bayesian instead of a frequentist framework.

[^surveys]: <https://github.com/flinder/rationalization/tree/master/surveys>

![Distribution of party preferences.\label{fig:pref}](../figures/main/preferences.png)

## Analysis and Statistical Model

Figure \ref{fig:d_dist} displays the distribution of the outcome variables of interest in both groups and both experiments. The panels correspond to the experiment and the colored lines to the experimental groups. The density lines correspond to the random variables defined in the research design section. 

![Distribution of outcome variables for the experiments. For Experiment 1 (left panel) the outcome is the distance between $\hat{S}$ and $S^*$ (Equation \ref{eq:s_dist}). For Experiment 2 (right panel) the outcome is the squared distance between $S^*$ or $\hat{S}$ and $\hat{P}$.  \label{fig:d_dist}](../figures/main/dist_dista_dens.png)

The blue line in the left panel corresponds to the control group in Experiment 1 (X) which was asked to report $S$ first. The green line corresponds to the treatment group (Y). In the right panel the blue line corresponds to the treatment group in Experiment 2 (Z), while the green line displays the density for the control group ($W$). It is obvious that the outcomes in the two experiments have to be modelled with different distributions[^transforamtion]. A Bayesian framework provides flexibility to model the data with distributions that seem appropriate. 

For Experiment 1 I model the data with a t-distribution. The t-distribution is more robust to outliers than the normal distribution because it can have heavier tails[^normal] [@kruschke2013bayesian].

\begin{equation}
  \label{eq:x_y_dist}
  \begin{split}
      X \sim T(\nu, \mu_1, \sigma_1) \\
      Y \sim T(\nu, \mu_2, \sigma_2) 
  \end{split}
\end{equation}

[^normal]: The model was also estimated using a normal distribution, but this model provided inferior fit in the posterior predictive checks.

Where $\nu$ is the parameter for the degrees of freedom (the lower $nu$, the heavier the tails of the distribution). I use uninformative priors since there is no prior research that would give me information on the relevant distribution: $\nu \sim exp(0.001)$, $\mu_i \sim N(0, 1000)$, $\sigma_i \sim IG(1, 1)$. Where $i = 1, 2$, $exp$ is the exponential distribution in scale parametrization, $N$ is the normal distribution, and $IG$ is the inverse gamma distribution.

For Experiment 2 I model the data using gamma distributions:

\begin{equation}
  \label{eq:z_w_dist}
  \begin{split}
      Z \sim GA(\alpha_1, \beta_1) \\
      W \sim GA(\alpha_2, \beta_2)
  \end{split}
\end{equation}
 
$\alpha$ and $\beta$ are the shape and rate parameters respectively. I will interpret results later in terms of the expectation of the distribution $\theta = \frac{\alpha}{\beta}$. The following noninformative priors are used: $\alpha_i \sim GA(0.0001, 0.0001)$, $\beta_i \sim GA(0.0001, 0.0001)$, for $i = 1,2$.

Samples from the posteriors of both experiments have been generated using \texttt{rSTAN} [@rstan-software:2014] with two chains with 9000 iterations each (1000 burn-in).

[^sensitivity]: The sensitivity of the results to the prior specification is reported in Appendix.

[^transforamtion]: A logarithmic transformation of the data in Experiment 2 still produces non-normal data. Additionally to the less intuitive interpretation and the problem of handling the many zeros. See the Appendix for the density of the transformations.

## Predictive Model

To get an estimate of $S$ in Group 2, a model is trained, that predicts the self placement from answers to issue questions. Since in Group 1 $S$ is asked before $P$ it is assumed to be unbiased as defined above (no consistency bias). I use a random forest to train the model [@breiman2001random]. Random forests have very good predictive performance [@hastie2009elements]. Further more, the use of random forests allows me to collect data only in two groups: To test Hypothesis 2 I compare the distances between $S$ and $S^*$ in both groups. With a standard predictive model, additional data would have to be collected, to avoid making predictions for the same data that has been used to train the model (which would overfit the data and therefore overstate the accuracy of the predictions). A random forest has the advantage, that it is an ensemble of many decision trees, each estimated using only a bootstrap sample of the data. The observations not contained in the bootstrap sample are called out-of bag observations for a specific tree. For these observations, the tree can be used to make a prediction that is not based on information of said observations. For the predictions of $S$ in Group 1, a new random forest is formed for each observation, just using the trees that were fit without that observation. 

The predictors were selected prior to the data collection from the American National Election Study, by training a random forest to the complete data set using all potential predictors for ideology (on a 7 point liberal - conservative scale). I then selected the 20 most important predictors according to the permutation importance criterion[^imp]. These predictors are mostly questions on classical ideoloically salient political issues like abortion, gun rights, and environmental protection, etc. Details on this process can be found in the online appendix. 

[^imp]: Permutation importance is defined as the increase in predictive accuracy if the variable of interst is randomly permuted. It is therefore a very suitable criterion to select variables for a model that has to achieve predictive accuracy, as in the present research design.

![Predicted self positions (out-of-bag for Group 1) from the random forest against observed values by experimental group. \label{fig:pred}](../figures/main/prediction.png)

Figure \ref{fig:pred} displays the predicted against the reported self placements. About 60% of the variance in $\hat{S}$ in Group 1 is explained by the model. At a first glance this might seem not very much, however it has to be kept in mind that ideology is not a very well defined concept, especially not for survey respondents. However, the predictions do not have to be perfect for the basic logic of the research design to work. If there is a treatment effect, the predictive model will be less accurate in Group 2 (the treatment group in experiment 1), due to the consistency bias it can not account for. More precisely it will be less accurate in the direction of the preferred party (see Equation \ref{eq:s_dist}). 
Therefore, the variance of the prediction is less important than its bias. In order for the logic of the research design to work, it is crucial that the prediction of the model is on average unbiased. The average distance between $\hat{S}$ and $S^*$ in Group 1 is $-0.27$ on a scale from 0 to 100. I take this as confirmation, that the model achieves sufficient average accuracy for the purposes of this study[^additional].

[^additional]: Additional information on the predictors, their importance and their relationship to self reported ideology can be found in Table \ref{tab:predictors}, Figure \ref{fig:imp}, and Figure\ref{fig:pd} in the Appendix.  


# Results

## Model Fit

Following @gelman1996posterior I conduct posterior predictive checks to asses the fit of the model assumed for the data. Figure \ref{fig:post_pred} displays a graphical test. The red lines represent the density of the observed outcome variables for both groups in both experiments. The blue lines are draws from the posterior predictive distribution at a random sample of 100 iterations of the MCMC sampler. The data simulated from the assumed models correspond with the observed data very well. This confirms, that the assumptions made in Equations \ref{eq:x_y_dist} and \ref{eq:z_w_dist} are justified.

![Posterior predictive check for assumed model. The red lines represent the density estimate of the observed data. The black lines are 100 replications from the assumed model using the current draw of parameters of the MCMC sampler. \label{fig:post_pred}](../figures/main/post_pred.png)

\clearpage

## Hypotheses

Table \ref{tab:res1} displays the parameter estimates for Experiment 1. $\mu_1$ and $\mu_2$ correspond to the estimates for the mean in Groups 1 (Control, $S$ is asked first) and 2 (Treatment, $P$ is asked first), respectively. $\mu_{diff}$ and $\mu_{ratio}$ correspond to the difference and ratio of means between Group 1 and 2. The colums $2.5\%$ and $97.5\%$ indicate the 95\% credible intervals for the parameters. The average distance between the reported self position and its prediction in the treatment group ($P$ is asked first) is two points or 20\% larger than in the control group. 

\input{res_table_1.tex}

This means that respondents on average, move their self reported position towards the position that they reported for their preferred party before. This difference could also be due to random variation in the accuracy of the prediction. To understand the uncertainty of this estmate, the left two panels in Figure \ref{fig:mean_diff} display the posterior distribution of the difference in means and the ratio of means for Experiment 1. The area shaded in blue is the probability mass for the respective hypothesis, the red area against. Given the priors, the assumed model and the data, the probability that there is a bias in the hypothesized direction (differences smaller 0 and ratios smaller 1) in Experiment 1 is $0.92$. The estimated probability for a substantively significant effect - which I define as an increase in distance between prediction and reported position of at least 10\% - is 0.71. In sum, the substantive effect is not very large, and the evidence is relatively uncertain. The data hints at a consistency bias on the individual level, but in order to be able to draw more firm theoretical conclusions, additional evidence would have to be collected.

![Posterior distribution of the difference in means and the ratio of means in both experiments. \label{fig:mean_diff}](../figures/main/mean_diff_ratio.png)

Table \ref{tab:res2} and the left two panels of Figure \ref{fig:mean_diff} display the results for Experiment 2. The parameters $\theta_1$ and $\theta_2$ are the means of the distributions in Groups 1 (Treatemet, $S$ is asked first) and 2 (Control, $P$ is asked first), respectively. Again, $\theta_{diff}$ and $\theta_{ratio}$ are the difference and ratio between Group 1 and Group 2. The unit of the parameters is the squared distance between the reported position for the preferred party $\hat{P}$ and the reported self position $\hat{S}$ in the treatment group and the estimated unbiased position $S^*$ in the control group. The distance between these positions is on average 135 points or 26\% smaller in the Treatment group, compared to the control group.  

\input{res_table_2.tex}

The posterior distributions of $\theta_{diff}$ and $\theta_{ratio}$ show that there is a high probability, that the distance between the postions is smaller in the treatment group. Given the priors and the model, the probability for a consistency bias is 0.99. The probability for an substantively significant effect (a reduction in distance of more than 10\%) is 0.95. This means that there is a high probability, that subjects biased their reported party positions in order to decrease the distance between their own and their preferred party. 

# Discussion

The literatures on source cues and party identification have delivered considerable evidence, that individuals positions on issues, values, and evaluations of policy are influenced by the positions political elites hold on them and the affect the individual has towards these elites. In this study I tried to clarify how much of a role consistency plays in these findings. Previous research attributed most of the effects to information and the role of heuristic reasoning on the side of the citizen.

 I took information out of the equation, and tested how much bias is introduced just by asking respondents to report their preferred pary, their own ideological position and the preferred party's ideological position. I hypothesized, that potential discrepancies between the own and the perceived party position would lead to cognitive dissonance, which could be attenuated by the subject by misreporting either their own or the party's position. I found weak evidence for consistency bias in the reported self positions and strong support for consitency bias in the reported party positions. 

These results add more evidence to the general findings of effects of group membership or affect on where individuals perceive themselfs in the political space. The effects of source cues, contain the possibility of the individual learning from the cue and therefore finding her 'real' position on the issue. Therefore, from a normative perspective, source cue effects have been often interpreted in a positive light, since they allow citizens to make informed decisions, without having to invest too many resources into politics [e.g. @druckman2001using]. However, my results indicate that part of the effect might also be due to consistency bias, which does not have these positive normative implications. 

The results of this study furthermore demonstrate a very conctete issue with the measurement of ideology in surveys: If asked in close proximity and in conjunction with party preference, the answers to self reported ideology as well as perceived party ideology might be biased towards each other. This has general implications for the validity of these measures, as well as for empirical work that is based on the distance between perceived and self reported positions. One such example is the debate on proximity and directional voting, where some evidence in favor of the proximity model is based on correlations between these concepts. 

\clearpage

\input{appendix.tex}

\clearpage

# References
