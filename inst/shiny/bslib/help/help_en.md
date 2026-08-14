<!--
Contextual help of the cards — reference file (English).

One section per card. The section name is the full id of the card, namespace
included (module-card), and must match the `help =` argument of icon_header().
The first "## ..." line is the title of the modal, the rest is markdown.

To translate : copy this file to help_<lang>.md and translate the text only,
never the "# key" lines. A missing key falls back to English.
-->

# wave-card_presence
## Variables missing in a wave
This table only appears when a variable is **present in one wave and absent in
another** among the compared waves.

- Each column is a wave, each cell lists the variables of that wave.
- ✔ the variable holds values, ✖ the variable is empty or absent.

A variable that disappears is often normal (questionnaire change, new routing).
The point is to confirm that every disappearance is intentional, before reading
the tables below: a variable absent from a wave cannot be compared.

# wave-card_cat
## Categorical variables that changed
One row per categorical variable whose distribution moved between the wave
selected in the sidebar and the comparison waves.

- One column block per wave, with `missing` (share of missing values) and
  `Nmod` (number of observed categories).
- A cell turns red when the variation between waves exceeds the threshold
  *Minimum rate of change* (button **Detection Thresholds**).
- Variables computed on too few rows or too few valid values are ignored
  (thresholds *Minimum number of rows* and *Minimum number of valid values*).

Click a row to display the chart on the right. Typical cases: a category that
disappears, a jump in missing values, a recoding applied to one wave only.

# wave-card_cat_detail
## Distribution across waves
Share of each category, wave by wave, for the variable selected in the table on
the left.

- Missing values appear as the `.NA` category.
- Colours are fixed per category, so the same category keeps its colour across
  waves.
- Beyond 15 categories the chart is not drawn (unreadable).

A break in the coloured bands between two waves confirms what the table
flagged; stable bands suggest a false alarm.

# wave-card_num
## Numeric variables that changed
One row per numeric variable whose level moved between the wave selected in the
sidebar and the comparison waves.

- One column block per wave, with `missing` (share of missing values) and
  `median`.
- A cell turns red when the variation between waves exceeds the threshold
  *Minimum rate of change* (button **Detection Thresholds**).
- The median is preferred to the mean, being less sensitive to extreme values.

Click a row to display the chart on the right. A median multiplied or divided by
a round factor usually points to a unit or scale change rather than a real
evolution.

# wave-card_num_detail
## Density across waves
Density of the variable selected in the table on the left, one curve per wave.

- The chart is trimmed at the 1st and 99th percentiles so that a few extreme
  values do not flatten the curves.
- Curves that overlap indicate a stable distribution; a shifted or squeezed
  curve confirms the break flagged by the table.

# intvwr-card_indics
## Choice of indicators
Indicators used in the *Synthesis* table and in the score.

Unticking an indicator removes it from the table **and** from the score
computation. This is useful when an indicator is not relevant for the survey
(for example night work when interviews are done by appointment only), or to
check whether the ranking rests on a single indicator.

# intvwr-card_synthesis
## Synthesis by interviewer
One row per interviewer, for the wave and the filter selected in the sidebar.

- `RANK` orders interviewers by the anomaly score (Isolation Forest): the rarer
  the combination of indicators, the higher the rank.
- `NB_INTV` is the number of interviews, `INDEX_H` the homogeneity of the
  answers, `MAX_CHI2` the largest deviation observed on a variable.
- Indicators starting with `DURATION_` come from the timers, those starting with
  `PC_` are shares of flagged interviews (night, too short, too close).
- A red cell is more than 2 standard deviations above the average of the column.
- Interviewers with fewer rows than the threshold *Minimum number of rows* are
  not displayed.

The rank prioritises the review, it proves nothing: click a row to open the
details in the tabs below.

# intvwr-card_distrib
## Details per indicator
One histogram per indicator, over all displayed interviewers.

The red vertical line marks the value of the interviewer selected in the
*Synthesis* table. It shows whether the interviewer is truly isolated, or simply
at the edge of a wide distribution — which the rank alone does not tell.

# intvwr-card_intv
## Interviews of the interviewer
Every interview of the selected interviewer, reconstructed from the timers.

- `DURATION_INTV` is the total duration, `DURATION_INTER_INTV` the gap with the
  previous interview.
- A 🔴 marks a flag: interview too short, night work, several sessions, or a
  negative gap (overlapping interviews).
- Thresholds for these flags are set at preparation time, in `config.txt`.

Click an interview to open its sessions below.

# intvwr-card_ssn
## Sessions of the interview
An interview can be split into several sessions (interrupted then resumed).

- One row per session, with its start, its duration and the gap with the
  previous one.
- `CHECK_SCTN` shows one dot per questionnaire section: 🟢 the section has been
  answered in this session, 🔴 it has not.

Many very short sessions, or sections answered out of order, are worth a look.

# intvwr-card_details
## Detail of the interview
Raw timer data of the selected interview: one row per item of the
questionnaire, with its timestamp and its duration.

This is the finest level available. Use it to confirm an anomaly seen above, for
example a series of items answered in a few seconds.

# intvwr-card_var
## Variables of the interviewer
Variables of the selected interviewer, ranked by deviation from the other
interviewers of the same wave and filter.

- `chi2` measures the distance between the interviewer's distribution and the
  reference distribution.
- `standard` is that distance normalised per variable: it is the value compared
  to the threshold *chi² distance / minimum median deviation*.
- Numeric variables are cut into 5 classes beforehand, so they are compared the
  same way as categorical ones.

Click a variable to display its distribution on the right.

# intvwr-card_var_distrib
## Comparison of distributions
Distribution of the selected variable for the interviewer (dark) against all the
other interviewers (grey).

- Categorical variable: share of each category, the percentage is the
  interviewer's.
- Numeric variable: density, trimmed at the 1st and 99th percentiles, with the
  median as a dashed line.
- The text summary below gives the usual statistics for the interviewer.

Read the shape, not only the gap: a category never used, or one used
systematically, is more telling than a small difference spread over everything.

# intvwr-card_var_mods
## Distribution of modalities
For each category of the selected variable, the distribution of its share **among
all interviewers**, with a red line at the value of the interviewer.

It answers a question the previous chart does not: is this interviewer isolated
on that category, or do many interviewers sit at the same level?

# intvwr-card_geo
## Localisation of interviews
Breakdown of the selected interviewer's interviews by the geographic variable
declared in `config.txt` (`var_info_geo`).

An interviewer covering an unusual area, or a single small area, may explain
deviations observed elsewhere: the respondents are simply not the same.

# intvwr-card_stat
## Number of interviews across wave
Number of interviews of the selected interviewer, per wave and per filter value.

This card is not filtered by the sidebar: it shows the whole history. Use it to
tell a newcomer from an experienced interviewer, or to spot an activity that
stops abruptly.

# intvwr_variable-card_cross
## Interviewer × variable deviations
One row per interviewer and variable pair whose deviation exceeds the threshold
*chi² distance / minimum median deviation*.

- `chi2` is the distance between the interviewer's distribution and the
  reference distribution of the wave and filter.
- `standard` is that distance normalised per variable, which makes variables
  comparable.
- Numeric variables are cut into 5 classes beforehand, so the whole table reads
  the same way.

The table is sorted by decreasing deviation: it is the entry point when you do
not know where to start. Click a row to display the distributions at the bottom
of the page.

# intvwr_variable-heatmap
## Anomaly heatmap
One row per interviewer, one column per variable. A cell is coloured when the
interviewer deviates from the others on that variable.

- *Choice of rows and columns* zooms on risky rows, risky columns, or only cells
  above the threshold.
- *Choice classification* reorders rows and columns to group similar profiles
  (seriation); `None` keeps the alphabetical order.
- Hover a cell to see the underlying values, click it to open the distributions
  at the bottom of the page.

An isolated cell is not a problem. A red row points to a recurring behaviour, a
red column to a fragile variable rather than to the interviewers.

# intvwr_variable-card_intvwr_ranking
## Ranking of interviewers
Interviewers ranked by anomaly score, computed on their deviations across all
variables.

- `score` comes from an Isolation Forest: the more atypical the profile of
  deviations, the higher the score.
- `N_outliers` counts the variables above the detection threshold.
- `Nrow` is the number of interviews, useful to relativise a score built on few
  observations.

Select an interviewer to list their variables on the right.

# intvwr_variable-card_variable_listing
## Variables of the selected interviewer
Variables of the interviewer selected on the left, with their deviation
(`diff`), sorted by decreasing absolute value.

It tells you which variables are responsible for the score, and whether they
share a theme (same questionnaire section, same routing) — which points to a
procedural cause rather than an individual one.

# intvwr_variable-card_variable_ranking
## Ranking of variables
Same principle as the ranking of interviewers, seen from the variables.

- `score` is high when the deviations of that variable are unevenly spread
  across interviewers.
- `N_outliers` counts the interviewers above the detection threshold.

A variable at the top of this table is often a questionnaire problem (ambiguous
wording, routing) rather than an interviewer problem. Select it to list the
interviewers concerned on the right.

# intvwr_variable-card_intvwr_listing
## Interviewers of the selected variable
Interviewers deviating on the variable selected on the left, with their
deviation (`diff`), sorted by decreasing absolute value.

If only one or two interviewers stand out, the cause is likely individual. If
many of them do, look at the variable itself, or at a subpopulation effect (try
the filter in the sidebar).

# intvwr_variable-card_distrib
## Comparison of distributions
Distribution of the selected variable for the selected interviewer (dark)
against all the other interviewers (grey).

- Categorical variable: share of each category, the percentage is the
  interviewer's.
- Numeric variable: density, trimmed at the 1st and 99th percentiles, with the
  median as a dashed line.
- The text summary gives the usual statistics for the interviewer.

This card is filled by clicking a cell of the heatmap or a row of the tables
above.

# intvwr_variable-card_distrib_mods
## Distribution of modalities
For each category of the selected variable, the distribution of its share among
all interviewers, with a red line at the value of the selected interviewer.

Use it to check whether the interviewer is really isolated on a category, or
whether the whole team spreads over a wide range.

# data-card_data
## Raw data
The survey data as they were prepared, with no filtering other than the variable
selection above.

Use it to check a value seen in an analysis, or to understand how a variable is
coded. The search boxes at the top of each column filter the rows; missing
values are shown as `NA`.

# dict-card_dict
## Variable dictionary
Labels and descriptions of the variables, coming from the dictionary file
supplied at launch.

It is the place to check the exact wording of a question before concluding on a
deviation: a difference between waves is often explained by a change of wording
or of routing.

# archive-card_archive
## Archive of observations
Comments recorded from the interface with the **Add history** button.

Each line keeps the date, the user, the survey, the interviewer and the variable
concerned, plus the comment. The file is shared by everyone using the same
archive path.

Record what you have checked, including false alarms: it saves the next person
from investigating the same case again.


# summary-card_survey
## The loaded survey
Identity of the survey selected in the sidebar.

- *Prepared on* is the date of the last `prepa_survey()` run: everything you see
  in the application comes from that moment, not from the live data.
- *Wave*, *Filter* and *Interviewer* recall which variables play these roles,
  as declared in `config.txt`. Two variables separated by a slash mean the
  breakdown has two levels.
- *Timers* tells whether the audit trail is available; without it the interview
  and session details stay empty.

# summary-card_waves
## Interviews per wave
Number of interviews of each wave, over the whole survey — this chart is not
affected by the selection in the sidebar.

When the wave has two levels, each bar is split by the second level (quarters
inside a year, for example). A wave much smaller than the others is expected
while fieldwork is running, but is worth a check once it is closed.

# summary-card_intvwr
## Where to start with interviewers
Number of interviewers showing at least one deviation above the detection
thresholds, for the wave and the filter selected in the sidebar.

The list gives the five interviewers with the most variables concerned. Open
the *Interviewer* tab to see the whole ranking, the audit indicators and the
detail of the interviews.

# summary-card_wave
## Where to start with waves
Number of variables whose level moved between the waves, for the filter selected
in the sidebar.

The same rule as the *Wave* tab is used (variation of an indicator compared to
the threshold *Minimum rate of change*), applied here to every wave at once. The
*Wave* tab lets you choose which waves to compare, so its list can be shorter.

# summary-card_cross
## Where to start with the crossing
Number of interviewer × variable pairs above the detection thresholds, for the
wave and the filter selected in the sidebar.

The list gives the five variables flagged for the largest number of
interviewers — often a sign of a fragile variable rather than of an interviewer
problem. Open the *Cross* tab for the heatmap and the rankings.
