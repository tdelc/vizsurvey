<!--
Guided tour — reference file (English). Same principle as help_en.md.

One section = one step, in the order of the file.
  tab:    tab to open (optional)
  target: CSS selector of the element to outline (optional -> centred bubble)
  ##      title of the bubble, the rest is markdown
  {xxx}   value from the current data, see tour_values() in mod_tour.R
          {survey} {n_rows} {n_vars} {n_intvwr} {n_waves}
          {wave} {filter} {n_outliers} {var_intvwr} {var_wave}

If the target is not on screen, the bubble is centred with no outline: the
step stays readable.
-->

# 01-bienvenue
tab: tab_summary
## Welcome to vizsurvey
This tour walks through the interface in ten steps, on the survey you have
loaded: **{survey}**, that is {n_rows} interviews and {n_vars} analysed
variables.

vizsurvey concludes nothing for you: it computes deviations and highlights
them to guide your review.

Use **Next**, the arrow keys, or **Esc** to quit.

# 02-source
target: #source-path_survey
## Choosing the survey
Everything starts here. The statistics are not computed on the fly: they were
prepared beforehand by `prepa_survey()` and stored in a `global.rds` file.

Switching survey reloads every tab instantly.

# 03-filtres
target: #filters-config_wave
## The wave and the filter
These two selectors drive **the whole application**. You are currently on the
wave **{wave}** and the filter **{filter}**.

This survey has {n_waves} waves, defined by the variable {var_wave}. The
filter restricts the analysis to a subpopulation: useful to check that a
deviation is not simply a geographic effect.

# 04-seuils
target: #filters-button_parms
## The detection thresholds
Nothing is flagged below these thresholds. They set how large a deviation must
be for a cell to turn red, and how many rows or valid values a variable needs
to be analysed at all.

There is no statistically grounded threshold: your team sets it by trial and
error. Lower it to see more cases, raise it to keep only the clearest ones.

# 05-resume
tab: tab_summary
target: #summary-card_intvwr
## Where to start
The first tab gives three numbered entry points. For the current wave and
filter, {n_outliers} interviewer × variable pairs are above the thresholds.

Each card has a button opening the matching tab: the shortest path between
"something is there" and "here is what".

# 06-vague
tab: tab_wave
target: #wave-card_cat
## Consistency across waves
Here the selected wave is compared with the comparison waves: one row per
variable whose distribution moved, a red cell when the variation exceeds the
threshold.

Typical cases are a category that disappears, a jump in missing values, or a
change of unit. Click a row: the chart on the right shows the evolution, wave
by wave.

# 07-enqueteur
tab: tab_intvwr
target: #intvwr-card_synthesis
## Consistency across interviewers
One row per interviewer, ranked by anomaly score. The score comes from an
Isolation Forest: the rarer the combination of indicators, the higher the rank.

This survey has {n_intvwr} interviewers, identified by the variable
{var_intvwr}. Click a row to open the details: interviews, sessions,
variables, distributions.

# 08-croise
tab: tab_intvwr_variable
target: #intvwr_variable-heatmap
## The interviewer × variable crossing
One row per interviewer, one column per variable, a coloured cell when the
deviation exceeds the threshold.

Reading it takes three rules: an isolated cell is not a problem, a red **row**
points to a recurring behaviour, a red **column** points to a fragile variable
rather than to the interviewers. Click a cell to see the compared
distributions at the bottom of the page.

# 09-aide
tab: tab_summary
target: #summary-card_survey .btn-link
## The help of every card
This small **?** sits on every card of the application. It explains what the
card shows, what its columns mean, and how to read it.

It complements this tour: the overview here, the detail there, right when you
need it.

# 10-archive
target: #archive-add_button
## Keeping a trace
Once you have checked a case, record it. The archive keeps the date, the user,
the survey, the interviewer and the variable concerned.

Record the false alarms too: that is what saves the next person from running
the same investigation again.

Enjoy the exploration.
