# Baseball-QEs

Created: August 16, 2024

Methods and analysis for the impact of rule changes on MLB and its players, using quasi-experimental methods and panel data.

A manuscript describing the methods and results is available at: <https://arxiv.org/abs/2411.15075>. Note: this version of the manuscript has not been peer-reviewed.

A Shiny App to interactively display results of the analysis is available at: <https://bit.ly/SCM-baseball>.

Or by using the app.R file in this repository.

For re-use, the analysis files are in order beginning with "01-import.R". Any changes to later-numbered files do not require re-running earlier-numbered files. Note that "03b-SCRuns.R" runs the synthetic controls and takes the longest amount of time.

Specific figures can be found in the following folders (replace [] with 2023-full, 2023-24, or 2024 for specific result sets):

-   DID analyses for different outcomes in figs/DID Analysis
-   Figures and Tables used in the manuscript in figs/Manuscript
-   Player-specific SCM analyses in figs/Players-SC-[]
-   SCM estimates across all players in figs/SC-[] Estimates
-   Trajectories of player outcomes by category in figs/Trajectories
