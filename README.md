# Human Cultural Boundaries (HCB)

## Overview
This agent-based model attempts to simulate the population of the Americas through the Bering Strait to assess culture exchange, specifically language phonemes.  

## Getting Started
1) Install the HCB package (requires RTools).
```
library(devtools)
install_github('NeuroBio/HCB_R_Package')
```

2) Add the package to your working environment.
```
library('HCB')
```

3) set up your simulation parameters (it makes a working set of params by default).
```
params = DefineParameters()
```

4) run the following simulation with the parameters and graph:
```
# completes a migration stage for x time steps and then allows for horizontal transfer stage for x time steps
data = HCBSimmulation(Params)
BeringStraitPlot(Parameters, data$Pre) # before horizontal Transfer
BeringStraitPlot(Parameters, data$Post) # after horizontal Transfer
```

## Further Help
See [the manual](manual.pdf) for an overview of how each function works and the arguments they take.

There are two simulation functions, `HCBSimmulation()` as described above, and `HCBAlternatorSimmulation()`, which alternates between a
migration phase and a horizontal transfer phase each timestep.  A full description of all parameters is listed and explained in `DefineParameters()`.
The built in plot types include:
- BeringStraitPlot() (only for use when the simulation employs the Bering Strait barriers)
- PopulationPlot()
- PhonemeFrequencyPlots()
- MigrationPlot()
- SnapshotPlot()
