# DISPLACE California

DISPLACE California is an open-source, agent-based, spatially explicit bioeconomic modeling framework designed to simulate the behavior of individual fishing vessels. The framework is used to evaluate the potential impacts of California’s Offshore Wind energy development lease areas on the DTS fisheries operating off the coast of California. 

> **Note:** The data in this repository is entirely example-based and does **not** represent confidential or real fisheries information. It is a demonstration of methods and workflows that can be adapted for real-world applications.

---

## Project Overview

DISPLACE simulates individual fishing vessels as agents whose decisions are influenced by historical fishing behavior, regulations, and economic incentives. This repository provides an application example based on our study of the California coast, using only example data.

The model can be used to:
- Assess spatial and temporal fishing effort distributions,
- Explore the effects of management measures and spatial closures,
- Analyze potential interactions with offshore wind development.

---

## Working Environment

- **DISPLACE Version:** 1.3.5 (Windows 11)
  - [Download DISPLACE installer](https://displace-project.org/blog/download/)
- **R Version:** 4.5.0 (cross-platform)
- All scripts for preparing input data and basic output processing are in the `r` and `routines` folders.

### Directory Structure

```
DISPLACE_California
  |__ docs
  |__ qmd
  |__ raw_inputs
  |__ processed_inputs
  |__ outputs
  |__ routines
  |__ r
```

- The `processed_inputs` folder is initially empty and is populated when running routines from `r/functions_routines.R`.

---

## Quick Setup & Run

1. **Edit the installer directory** in the `directory` parameter of the `generate_dis_run_files` function within `functions_routines.R`.
2. To test a basic model run:
    ```r
    source(here::here("r/functions_routines.R"))
    ```
3. Review the repository to understand the full workflow and input requirements.

---

## Input Data & Example Outputs

### Fishing Effort Distribution

DISPLACE requires spatial object inputs defining fishing effort distributions. These can be provided at various levels of aggregation (fleet-wide, by port, métier, or vessel). The example below demonstrates overall fleet effort and vessel-specific effort inputs:

**Example Figure: Fishing effort distribution input examples**

_Left: Overall fleet fishing effort (hours), Center/Right: Example vessel (USA0028) effort by métier._

> (See the actual figure in the rendered notebook or [fisheries.qmd](qmd/fisheries.qmd). Example code for generating these maps is included.)

---

### Spatial Extent

The spatial domain for this study covers the California coast, from the US/Mexico EEZ border to Cape Blanco (32.0° to 42.6° latitude, -125.1° to -117.3° longitude), with nodes spaced 4 km apart.

**Example Figure: DISPLACE study area and node network**

_The spatial extent is determined by the merged VMS-fish ticket dataset, with OSW lease areas shaded. See the rendered notebook or [spatial_extend.qmd](qmd/spatial_extend.qmd) for the figure._

---

## Learn More

- [DISPLACE Project Documentation](https://displace-project.org/blog/tutorials/)
- [How to set up a new graph of nodes](https://displace-project.org/blog/wp-content/uploads/2014/11/DISPLACE-step-by-step-guideline-Set-up-a-new-graph-of-node1.pdf)

---

## Citation

If you use this repository or DISPLACE in your work, please cite the original DISPLACE publication and this repository.

---

## License

Distributed under the MIT License. See `LICENSE` for more information.

---

_For detailed explanations, input data requirements, and scripts, consult the `qmd/` directory and the respective Quarto notebooks.