# DISPLACE California

[DISPLACE](https://displace-project.org) is an open-source, agent-based, spatially explicit bioeconomic modeling framework designed to simulate the behavior of individual fishing vessels, developed by François et al. (2014).  

Here we present an specific DISPLACE application used to evaluate the potential impacts of California’s offshore wind energy development lease areas on the DTS fisheries operating off the coast of California.  

> **Note:** The data in this repository are entirely example-based and do **not** represent real fisheries information. They are provided solely to demonstrate methods and workflows that can be adapted for real-world applications.

---

## Working Environment

- **DISPLACE Version:** 1.3.5 (Windows 11)
  - [Download DISPLACE installer](https://displace-project.org/blog/download/)
- **R Version:** 4.5.0 (cross-platform)
- All scripts for preparing input data and basic output processing are in the `r` and `routines` folders.
- The `processed_inputs` and `outouts` folder are initially empty.

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

---

## Quick Setup & Run

1. **Edit the installer directory** in the `directory` parameter of the `generate_dis_run_files` function within `functions_routines.R`.
2. To test a basic model run:
    ```r
    source(here::here("r/functions_routines.R"))
    ```
3. Review the notebook to understand the full workflow and input requirements.

---

## Citation

If you use this repository or any reference from the notebook in your work, please cite this repository:  

**Citation**  
Cabral, R., Carbo-Mestre, P., Free, C., Gaines, S., Mangin, T., & Willis-Norton, E. (2025). *Projected impacts of offshore energy development on California's DTS fishery* [Web notebook & repository]. DISPLACE California Case Study Example. [https://github.com/pcarbomestre/DISPLACE_California](https://github.com/pcarbomestre/DISPLACE_California)
