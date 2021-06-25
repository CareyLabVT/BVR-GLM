README
A. Hounshell
25 June 2021

Some notes on the various Scripts used to run GLM-AED as well as to calibrate GLM-AED. Highlighting the most *important* scripts (others are various iterations of scripts!)

1. FieldDataPrep - script to prepare field data used for GLM-AED calibration/validation. Following scripts developed by C. Carey for FCR.
2. InflowPreparation - script to prepare inflow data (both discharge and nutrients) for BVR inflows. Following scripts developed by C. Carey for FCR. NOTE: All inflows are aggregated as one within the inflow + overland flow model from H. Wander; thus, there is ONLY one inflow file (discharge + nutrients) even though there are mulitple inflows into BVR! In addition, the inflowing nutrients are randomly distributed following the *few* inflow nutrient measurements we have for BVR. Finally, I started playing around with the idea of including a 'land-use change scenario' to (eventually!) see how changes inflowing nutrients + carbon due to land use change might affect carbon cycling in the reservoir. The numbers in the script were NEVER validated against literature values (represent a 'first guess')!
3. GLM_Run_Script_BVR_AGH - script to run GLM! Makes sure you can actually run GLM on your computer (important!) and has code for various analyses to check GLM calibrations. This is the most updated version of this script!
4. SensitivityCalibration_AGH - script to run sensitivity analysis/calibration for BVR GLM-AED. Again, this is the most updated script for this and includes GLM calibrations (temp) and AED calibrations through methane.
5. 20210225_functions-glm_hw - functions needed by the SensitivityCalibration_AGH script. These are the most updated functions and I reccommend using these!