# Preprocess bias-adjusted WRF wind output for Phase 2 of Community Winds tool
# For each GCM (CM3, CCSM4) create data files containing high wind events 
#   based on 6 wind speed thresholds and 5 duration thresholds (30 files per GCM)

# preprocess all sites for a particular GCM
# ignore_era: ignore ERA output (e.g. if was included with first GCM and combining
#   both DataFrames, will prevent duplicates)
def preprocess_gcm(gcm_fns, ignore_era=False):
    # calculate mean angle (input/output scale: [0, 360))
    def mean_angle(deg):
        x = degrees(phase(sum(rect(1, radians(d)) for d in deg)/len(deg)))
        if x < 0:
            return round(360 + x, 2)
        else:
            return round(x, 2)

    thresholds = [list(range(25, 55, 5)), [1, 6, 12, 24, 48]]
    # initialize DataFrame for GCM
    colnames = ["gcm", "stid", "wd", "ts", "ws_thr", "dur_thr"]
    result = pd.DataFrame(columns = colnames)
    # initialize dict of year values representing 20-year periods (for lookup)
    grp_yrs = list(range(1980, 2120, 20))
    yrs_di = dict.fromkeys(list(range(grp_yrs[0], grp_yrs[1])), grp_yrs[0])
    for i in range(len(grp_yrs) - 1):
        yrs_di.update(dict.fromkeys(list(range(grp_yrs[i], grp_yrs[i + 1])), grp_yrs[i]))

    for fn in gcm_fns:
        d = pd.read_csv(os.path.join(directory, fn))
        # remove ERA observations if requested
        if ignore_era == True:
            d = d[d["gcm"] != "ERA"]
        # iterate through ws thresholds, compute DF with all potential hwes
        for ws_thr in thresholds[0]:
            d_thr = d.copy()
            ws_b = d_thr["ws"] > ws_thr
            hw_id = ws_b != ws_b.shift(1)
            hw_id = hw_id.cumsum()
            hw_id = hw_id//2
            d_thr["hw_id"] = hw_id
            d_thr = d_thr[ws_b]
            hw_id = hw_id[ws_b]
            hw_id = hw_id.reset_index()
            hw_counts = hw_id.groupby("ws").agg(lambda x: np.max(x) - np.min(x))
            # iterate through dur thresholds, compute
            for dur_thr in thresholds[1]:
                keep = hw_counts[hw_counts['index'] > (dur_thr - 1)].index.values
                d_hwe = d_thr[d_thr["hw_id"].isin(keep)]
                d_hwe = d_hwe.drop(["ws"], axis=1)
                d_hwe = d_hwe.groupby(["hw_id", "gcm", "stid"], as_index=False).agg({"wd": mean_angle, "ts": "first"})
                d_hwe["ts"] = list(map(int, d_hwe["ts"].str[:4].values))
                d_hwe = d_hwe[d_hwe["ts"] < 2100]
                # map year groups
                d_hwe["ts"] = d_hwe.ts.map(yrs_di)
                # add threshold columns
                d_hwe["ws_thr"] = ws_thr
                d_hwe["dur_thr"] = dur_thr
                d_hwe = d_hwe.drop("hw_id", 1)
                result = result.append(d_hwe, ignore_index=True)
    return result

# preprocess both models and save files
def preprocess_stations(fn_lst):
    out_dir = "../data/cw/"

    print("Preprocessing CM3 output")
    tic = time.clock()
    cm3 = preprocess_gcm(fn_lst[0])
    print("CM3 output preprocessed, time elapsed: " + str(round(time.clock() - tic, 2)) + "s")
    
    print("Preprocessing CCSM4 output")
    tic = time.clock()
    ccsm4 = preprocess_gcm(fn_lst[1], ignore_era=True)
    print("CCSM4 output preprocessed, time elapsed: " + str(round(time.clock() - tic, 2)) + "s")

    print("Saving files")
    tic = time.clock()
    gcm = cm3.append(ccsm4, ignore_index=True)
    gcm.to_csv(out_dir + "WRF_hwe.csv", index=False)
    print("Preprocessed data files saved, time elapsed: " + str(round(time.clock() - tic, 2)) + "s")

import pandas as pd
import numpy as np
import os
import time
from cmath import rect, phase
from math import radians, degrees

directory = "../data/cw/wrf_adj/"
wrf_fns = os.listdir(directory)

# create list (of lists) of filenames (for each gcm)
gcms = ["CM3", "CCSM4"]
fn_lst = []
for gcm in gcms:
    fns = [i for i in wrf_fns if gcm in i]
    fn_lst.append(fns)

# Make this skippable during dev
preprocess = True

if preprocess:
    preprocess_stations(fn_lst)

