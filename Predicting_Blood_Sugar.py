##----- Importing Required Packages -----##
import pandas as pd
import numpy as np
import re
#from keras.models import Sequential
#from keras.layers import Dense


##----- Setting option to see 15 rows at once -----##
pd.set_option('display.max_columns', 15)

##----- Reading in Required Files -----##
bg = pd.read_csv("D:\\Personal\\Bioconscious\\data\\blood-glucose-data.csv")
da = pd.read_csv("D:\\Personal\\Bioconscious\\data\\distance-activity-data.csv")
hr = pd.read_csv("D:\\Personal\\Bioconscious\\data\\heart-rate-data.csv")

##----- Converting Date Column into Date-Time Format -----##
bg.dtypes
bg["point_timestamp"] = pd.to_datetime(bg.point_timestamp)
da["point_timestamp"] = pd.to_datetime(da.point_timestamp)
hr["point_timestamp"] = pd.to_datetime(hr.point_timestamp)
bg.dtypes

##----- Interpolating the Blood Sugar Data -----##
bg2 = bg.copy()
bg2.point_timestamp = pd.to_datetime(bg2['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[:17]+"00")
point_time = pd.date_range(start = bg2.point_timestamp.min(), end = bg2.point_timestamp.max(), freq = "1min")
point_time = pd.DataFrame({"point_timestamp" : point_time})
point_time["Check"] = 1

## Merging to get 1 min intervals
bg2 = point_time.merge(bg2, how = "outer", left_on = "point_timestamp", 
                       right_on = "point_timestamp").sort_values(by = ["point_timestamp"])

bg2 = bg2.drop(["Check", "timezone_offset"], axis = 1)
bg2 = bg2.rename(columns = {"point_value(mg/dL)": "point_value.mg.dL"})

bg2["point_value.mg.dL"] = bg2["point_value.mg.dL"].interpolate(method = "linear")
bg2["point_value.mg.dL"] = round(bg2["point_value.mg.dL"])
bg2 = bg2.groupby(["point_timestamp"]).mean()
bg2["point_timestamp"] = bg2.index
bg2.index = range(len(bg2))

##----- Interpolating the Heart Rate Data -----##
hr2 = hr.copy()
hr2.point_timestamp = pd.to_datetime(hr2['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[:17]+"00")
point_time_hr = pd.date_range(start = hr2.point_timestamp.min(), end = hr2.point_timestamp.max(), freq = "1min")
point_time_hr = pd.DataFrame({"point_timestamp" : point_time_hr})
point_time_hr["Check"] = 1

## Merging to get 1 min intervals
hr2 = point_time_hr.merge(hr2, how = "outer", left_on = "point_timestamp", 
                       right_on = "point_timestamp").sort_values(by = ["point_timestamp"])

hr2 = hr2.drop(["Check", "timezone_offset"], axis = 1)

hr2["point_value"] = hr2["point_value"].interpolate(method = "linear")
hr2["point_value"] = round(hr2["point_value"])
hr2 = hr2.groupby(["point_timestamp"]).mean()
hr2["point_timestamp"] = hr2.index
hr2.index = range(len(hr2))

##----- Joining Heart Rate to Blood Sugar -----##
bg2 = bg2.merge(hr2, how = "left", left_on = "point_timestamp", right_on = "point_timestamp")
bg2.dropna(axis = 0, inplace = True)

##----- Interpolating the Distance Data -----##
da2 = da.copy()
da_iphone = da2[da2.device == "iPhone"]
da_fitbit = da2[da2.device == "FitbitWatch"]
da_iphone = da_iphone[da_iphone["point_value(kilometers)"] > 0]
da_fitbit = da_fitbit[da_fitbit["point_value(kilometers)"] > 0]

da_iphone.point_timestamp = pd.to_datetime(da_iphone['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[:17]+"00")
da_fitbit.point_timestamp = pd.to_datetime(da_fitbit['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[:17]+"00")

da_full = da_iphone.append(da_fitbit, ignore_index=True)
da_full = da_full.groupby(["point_timestamp"]).mean()
da_full["point_timestamp"] = da_full.index
da_full.index = range(len(da_full))
da_full = da_full.drop(["timezone_offset"], axis = 1)


point_time_da = pd.date_range(start = da_full.point_timestamp.min(), end = da_full.point_timestamp.max(), freq = "1min")
point_time_da = pd.DataFrame({"point_timestamp" : point_time_da})
point_time_da["Check"] = 1

## Merging to get 1 min intervals
da_full = point_time_da.merge(da_full, how = "outer", left_on = "point_timestamp", 
                       right_on = "point_timestamp").sort_values(by = ["point_timestamp"])

da_full = da_full.drop(["Check"], axis = 1)
da_full = da_full.rename(columns = {"point_value(kilometers)": "point_value.kilometers"})

da_full["point_value.kilometers"] = da_full["point_value.kilometers"].interpolate(method = "linear")

da_full = da_full.groupby(["point_timestamp"]).mean()
da_full["point_timestamp"] = da_full.index
da_full.index = range(len(da_full))

##----- Joining Distance to Blood Sugar & Heart Rate -----##
bg2 = bg2.merge(da_full, how = "left", left_on = "point_timestamp", right_on = "point_timestamp")

##----- Aggregating Data to 5 min intervals -----##
bg2 = bg2[0:len(bg2) - len(bg2) % 5]
bg2["grp"] = np.repeat(range(1,int((len(bg2) + 5) /5) ), 5)
bg2 = bg2.groupby(["grp"]).agg({"point_value.mg.dL": 'mean' , "point_value": 'mean',
                           "point_value.kilometers": 'mean', 'point_timestamp': 'min'})
bg2["grp"] = bg2.index
bg2.index = range(len(bg2))
bg2["point_value.mg.dL"] = round(bg2["point_value.mg.dL"])
bg2["point_value"] = round(bg2["point_value"])

##----- Creating 5 min Future Value as Y -----##
bg2["future"] = pd.to_datetime(bg2['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[:17]+"00") + pd.Timedelta(minutes=5)

bg2 = bg2.merge(bg2[["point_timestamp", "point_value.mg.dL"]], how = "inner", left_on = "future", right_on = "point_timestamp")
bg2 = bg2.drop(columns = ["point_timestamp_y"], axis = 1)
bg2 = bg2.rename(columns = {"point_value.mg.dL_x" : "point_value.mg.dL", "point_value.mg.dL_y" : "Y"})

##----- Getting Moving average of past Blood-Sugar level as a Predictor -----##
bg2 = bg2.dropna(how = "any")
bg2["maverage"] = bg2.loc[:, ["point_value.mg.dL"]].rolling(window=12, min_periods = 1).mean()
mavg = bg2["maverage"].tolist()
mavg.insert(0,np.nan)
mavg.pop(len(mavg)-1)
bg2["maverage"] = mavg
bg2.loc[0,"maverage"] = bg2.loc[1,"maverage"]
bg2["maverage"] = round(bg2["maverage"])

##----- Splitting data into Train and Test -----##
bg2 = bg2.rename(columns = {"point_timestamp_x" : "point_timestamp"})
bg2_train = bg2.loc[(bg2["point_timestamp"] <= "2017-05-29 23:59:00")]
bg2_Test = bg2.loc[(bg2["point_timestamp"] > "2017-05-29 23:59:00") & (bg2["point_timestamp"] <= "2017-07-03 23:59:00")]
bg2_actTest = bg[(bg["point_timestamp"] > "2017-05-29 23:59:00") & (bg["point_timestamp"] <= "2017-07-03 23:59:00")]
bg2_actTest["point_timestamp"] = pd.to_datetime(bg2_actTest['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[:17]+"00")

bg2_actTest = bg2_actTest.groupby("point_timestamp").mean()
bg2_actTest["point_timestamp"] = bg2_actTest.index
bg2_actTest.index = range(0,len(bg2_actTest))
bg2_actTest = bg2_actTest.drop(columns = ["timezone_offset"], axis = 1)

##----- Assuming that the test data has a new starting point. Recalculating moving average -----##
bg2_Test["maverage"] = np.nan
bg2_Test["maverage"] = bg2_Test.loc[:, ["point_value.mg.dL"]].rolling(window=12, min_periods = 1).mean()

mavg2 = bg2_Test["maverage"].tolist()
mavg2.insert(0,np.nan)
mavg2.pop(len(mavg2)-1)
bg2_Test["maverage"] = mavg2
bg2_Test = bg2_Test.reset_index()
bg2_Test.loc[0,"maverage"] = bg2_Test.loc[1,"maverage"]
bg2_Test["maverage"] = round(bg2_Test["maverage"])

bg2_train["speed"] = bg2_train["point_value.kilometers"]/(5/60)
bg2_Test["speed"] = bg2_Test["point_value.kilometers"]/(5/60)
bg2_train["day_night"] = np.where((bg2_train['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[11:13].astype("int") >= 7)
                                            & (bg2_train['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[11:13].astype("int") < 19),
                                            1,0)

bg2_Test["day_night"] = np.where((bg2_Test['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[11:13].astype("int") >= 7)
                                            & (bg2_Test['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[11:13].astype("int") < 19),
                                            1,0)

## Ensuring the 1 min difference in the actual test data matches the interpolated one to calculate actual MSE
bg2_actTest["point_timestamp"] = np.where((bg2_actTest['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[15:16].astype("int") == 2)
            | (bg2_actTest['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[15:16].astype("int") == 7),
                                          pd.to_datetime(bg2_actTest['point_timestamp'] - pd.Timedelta(minutes=1)),
                                                         np.where((bg2_actTest['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[15:16].astype("int") == 0)
            | (bg2_actTest['point_timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S').str[15:16].astype("int") == 5), 
            pd.to_datetime(bg2_actTest['point_timestamp'] + pd.Timedelta(minutes=1)),bg2_actTest["point_timestamp"] + pd.Timedelta(minutes=0)))

##----- Scaling Data for Neural Nets -----##
bg2_train = bg2_train.drop(columns = ["grp"])
bg2_Test = bg2_Test.drop(columns = ["index","grp"])
a = pd.concat([bg2_train, bg2_Test])
scaled = a.copy()
scaled.loc[:,['point_value.mg.dL', 'point_value', 'point_value.kilometers',
        'Y', 'maverage', 'speed', 'day_night']] -= scaled.drop(columns = ["point_timestamp", "future"], axis=1).min()

scaled.loc[:,['point_value.mg.dL', 'point_value', 'point_value.kilometers',
        'Y', 'maverage', 'speed', 'day_night']] /= scaled.drop(columns = ["point_timestamp", "future"], axis=1).max()


scaled_train = scaled[0:len(bg2_train)]
scaled_test = scaled[(len(bg2_train)):len(scaled)]


