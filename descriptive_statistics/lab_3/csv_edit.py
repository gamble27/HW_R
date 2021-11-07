import pandas as pd


df = pd.read_csv("geoMap_lsv.csv")
df.columns = ["country", "beer", "wine"]
df2 = df.dropna()

# for row in df2.itertuples():
#     print(row[2])
#     row[2] = int(row[2][:-1])
#     row[3] = int(row[3][:-1])

for i,row in df2.iterrows():
    for field in ("beer", "wine"):
        # print(df2[field][i])
        df2[field][i] = int(df2[field][i][:-1])

df2.to_csv("geoMap_fixed.csv")
