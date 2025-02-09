import pandas as pd
import sys

sys.stdout.reconfigure(encoding='utf-8')

df = pd.read_csv("avar_andic_rus_ranks.csv")
df = pd.DataFrame(df)

df = df.dropna(subset=['russian_ipa', 'target_ipa', 'language'])
df["russian_ipa_segments"] = df["russian_ipa"].astype(str).str.split("-")
df["target_ipa_segments"] = df["target_ipa"].astype(str).str.split("-")


df = df.explode(["russian_ipa_segments", "target_ipa_segments"]).reset_index(drop=True)

grouped_df = df.groupby(["language", "russian_ipa_segments", "target_ipa_segments"]).agg(
    count=("target_ipa", "size"),
    corresponding_target_ipa=("target_ipa", list)
).reset_index()

grouped_df.to_csv("to_view_segments.csv")
