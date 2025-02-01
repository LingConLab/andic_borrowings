import pandas as pd
import re

def CountSegments(string):
    return len(string.split("-"))


def RemoveDash(string):
    return "".join(string.split("-")).lower()

def DeletePalatal(string):
    string = re.sub(r'[\']', '', string)
    string = re.sub(r'ts', '7', string)
    string = re.sub(r'[\u02B2\u02B7]', '', string)
    return string


def CompareStrings(str1, str2):
    count = sum(char1 == char2 for char1, char2 in zip(str1, str2))
    return count


df = pd.read_csv("bor_to_count.csv")
df = pd.DataFrame(df)

df["total_segments"] = df["target_ipa"].apply(CountSegments)
df["russian_ipa_nonpal"] = df["russian_ipa"].dropna().apply(DeletePalatal).apply(RemoveDash)
df["target_ipa_nonpal"] = df["target_ipa"].dropna().apply(DeletePalatal).apply(RemoveDash)
df["match_count"] = df.apply(lambda row: CompareStrings(str(row["russian_ipa_nonpal"]), str(row["target_ipa_nonpal"])), axis=1)

df = df.drop(["russian_ipa_nonpal", "target_ipa_nonpal"], axis=1)

df.to_csv("avar_andic_bor_with_ranks.csv")
