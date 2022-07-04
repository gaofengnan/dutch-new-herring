# import re
import os
import csv
import pandas as pd
import numpy as np

input_csv_file = '1617_combined_output.csv'
input_vollaard_csv_file = 'scores_herringtest_2016_2017.csv'
output_csv_vollaard_file = 'vollaard_output.csv'
output_csv_ours_file = 'ours_output.csv'
output_csv_ours_reordered_file = 'ours_reordered_output.csv'
csv_delimiter = ';'

# init the output 
if os.path.exists(output_csv_vollaard_file):
    os.remove(output_csv_vollaard_file)
    print("The output file has been removed.")
else:
    print("The output file does not exist. Nothing to remove.")

if os.path.exists(output_csv_ours_file):
    os.remove(output_csv_ours_file)
else:
    print("The vollaard output file does not exist. Nothing to remove.")


df = pd.read_csv(input_csv_file, delimiter=csv_delimiter)
contents = [list(row) for row in df.values]
contents_head = list(df.columns)
v_df = pd.read_csv(input_vollaard_csv_file, delimiter=csv_delimiter)
vollaard = [list(row) for row in v_df.values]
vollaard_head = list(v_df.columns)


print(contents_head)
# print(contents[0])
i_set = [9,13,11, -4]
print([contents_head[i] for i in i_set])

print(vollaard_head)
# print(vollaard[0])
v_i_set = [1,12,14,8]
print([vollaard_head[i] for i in v_i_set])

match = []
match_r = [] # match reverse
match_a = [] # for re-(a)rranging purposes

for row_i in range(len(contents)):
    row = contents[row_i]
    numbers = np.array([row[i] for i in i_set])
    if row_i <= 143: 
        year = 16
    else:
        year = 17
    for v_row_i in range(len(vollaard)):
        v_row = vollaard[v_row_i]
        v_numbers = np.array([v_row[i] for i in v_i_set])
        # find the row number corresponding to the record in Vollaard's file
        # match the (gewicht, vet_percentage, prijs_per_100g)
        if v_row[-1]==1:
            v_year = 17
        else:
            v_year = 16
        if np.linalg.norm(numbers - v_numbers) <= 0.000001 and v_year == year:
            # print(f"We matched {row[15]} in our records with {v_row[0]} in Vollaard's records")
            match.append([row[15], v_row[0]])
            match_r.append([v_row[0], row[15]])
            match_a.append([v_row_i, row_i])


match = dict(match)
match_r = dict(match_r)
match_a = dict(match_a)

# print(match)
# print(match_r)

contents_head.append('vollaard_id')
for i in range(len(contents)):
    row = contents[i]
    vollaard_id = match[row[15]]
    contents[i].append(vollaard_id)
    # print(f"Processed {i+1} rows of our records of {len(contents)} row")

# print(contents[156])

# re-arrange into vollaard's ordering
reordered_contents = []
reordered_contents.append(contents_head)

for v_i in range(len(vollaard)):
    # unique_id = match_r[row[0]]
    row_i = match_a[v_i]
    print([row_i+1, v_i+1])
    reordered_contents.append(contents[row_i])

# print(reordered_contents[1])
# print(reordered_contents[292])

vollaard_head.append('our_unique_id')
for i in range(len(vollaard)):
    row = vollaard[i]
    unique_id = match_r[row[0]]
    vollaard[i].append(unique_id)
    # print(f"Processed {i+1} rows of Vollaard's records of {len(vollaard)} row")

# print(vollaard[212])

# write into csv files

with open(output_csv_ours_reordered_file, 'w', newline='', encoding='utf-8') as csvoutput:
    writer = csv.writer(csvoutput, delimiter=csv_delimiter)
    writer.writerows(reordered_contents)
    print("done")


with open(output_csv_ours_file, 'w', newline='', encoding='utf-8') as csvoutput:
    writer = csv.writer(csvoutput, delimiter=csv_delimiter)
    writer.writerow(contents_head)
    writer.writerows(contents)
    print("done")


with open(output_csv_vollaard_file, 'w', newline='', encoding='utf-8') as csvoutput:
    writer = csv.writer(csvoutput, delimiter=csv_delimiter)
    writer.writerow(vollaard_head)
    writer.writerows(vollaard)
    print("done")
