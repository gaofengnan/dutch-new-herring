# import re
import os
import csv
import pandas as pd
import numpy as np

csv_16_file = open('2016_output.csv', 'r', encoding='utf-8', newline='')
csv_17_file = open('2017_output.csv', 'r', encoding='utf-8', newline='')
output_csv_file = '1617_combined_output.csv'
output_csv_vollaard_file = '1617_combined_vollaard_output.csv'
csv_delimiter = ';'

# init the output 
if os.path.exists(output_csv_file):
    os.remove(output_csv_file)
    print("The output file has been removed.")
else:
    print("The output file does not exist. Nothing to remove.")
if os.path.exists(output_csv_vollaard_file):
    os.remove(output_csv_vollaard_file)
else:
    print("The vollaard output file does not exist. Nothing to remove.")

data_16_reader = csv.DictReader(csv_16_file, delimiter=csv_delimiter)
data_17_reader = csv.DictReader(csv_17_file, delimiter=csv_delimiter)

match_16 = []
match_17 = []

for row_16 in data_16_reader:
    # print("Now processing 2017, No %s" % (int(row_16['original_id_2016'])+1))
    # print(row_16['address'])
    csv_17_file.seek(0)
    for row_17 in data_17_reader:
        # print(row_17['address'])
        if row_16['address']==row_17['address']:
            # print("No %s in 2016 is probably the same as No %s in 2017."% (int(row_16['original_id_2016']), int(row_17['original_id_2017'])))
            match_16.append([row_16['original_id_2016'], row_17['original_id_2017']])
            match_17.append([row_17['original_id_2017'], row_16['original_id_2016']])

match_16 = dict((x[0], x[1]) for x in match_16)
match_17 = dict((x[0], x[1]) for x in match_17)
# print(match_16, match_17)



with open('2016_output.csv', 'r', encoding='utf-8', newline='') as csvinput:
    with open(output_csv_file, 'a+', encoding='utf-8') as csvoutput:
        reader = csv.reader(csvinput,delimiter=csv_delimiter)
        writer = csv.writer(csvoutput,delimiter=csv_delimiter)
        contents = []
        row = next(reader)
        row = row[1:]
        row.append('unique_id')
        contents.append(row)
        for row in reader:
            if row[0] in match_16.keys():
                unique_id = str('16_') + row[0].zfill(3) + str('_17_') + match_16[row[0]].zfill(3)
            else:
                unique_id = str('16_') + row[0].zfill(3)
            row = row[1:]
            row.append(unique_id)

            contents.append(row)

        writer.writerows(contents)
        print('done')


with open('2017_output.csv', 'r', encoding='utf-8', newline='') as csvinput:
    with open(output_csv_file, 'a+', encoding='utf-8') as csvoutput:
        reader = csv.reader(csvinput,delimiter=csv_delimiter)
        writer = csv.writer(csvoutput,delimiter=csv_delimiter)
        contents = []
        row = next(reader)
        for row in reader:
            if row[0] in match_17.keys():
                unique_id =  str('17_')+row[0].zfill(3)+ str('_16_')+ match_17[row[0]].zfill(3)
            else:
                unique_id = str('17_') + row[0].zfill(3)
            row = row[1:]
            row.append(unique_id)

            contents.append(row)

        writer.writerows(contents)
        print('done')



