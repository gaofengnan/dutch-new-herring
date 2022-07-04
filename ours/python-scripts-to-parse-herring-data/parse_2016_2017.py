# import re
import os
import csv

data_file = '2016.txt'
output_csv_file = '2016_output.csv'
contents = open(data_file,'r',encoding='utf-8').readlines()

csv_delimiter = ';'

# init the output 
if os.path.exists(output_csv_file):
    os.remove(output_csv_file)
else:
    print("The file does not exist")

def plus_minus_epsilon(number, epsilon=0.03):
    epsilon = float(epsilon)
    if str(number)[-1] == '+':
        new_number = str(float(str(number)[0:-1]) + epsilon)
    elif str(number)[-1] == '-':
        new_number = str(float(str(number)[0:-1]) - epsilon)
    else:
        new_number = number
    return(new_number)


data_positions = {'original_id_2016': 0, 'name': 2, 'address': 1, 'eindcijfer': 4, 'conclusie': 6, 'vers_van_het_mes': 12, 'beroodeling': 13, 'cijfer': 14, 'rijping': 15, 'schoonmaak': 16, 'gewicht': 17, 'prijs_per_stuck': 18, 'prijs_per_100g': 19, 'temp': 20, 'vet_percentage': 21, 'micro': 22, 'eindcategorie': 11}
data_positions = {'original_id_2016': 0, 'name': 2, 'address': 1, 'eindcijfer': 4, 'conclusie': 6, 'vers_van_het_mes': 12, 'beroodeling': 13, 'cijfer': 14, 'rijping': 15, 'schoonmaak': 16, 'gewicht': 17, 'prijs_per_stuck': 18, 'prijs_per_100g': 19, 'temp': 20, 'vet_percentage': 21, 'micro': 22} #, 'eindcategorie': 11}
# trunc_positions = {'original_id_2016': 0, 'name': 2, 'address': 1, 'eindcijfer': 4, 'conclusie': 6, 'ver': 12, 'beroodeling': 13, 'cijfer': 14, 'rijping': 15, 'schoonmaak': 16, 'gewicht': 17, 'prijs_per_stuck': 18, 'prijs_per_100g': 19, 'temp': 20, 'vet': 21, 'micro': 22, 'eindcategorie': 11}

# print(list(data_positions.keys()))

with open(output_csv_file, 'a+', newline='', encoding='utf-8') as csv_write_obj:
    writer = csv.writer(csv_write_obj, delimiter=csv_delimiter)
    writer.writerow(list(data_positions.keys()))


name_position = 2
address_position = 1
eindcijfer_position = 4
conclusie_position = 6
vers_position = 12
beroodeling_position = 13
cijfer_position = 14
rijping_position = 15
schoonmaak_position = 16
gewicht_position = 17
prijs_position = 18
prijs_per_position = 19
temp_position = 20
vet_position = 21
micro_position = 22
eindcat_position = 11


no_lines_per_shop = 24
no_shops = round(len(contents) / no_lines_per_shop)

for i in range(no_shops):
# for i in range(5):
    base_line_no = i * no_lines_per_shop
    original_id_2016 = i + 1
    name = contents[base_line_no + name_position].rstrip()
    address = contents[base_line_no + address_position].rstrip()
    eindcijfer = plus_minus_epsilon(contents[base_line_no+ eindcijfer_position][12:].rstrip())
    conclusie = contents[base_line_no + conclusie_position][11:].rstrip()
    vers = contents[base_line_no + vers_position][13:].rstrip()
    beroodeling = contents[base_line_no + beroodeling_position][12:].rstrip()
    cijfer = plus_minus_epsilon(contents[base_line_no + cijfer_position][14:].rstrip())
    rijping = contents[base_line_no + rijping_position][8:].rstrip()
    schoonmaak = contents[base_line_no + schoonmaak_position][11:].rstrip()
    gewicht = contents[base_line_no + gewicht_position][15:].rstrip()
    prijs = contents[base_line_no + prijs_position][19:].rstrip()
    prijs_per = contents[base_line_no + prijs_per_position][23:].rstrip()
    temp = contents[base_line_no + temp_position][22:].rstrip()
    vet = contents[base_line_no + vet_position][14:].rstrip()
    micro = contents[base_line_no + micro_position][29:].rstrip()
    # eindcat = contents[base_line_no + eindcat_position][14:].rstrip()
    # print([name, address, eindcijfer,conclusie,vers, beroodeling, cijfer, rijping, schoonmaak, gewicht, prijs, prijs_per, temp, vet, micro])
    with open(output_csv_file, 'a+', newline='', encoding='utf-8') as csv_write_obj:
        writer = csv.writer(csv_write_obj, delimiter=csv_delimiter)
        writer.writerow([original_id_2016, name, address, eindcijfer,conclusie,vers, beroodeling, cijfer, rijping, schoonmaak, gewicht, prijs, prijs_per, temp, vet, micro])
        #print('done')




data_file = '2017.txt'
output_csv_file = '2017_output.csv'
contents = open(data_file,'r',encoding='utf-8').readlines()

csv_delimiter = ';'

# init the output 
if os.path.exists(output_csv_file):
    os.remove(output_csv_file)
else:
    print("The file does not exist")

def common_numeric_delim(number):
    return(number.replace(',','.'))


def plus_minus_epsilon(number, epsilon=0.03):
    epsilon = float(epsilon)
    if str(number)[-1] == '+':
        new_number = str(float(str(number)[0:-1]) + epsilon)
    elif str(number)[-1] == '-':
        new_number = str(float(str(number)[0:-1]) - epsilon)
    else:
        new_number = number
    return(new_number)

no_lines_per_shop = 23

data_positions = {'original_id_2017': 0, 'name': 2, 'address': 1, 'eindcijfer': 4, 'conclusie': 6, 'vers_van_het_mes': 11, 'beroodeling': 12, 'cijfer': 13, 'rijping': 14, 'schoonmaak': 15, 'gewicht': 16, 'prijs_per_stuck': 17, 'prijs_per_100g': 18, 'temperatuur': 19, 'vet_percentage': 20, 'micro': 21}
# trunc_positions = {'original_id_2017': 0, 'name': 2, 'address': 1, 'eindcijfer': 4, 'conclusie': 6, 'ver': 11, 'beroodeling': 12, 'cijfer': 13, 'rijping': 14, 'schoonmaak': 15, 'gewicht': 16, 'prijs_per_stuck': 17, 'prijs_per_100g': 18, 'temp': 19, 'vet': 20, 'micro': 21}

# print(list(data_positions.keys()))
with open(output_csv_file, 'a+', newline='', encoding='utf-8') as csv_write_obj:
    writer = csv.writer(csv_write_obj, delimiter=csv_delimiter)
    writer.writerow(list(data_positions.keys()))


name_position = 2
address_position = 1
eindcijfer_position = 4
conclusie_position = 6
vers_position = 11
beroodeling_position = 12
cijfer_position = 13
rijping_position = 14
schoonmaak_position = 15
gewicht_position = 16
prijs_position = 17
prijs_per_position = 18
temp_position = 19
vet_position = 20
micro_position = 21

no_shops = int(round(len(contents) / no_lines_per_shop))

for i in range(no_shops):
    base_line_no = i * no_lines_per_shop
    original_id_2017 = i + 1
    name = contents[base_line_no + name_position].rstrip()
    address = contents[base_line_no + address_position].rstrip()
    eindcijfer = plus_minus_epsilon(common_numeric_delim(contents[base_line_no+ eindcijfer_position][12:].rstrip()))
    conclusie = contents[base_line_no + conclusie_position][11:].rstrip()
    vers = contents[base_line_no + vers_position][18:].rstrip()
    beroodeling = contents[base_line_no + beroodeling_position][54:].rstrip()
    cijfer = plus_minus_epsilon(common_numeric_delim(contents[base_line_no + cijfer_position][19:].rstrip()))
    rijping = contents[base_line_no + rijping_position][8:].rstrip()
    schoonmaak = contents[base_line_no + schoonmaak_position][11:].rstrip()
    gewicht = contents[base_line_no + gewicht_position][15:].rstrip()
    prijs = common_numeric_delim(contents[base_line_no + prijs_position][17:].rstrip())
    prijs_per = common_numeric_delim(contents[base_line_no + prijs_per_position][21:].rstrip())
    temp = common_numeric_delim(contents[base_line_no + temp_position][17:].rstrip())
    vet = common_numeric_delim(contents[base_line_no + vet_position][15:].rstrip())
    micro = contents[base_line_no + micro_position][29:].rstrip()
    # print([name, address, eindcijfer,conclusie,vers, beroodeling, cijfer, rijping, schoonmaak, gewicht, prijs, prijs_per, temp, vet, micro])
    with open(output_csv_file, 'a+', newline='', encoding='utf-8') as csv_write_obj:
        writer = csv.writer(csv_write_obj, delimiter=csv_delimiter)
        writer.writerow([original_id_2017, name, address, eindcijfer,conclusie,vers, beroodeling, cijfer, rijping, schoonmaak, gewicht, prijs, prijs_per, temp, vet, micro])
        #print('done')

