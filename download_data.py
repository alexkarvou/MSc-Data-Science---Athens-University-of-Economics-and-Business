import requests

var_list = [
    'FL_DATE',
    'CARRIER',
    'ORIGIN',
    'ORIGIN_CITY_NAME',
    'DEST',
    'DEST_CITY_NAME',
    'DEP_TIME',
    'DEP_DELAY',
    'ARR_TIME',
    'ARR_DELAY',
    'CANCELLED',
    'CANCELLATION_CODE',
    'DIVERTED',
    'CARRIER_DELAY',
    'WEATHER_DELAY',
    'NAS_DELAY',
    'SECURITY_DELAY',
    'LATE_AIRCRAFT_DELAY'
]

var_list_str = ','.join(var_list) 

sql_str = 'SELECT ' + var_list_str + ' FROM T_ONTIME WHERE YEAR=2015'

payload = {
    'UserTableName': 'On_Time_Performance',
    'DBShortName': 'On_Time',
    'RawDataTable': 'T_ONTIME',
    'sqlstr': sql_str,
    'varlist': var_list
}

url = 'http://transtats.bts.gov/DownLoad_Table.asp?Table_ID=236&Has_Group=3&Is_Zipped=0'

print(sql_str)
r = requests.post(url, payload)

with open('flights.csv.zip', 'wb') as fd:
    for chunk in r.iter_content():
        fd.write(chunk)

