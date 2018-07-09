import os.path, time
import pandas as pd
import datetime

actual_time = datetime.datetime.now().strftime('%m/%d/%Y')
print(str(actual_time))

dict_file = {}

for root, dirs, files in os.walk("/raid/home/protassow/Forschungspraktikum2/chlor_methan_kom/"):
    for name in files:
        if name == 'cl_me_km.llog':
            result = os.path.join(root[59:], name)
            d_file = os.path.join(root, name)
            d_file_conv = time.localtime(os.path.getctime(d_file))
            d_file_fine = time.strftime('%m/%d/%Y - %H:%M:%S', d_file_conv)
            dict_file[d_file_fine] = result


df = pd.DataFrame.from_dict(dict_file, orient="index")
df.index = pd.to_datetime(df.index)
df = df.sort_index()
print(df['8/17/2017' : actual_time])

df.to_csv('file_info.txt', sep='\t', header=False)
