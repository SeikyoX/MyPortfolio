# Load required function to import excel to Python
import pandas as pd
from sqlalchemy import create_engine, URL
# Set up the sqlalchemy engine
url_object = URL.create(
'mysql+pymysql',
username='root',
password='anl503',
host='localhost',
database='anl503jul24')
engine = create_engine(url_object)
# Read in excel data into Pandas DataFrame
file_path = "ECA_data.xlsx"  #excel file
sheet_name = "ECA_data"  #excel file sheet name
df = pd.read_excel(file_path, sheet_name=sheet_name)
# View DataFrame summary info
print(df.info())
# Upload DataFrame to MySQL as 'shopper_profile' table.
df.to_sql('shopper_profile', engine,
if_exists='replace', index=False)