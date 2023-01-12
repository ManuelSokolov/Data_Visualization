import pandas as pd
import numpy as np
def convert_raised(val):
    val = val.replace('$','')
    if val[-1] == "B":
        val = val.replace('B','')
    elif val[-1] == "M":
        val = val.replace('M','')
        val = pd.to_numeric(val) * 10 ** -3
    elif val[-1] == "K":
        val = val.replace('K','')
        val = pd.to_numeric(val) * 10 ** -6
    else:
        val = pd.to_numeric(val)
    return val
# Load the CSV file into a DataFrame
# df = pd.read_csv('Unicorn_Clean.csv', index_col=False)
df55 = pd.read_csv('Unicorn_Companies.csv', index_col=False)
df55['Valuation ($B)'] = df55['Valuation ($B)'].replace("None",np.nan)
df55['Total Raised'] = df55['Total Raised'].replace("None",np.nan)
#df55['Total Raised'] = df55['Founded Year'].replace("None",np.nan)
df55 = df55.dropna(subset=['Valuation ($B)','Founded Year','Total Raised'])
# #print (df.head(5))
# Remove "$" character
df55 = df55.drop_duplicates(subset='Company', keep="first")
df55['Valuation ($B)'] = df55['Valuation ($B)'].str.replace('$','')

# Convert column to float
df55['Valuation ($B)'] = df55['Valuation ($B)'].astype(float)

df55['Total Raised'] = df55['Total Raised'].apply(convert_raised)

# #print(df.columns)
# df = df.drop(['Company', 'Valuation ($B)', 'Date Joined', 'Country', 'City'], axis=1)

# lista1 = [x for x in df["Investor 1"].unique().tolist() if str(x) != 'nan']
# lista2 = [x for x in df["Investor 2"].unique().tolist() if str(x) != 'nan']
# lista3 = [x for x in df["Investor 3"].unique().tolist() if str(x) != 'nan']
# lista4 = [x for x in df["Investor 4"].unique().tolist() if str(x) != 'nan']

# lista5 = df["Industry"].unique().tolist()

# lista = lista1 + lista2 + lista3 + lista4
# unique_list = list(set(lista))

# df1 = pd.DataFrame(0,index = lista5, columns=unique_list)

# for industry in lista5:
#     dfAI = pd.read_csv(str(industry)+ '.csv', index_col=False)
#     listanova = dfAI["name"].tolist()
#     listanova1 = dfAI["n"].tolist()
#     lista_final = zip(listanova,listanova1)
#     for name, qtd in lista_final:
#         if name in df1.columns:
#             df1.at[industry, name] += qtd

#df1.to_csv('industry_investor_frequencies.csv')


df55['Country'] = df55['Country'].replace('United States', 'USA')
df55['Country'] = df55['Country'].replace('United Kingdom', 'UK')
df55['Country'] = df55['Country'].replace('Hong Kong', 'China')
df55['Country'] = df55['Country'].replace('Santa Clara', 'USA')
df55= df55.sort_values('Country')
df55.to_csv('unicorn_ready_for_clustering_and_map.csv')