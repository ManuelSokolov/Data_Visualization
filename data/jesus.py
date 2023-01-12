import pandas as pd

# Load the CSV file into a DataFrame
# df = pd.read_csv('Unicorn_Clean.csv', index_col=False)
df55 = pd.read_csv('Unicorn_Companies.csv', index_col=False)
print(df55.columns)
df55.dropna(subset=['Valuation ($B)','Founded Year'])
# #print (df.head(5))
print(df55['Valuation ($B)'].isna().sum())
print(df55['Founded Year'].isna().sum())

print(df55['Valuation ($B)'][0])

#df55.to_csv('Unicorn_Companies_for_cluster.csv')
# #print(df.columns)
# df = df.drop(['Company', 'Valuation ($B)', 'Date Joined', 'Country', 'City'], axis=1)
exit(1)
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
df55.to_csv('teste.csv')