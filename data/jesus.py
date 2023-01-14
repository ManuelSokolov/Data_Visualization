import pandas as pd
import numpy as np

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
#df55 = pd.read_csv('country-capitals.csv')
#df55['CountryName'] = df55['CountryName'].replace('United States', 'USA')
#df55['CountryName'] = df55['CountryName'].replace('United Kingdom', 'UK')
#df55['CountryName'] = df55['CountryName'].replace('Hong Kong', 'China')
#df55['CountryName'] = df55['CountryName'].replace('Santa Clara', 'USA')

#df55= df55.sort_values('CountryName')

#df55.to_csv(.csv')