
df=pd.read_csv('all-crypto-currencies/crypto-markets.csv')
df.head()
list(df)
df=df[['date','name','volume','close']]
df=df[df['name']=='Bitcoin']
df=df.iloc[243:,:]

res_table=prepare_df(df)
res
# res_table=res_table.iloc[50:,:]
res_table.to_csv('df_prepared_only_btc.csv',header=True,index=True)