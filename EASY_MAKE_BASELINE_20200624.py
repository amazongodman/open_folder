
#import
import pandas as pd
from sklearn.model_selection import RandomizedSearchCV
from sklearn.metrics import mean_squared_error, mean_squared_log_error
from sklearn.linear_model import Lasso, ElasticNet, Ridge, SGDRegressor
from sklearn.svm import SVR, NuSVR
from sklearn.ensemble import BaggingRegressor, RandomForestRegressor
from sklearn.neighbors import KNeighborsClassifier
from sklearn.cluster import KMeans
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split

import xgboost as xgb
from sklearn.model_selection import GridSearchCV
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

from prettytable import PrettyTable

import lightgbm as lgb
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score


#ダミー化関数の定義
def generate_dummies(df, dummy_column):
    dummies = pd.get_dummies(df[dummy_column], prefix=dummy_column)
    df = pd.concat([df, dummies], axis=1)
    return df

#データ読み込み
train = pd.read_csv('C:/Users/train.tsv',sep='\t')
test  = pd.read_csv('C:/Users/test.tsv',sep='\t')
train.head()


#特徴量の名前や欠損を調べる
train.info()


#データの特徴量が量的か質的か判断、使わないものも選んでおく

category_features = ['season','yr',  'mnth', 'hr','holiday', 'weekday', 'workingday', 'weathersit']
number_features = ['temp', 'atemp', 'hum', 'windspeed']
remove_features = ['atemp','id','dteday','yr']
target = ['cnt']
features= category_features + number_features

#最低限の可視化 hist
train.hist(figsize=(12,10))


#最低限の可視化 相関

matrix = train[number_features + target].corr()
heat = np.array(matrix)
heat[np.tril_indices_from(heat)] = False
fig,ax= plt.subplots()
fig.set_size_inches(20,10)
sns.heatmap(matrix, mask=heat,vmax=1.0, vmin=0.0, square=True,annot=True, cmap="Reds")

#test train splitと変数削除
X = pd.DataFrame.copy(train)
y = X[target]

for dummy_column in category_features:
    X = generate_dummies(X, dummy_column)
 
X = X.drop(category_features+target+remove_features, axis=1)

x_train, x_val, y_train, y_val = train_test_split(X, y, random_state = 22, test_size = 0.2)

#アルゴリズムの比較用テーブル

table = PrettyTable()
table.field_names = ["Model", "MSE", "R-sq"]

#アルゴリズムの比較

models = [
    SGDRegressor(max_iter=1000, tol=1e-3),
    Lasso(alpha=0.1),
    ElasticNet(random_state=0),
    Ridge(alpha=.5),
    SVR(gamma='auto', kernel='linear'),
    SVR(gamma='auto', kernel='rbf'),
    BaggingRegressor(),
    BaggingRegressor(KNeighborsClassifier(), max_samples=0.5, max_features=0.5),
    NuSVR(gamma='auto'),
    RandomForestRegressor(random_state=0, n_estimators=300),
    xgb.XGBRegressor(),
    LinearRegression(),
    lgb.LGBMRegressor(num_leaves=80)
]

for model in models:
    model.fit(x_train, y_train) 
    y_res = model.predict(x_val)
    mse = mean_squared_error(y_val, y_res)
    score = model.score(x_val, y_val)    
    table.add_row([type(model).__name__, format(mse, '.2f'), format(score, '.2f')])

print(table)

#一番良かったモデルを検証データで精度確認

table = PrettyTable()
table.field_names = ["Model", "Dataset", "MSE", 'RMSLE', "R-sq"]


model = RandomForestRegressor( random_state=0, n_estimators=100)
model.fit(x_train, y_train) 

def evaluate(x, y, dataset):
    pred = model.predict(x)
    mse = mean_squared_error(y, pred)
    score = model.score(x, y)    
    rmsle = np.sqrt(mean_squared_log_error(y, pred))
    table.add_row([type(model).__name__, dataset, format(mse, '.2f'), format(rmsle, '.2f'), format(score, '.2f')])
    
evaluate(x_train, y_train, 'training')
evaluate(x_val, y_val, 'validation')

print(table)

#パラメータ探索を定義
n_estimators = [int(x) for x in np.linspace(start = 200, stop = 2000, num = 10)]
max_features = ['auto', 'sqrt']
max_depth = [int(x) for x in np.linspace(10, 110, num = 11)]
max_depth.append(None)
min_samples_split = [2, 5, 10]
min_samples_leaf = [1, 2, 4]
bootstrap = [True, False]

random_grid = {'n_estimators': n_estimators,
               'max_features': max_features,
               'max_depth': max_depth,
               'min_samples_split': min_samples_split,
               'min_samples_leaf': min_samples_leaf,
               'bootstrap': bootstrap}
print(random_grid)

#探索実行

rf = RandomForestRegressor()
model = RandomizedSearchCV(estimator = rf, param_distributions = random_grid, n_iter = 100, cv = 3, verbose=2, random_state=0)
model.fit(x_train, y_train)
print(model)

#一番良かったパラメータを確認
model.best_params_

#改めて表にする
table = PrettyTable()
table.field_names = ["Model", "Dataset", "MSE", 'RMSLE', "R-sq"]
evaluate(x_train, y_train, 'training')
evaluate(x_val, y_val, 'validation')
print(table)

#重要度の計算
importances = rf.feature_importances_
std = np.std([tree.feature_importances_ for tree in rf.estimators_], axis=0)
indices = np.argsort(importances)[::-1]

dummy_features = x_train.columns.values
for f in range(dummy_features.shape[0]):
    print("%d. feature %s (%f)" % (f + 1, dummy_features[indices[f]], importances[indices[f]]))

#重要度の作図
# Plot the feature importances of the forest
plt.figure(figsize=(18,5))
plt.title("Feature importances")
plt.bar(range(dummy_features.shape[0]), importances[indices], color="cornflowerblue", yerr=std[indices], align="center")
plt.xticks(range(dummy_features.shape[0]), [dummy_features[i] for i in indices])
plt.xlim([-1, dummy_features.shape[0]])
plt.show()
