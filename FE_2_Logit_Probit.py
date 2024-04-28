import pandas as pd 
import numpy as np  
import numpy as np
import pandas as pd
from statsmodels.discrete.discrete_model import Probit, Logit
from statsmodels.tools.tools import add_constant
import seaborn as sns


from scipy.stats import kurtosis, skew
import pandas as pd
import numpy as np
import statsmodels as sm
from statsmodels.stats.outliers_influence import variance_inflation_factor

import pandas as pd
from statsmodels.discrete.discrete_model import Probit
from statsmodels.tools.tools import add_constant
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, confusion_matrix

from sklearn.metrics import accuracy_score, confusion_matrix
from sklearn.model_selection import train_test_split
df = pd.read_excel('DATASET.xlsx')
df
selected_columns  = ['Date', 'SP50_L', 'VIX_L', 'RUSSEL_L', 'US CPI Q %', 'US wholesale Q%',
       'US_LT_INT', 'US_SR_INT', 'US_UNEMP_L', 'US_INDPROD_C',
       'US_HSTART_C', 'US_BUD_BAL_PCT_GDP',
       'US_DEBT_L',  'US_EXP_L', 'US_IMP_L', 'US_RTL_IND_L',
       'US_FX_RS_L', 'US_HS_STARTS_L', 'Bentoil_L', 'NDAQ_L']

df['Date'] = pd.to_datetime(df['Date'], format = '%Y-%m-%d')
df=df[selected_columns].dropna().sort_values(by='Date', ascending =True)


# Assuming `df` is your DataFrame and it's already loaded and pre-processed

# First, ensure the 'Date' column is in a datetime format if not already
df['Date'] = pd.to_datetime(df['Date'], format='%Y%m')

# Set 'Date' as the index
df.set_index('Date', inplace=True)

# Resample to weekly frequency, choosing an appropriate aggregation method for non-numeric columns if necessary
# For numeric columns, we can interpolate the values after resampling
df_monthly = df.resample('M').mean()  # This creates NaNs for weeks without data

# Interpolate missing values
df_monthly = df_monthly.interpolate(method='linear')
df_monthly['US_HSTART_C'] = df_monthly['US_HSTART_C'].div(100)# Linear interpolation
df_monthly['US_INDPROD_C'] = df_monthly['US_INDPROD_C'].div(100)# Linear interpolation
df_monthly['US_BUD_BAL_PCT_GDP'] = df_monthly['US_BUD_BAL_PCT_GDP'].div(100)# Linear interpolation
df_monthly['US wholesale Q%'] = df_monthly['US wholesale Q%'].div(100)# Linear interpolation

# Check the result
df_monthly.describe()

df_monthly['log_SPX'] = np.log(df_monthly['SP50_L'])
df_monthly['log_VIX_L'] = np.log(df_monthly['VIX_L'])
df_monthly['log_RUSSEL_L'] = np.log(df_monthly['RUSSEL_L'])

df_monthly['SPX_returns'] = df_monthly['log_SPX'].diff()
df_monthly['VIX_returns'] = df_monthly['log_VIX_L'].diff()
df_monthly['RUSSEL_returns'] = df_monthly['log_RUSSEL_L'].diff()
df_monthly['US_SR_INT'] = df_monthly.US_SR_INT.div(100)

df_monthly['Excess_SPX_returns'] = df_monthly['SPX_returns'] - df_monthly['US_SR_INT']
df_monthly['Excess_VIX_returns'] = df_monthly['VIX_returns'] - df_monthly['US_SR_INT']
df_monthly['Excess_RUSSEL_returns'] = df_monthly['RUSSEL_returns'] - df_monthly['US_SR_INT']

df_monthly['Target'] = np.where(df_monthly['Excess_SPX_returns'] > 0 , 1, 0 ) 

import matplotlib.pyplot as plt

plt.figure(figsize=(14, 7))
plt.plot( df_monthly['log_SPX'], label='Log S&P 500')
plt.plot(df_monthly['log_VIX_L'], label='Log VIX')
plt.plot( df_monthly['log_RUSSEL_L'], label='Log Russell')
plt.title('Time Series of Log-transformed Indices')
plt.xlabel('Date')
plt.ylabel('Log Value')
plt.legend()
plt.show()


df_monthly[[ 'US CPI Q %', 'US wholesale Q%',
       'US_LT_INT', 'US_SR_INT', 'US_UNEMP_L', 'US_INDPROD_C', 'US_HSTART_C',
       'US_BUD_BAL_PCT_GDP', 'US_DEBT_L', 'US_EXP_L', 'US_IMP_L',
       'US_RTL_IND_L', 'US_FX_RS_L', 'US_HS_STARTS_L', 'Bentoil_L', 'NDAQ_L',
       'log_SPX', 'log_VIX_L', 'log_RUSSEL_L', 'SPX_returns', 'VIX_returns',
       'RUSSEL_returns', 'Excess_SPX_returns', 'Target']].describe()

plt.figure(figsize=(14, 7))
plt.hist(df_monthly['SPX_returns'], bins=50, alpha=0.5, label='S&P 500 Returns')
plt.hist(df_monthly['VIX_returns'], bins=50, alpha=0.5, label='VIX Returns')
plt.hist(df_monthly['RUSSEL_returns'], bins=50, alpha=0.5, label='Russell Returns')
plt.title('Histogram of Returns')
plt.xlabel('Returns')
plt.ylabel('Frequency')
plt.legend()
plt.show()


plt.figure(figsize=(14, 7))
plt.scatter(df_monthly['US_SR_INT'], df_monthly['Excess_SPX_returns'], label='Excess S&P 500 Returns')
plt.scatter(df_monthly['US_SR_INT'], df_monthly['Excess_VIX_returns'], label='Excess VIX Returns')
plt.scatter(df_monthly['US_SR_INT'], df_monthly['Excess_RUSSEL_returns'], label='Excess Russell Returns')
plt.title('Excess Returns vs. Short-Term Interest Rate')
plt.xlabel('US Short-Term Interest Rate')
plt.ylabel('Excess Returns')
plt.legend()
plt.show()


plt.figure(figsize=(14, 7))
plt.boxplot([df_monthly[df_monthly['Target']==0]['SPX_returns'].dropna(), df_monthly[df_monthly['Target']==1]['SPX_returns']], labels=['Target 0', 'Target 1'])
plt.title('S&P 500 Returns by Target Variable')
plt.ylabel('S&P 500 Returns')
plt.show()



# Calculating the correlation matrix
corr = df_monthly[['SPX_returns', 'VIX_returns', 'RUSSEL_returns', 'US_SR_INT', 'Excess_SPX_returns', 'Excess_VIX_returns', 'Excess_RUSSEL_returns']].corr()

# Plotting the heatmap
plt.figure(figsize=(10, 8))
sns.heatmap(corr, annot=True, cmap='coolwarm')
plt.title('Correlation Heatmap')
plt.show()


df =  df_monthly.copy()
df= df.reset_index().dropna()
df= df.drop(['Date' ], axis=1)

# Define predictors - using lagged variables

# Define predictors - using lagged variables
predictor_cols = ['US_INDPROD_C', 'US_HSTART_C', 'US_BUD_BAL_PCT_GDP', 'US_HS_STARTS_L',
       'Bentoil_L', 'NDAQ_L']
# Add a constant term to the predictors for the intercept
X = add_constant(df[predictor_cols])

# The response variable
y = df['Target']

# Split the data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Fit a Probit model
probit_model = Probit(y_train.values, X_train.values).fit()

# Print model summary
print(probit_model.summary())

# Predict probabilities
y_pred_prob = probit_model.predict(X_test)

# Convert probabilities to 0/1 binary outcome
y_pred = (y_pred_prob > 0.5).astype(int)

# Evaluate the model
accuracy = accuracy_score(y_test, y_pred)
conf_matrix = confusion_matrix(y_test, y_pred)

print(f'Accuracy: {accuracy}')
print(f'Confusion Matrix:\n{conf_matrix}')


# Assuming 'df_monthly' is a pandas DataFrame with the relevant variables and 'Date' column.

# Copy the original DataFrame and drop NA values
df = df_monthly.copy()
df = df.reset_index().dropna()
df = df.drop(['Date'], axis=1)

# Define predictors - excluding certain columns
exclude_cols = ['SP50_L', 'US_SR_INT', 'VIX_L', 'RUSSEL_L', 'log_SPX', 'log_VIX_L', 'log_RUSSEL_L',
                'SPX_returns', 'VIX_returns', 'Target', 'RUSSEL_returns', 'Excess_SPX_returns', 
                'Excess_VIX_returns', 'Excess_RUSSEL_returns']
predictor_cols = ['US_INDPROD_C', 'US_HSTART_C', 'US_BUD_BAL_PCT_GDP', 'US_HS_STARTS_L',
       'Bentoil_L', 'NDAQ_L']

# Create lagged variables for predictors
for col in predictor_cols:
    df[f'{col}_lag'] = df[col].shift(1)

# Remove the first row with NaNs after lagging
df = df.dropna()

# Define the new set of predictors including lags
lagged_predictor_cols = [col for col in df.columns if 'lag' in col]
# Add a constant term to the predictors for the intercept
X_lagged = add_constant(df[lagged_predictor_cols])

# The response variable remains the same
y = df['Target']

# Split the data into training and test sets for both original and lagged predictors
X_train, X_test, y_train, y_test = train_test_split(df[predictor_cols], y, test_size=0.01, random_state=42)
X_train_lagged, X_test_lagged, _, _ = train_test_split(X_lagged, y, test_size=0.01, random_state=42)

# Fit the original Probit model
probit_model = Probit(y_train, add_constant(X_train)).fit()
print(probit_model.summary())

# Fit the Probit model with lagged variables
probit_model_lagged = Probit(y_train, X_train_lagged).fit()
print(probit_model_lagged.summary())

# Predict and evaluate the original model
y_pred = (probit_model.predict(add_constant(X_test)) > 0.5).astype(int)
accuracy = accuracy_score(y_test, y_pred)
conf_matrix = confusion_matrix(y_test, y_pred)
print(f'Original Model Accuracy: {accuracy}')
print(f'Original Model Confusion Matrix:\n{conf_matrix}')

# Predict and evaluate the lagged model
y_pred_lagged = (probit_model_lagged.predict(X_test_lagged) > 0.5).astype(int)
accuracy_lagged = accuracy_score(y_test, y_pred_lagged)
conf_matrix_lagged = confusion_matrix(y_test, y_pred_lagged)
print(f'Lagged Model Accuracy: {accuracy_lagged}')
print(f'Lagged Model Confusion Matrix:\n{conf_matrix_lagged}')

# Compare performances
print(f'Accuracy Improvement: {accuracy_lagged - accuracy}')


from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt

# For the original model
y_score = probit_model.predict(add_constant(X_test))
fpr, tpr, _ = roc_curve(y_test, y_score)
roc_auc = auc(fpr, tpr)

plt.figure()
plt.plot(fpr, tpr, color='darkorange', lw=2, label='ROC curve (area = %0.2f)' % roc_auc)
plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve for Original Model')
plt.legend(loc="lower right")
plt.show()

# For the lagged model
y_score_lagged = probit_model_lagged.predict(X_test_lagged)
fpr_lagged, tpr_lagged, _ = roc_curve(y_test, y_score_lagged)
roc_auc_lagged = auc(fpr_lagged, tpr_lagged)

plt.figure()
plt.plot(fpr_lagged, tpr_lagged, color='darkorange', lw=2, label='ROC curve (area = %0.2f)' % roc_auc_lagged)
plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve for Lagged Model')
plt.legend(loc="lower right")
plt.show()


df['Target_lag'] = df['Target'].shift(1)
predictor_cols = ['US_INDPROD_C', 'US_HSTART_C', 'US_BUD_BAL_PCT_GDP', 'US_HS_STARTS_L',
       'Bentoil_L', 'NDAQ_L', 'Target_lag'] 
df=df.dropna()
y = df['Target']


# Assuming 'Target' is your Y variable

# Split the data into training and test sets for both original and lagged predictors
X_train, X_test, y_train, y_test = train_test_split(df[predictor_cols].drop('Target_lag',axis=1), y, test_size=0.2, random_state=42)
probit_model = Probit(y_train, add_constant(X_train)).fit()
print(probit_model.summary())
y_pred = (probit_model.predict(add_constant(X_test)) > 0.5).astype(int)
accuracy = accuracy_score(y_test, y_pred)
conf_matrix = confusion_matrix(y_test, y_pred)
print(f'Original Model Accuracy: {accuracy}')
print(f'Original Model Confusion Matrix:\n{conf_matrix}')

y_score = probit_model.predict(add_constant(X_test))
fpr, tpr, _ = roc_curve(y_test, y_score)
roc_auc = auc(fpr, tpr)

plt.figure()
plt.plot(fpr, tpr, color='darkorange', lw=2, label='ROC curve (area = %0.2f)' % roc_auc)
plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve for Original Model')
plt.legend(loc="lower right")
plt.show()

# Assuming `probit_model` is your fitted Probit model from statsmodels
marginal_effects = probit_model_lagged.get_margeff()
print(marginal_effects.summary())


import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

# Assuming 'probit_model' is your fitted Probit model from statsmodels

# Predict probabilities
predicted_probs = probit_model.predict()

# Calculate standardized residuals
residuals = y_train- predicted_probs
standardized_residuals = (residuals - np.mean(residuals)) / np.std(residuals)

# Plotting the distribution of standardized residuals
sns.histplot(standardized_residuals, kde=True)
plt.title('Distribution of Standardized Residuals : Original Model')
plt.xlabel('Standardized Residuals')
plt.ylabel('Frequency')
plt.show()
import scipy.stats as stats

# Generate Q-Q plot
stats.probplot(standardized_residuals, dist="norm", plot=plt)
plt.title('Q-Q Plot of Standardized Residuals : Original Model')
plt.show()

from scipy.stats import kurtosis, skew

# Assuming `residuals_original` and `residuals_lagged` are your residuals from the original and lagged models

# Calculate skewness and kurtosis for the original model
skewness_original = skew(residuals)
kurtosis_original = kurtosis(residuals, fisher=True)  # Fisher=True returns the excess kurtosis



# Output the metrics
print(f"Original Model Skewness: {skewness_original}")
print(f"Original Model Kurtosis: {kurtosis_original}")
# Assuming 'Target' is your Y variable
lagged_predictor_cols = [col for col in df.columns if 'lag' in col]
# Add a constant term to the predictors for the intercept
X_lagged = add_constant(df[lagged_predictor_cols])
# Split the data into training and test sets for both original and lagged predictors
X_train_lagged, X_test_lagged, _, _ = train_test_split(X_lagged, y, test_size=0.2, random_state=42)

# Fit the original Probit model

# Fit the Probit model with lagged variables
probit_model_lagged = Probit(y_train, X_train_lagged).fit()
print(probit_model_lagged.summary())

# Predict and evaluate the original model


# Predict and evaluate the lagged model
y_pred_lagged = (probit_model_lagged.predict(X_test_lagged) > 0.5).astype(int)
accuracy_lagged = accuracy_score(y_test, y_pred_lagged)
conf_matrix_lagged = confusion_matrix(y_test, y_pred_lagged)
print(f'Lagged Model Accuracy: {accuracy_lagged}')
print(f'Lagged Model Confusion Matrix:\n{conf_matrix_lagged}')

# Compare performances
print(f'Accuracy Improvement: {accuracy_lagged - accuracy}')


y_score_lagged = probit_model_lagged.predict(X_test_lagged)
fpr_lagged, tpr_lagged, _ = roc_curve(y_test, y_score_lagged)
roc_auc_lagged = auc(fpr_lagged, tpr_lagged)

plt.figure()
plt.plot(fpr_lagged, tpr_lagged, color='darkorange', lw=2, label='ROC curve (area = %0.2f)' % roc_auc_lagged)
plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve for Lagged Model')
plt.legend(loc="lower right")
plt.show()




# Assuming 'probit_model' is your fitted Probit model from statsmodels

# Predict probabilities
predicted_probs = probit_model_lagged.predict()

# Calculate standardized residuals
residuals = y_train- predicted_probs
standardized_residuals = (residuals - np.mean(residuals)) / np.std(residuals)

# Plotting the distribution of standardized residuals
sns.histplot(standardized_residuals, kde=True)
plt.title('Distribution of Standardized Residuals')
plt.xlabel('Standardized Residuals')
plt.ylabel('Frequency')
plt.show()

# Generate Q-Q plot
stats.probplot(standardized_residuals, dist="norm", plot=plt)
plt.title('Q-Q Plot of Standardized Residuals')
plt.show()




skewness_original = skew(residuals)
kurtosis_original = kurtosis(residuals, fisher=True)  # Fisher=True returns the excess kurtosis



# Output the metrics
print(f"Original Model Skewness: {skewness_original}")
print(f"Original Model Kurtosis: {kurtosis_original}")





# Assuming `X_train` is your predictors DataFrame for the original model and `X_train_with_y_lag` for the lagged model

def calculate_vif(X):
    # Adding a constant for the intercept
    X = add_constant(X)
    vif = pd.DataFrame()
    vif["Variables"] = X.columns
    vif["VIF"] = [variance_inflation_factor(X.values, i) for i in range(X.shape[1])]
    return vif

print("VIF for Original Model Predictors:")
print(calculate_vif(X_train))

print("\nVIF for Lagged Model Predictors:")
print(calculate_vif(X_train_lagged))
