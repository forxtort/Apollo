#!/usr/bin/env python
# coding: utf-8

# In[1]:


# The packages required for data visualization for this data set. 
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pylab


# In[2]:


# The Titanic project from kaggle we will discuss here
# Link for that project https://www.kaggle.com/c/titanic


# In[3]:


# Lets Begin the EDA process and find sum insight of it. 


# In[4]:


Titanic_train=pd.read_csv('C:/Users/HP/Desktop/Titanic data set/train.csv')


# In[5]:


Titanic_train.head()


# In[6]:


Titanic_train.isnull().sum()


# In[7]:


# Column reduction 
# Person Name column we will not take into our analysis, it will impact anything to our predicition. 
# Same with cabin col it will also not impact our analysis and also has 77% NA values in it. 
# Also remove passenger id coloumn which as no significance it just tell which person will live or not in raw data also
# id can't impart our future predictive analysis.
# Let convert sex column into continuous and also create a dummies for embarked columns.


# In[8]:


# Continous variables column:'Age','Fare', 'passengerid'.
# Categorical variables column: 'Survived', 'Pclass', 'sex', 'sibsp', 'parch', 'ticket', 'embarked', 'Name'.


# In[9]:


from sklearn import preprocessing as pre
df=pre.LabelEncoder()


# In[10]:


Titanic_train['Sex']=df.fit_transform(Titanic_train['Sex'])


# In[11]:


# after transfor sex column we have found that it represent gender as mention below. 
# male=1
# Female=0


# In[12]:


#create dummies for embarked column.


# In[13]:


Titanic_embarked=pd.get_dummies(Titanic_train.Embarked)
Titanic_embarked=Titanic_embarked.astype(int)


# In[14]:


Titanic_A=pd.concat([Titanic_train,Titanic_embarked],axis=1)


# In[15]:


Titanic_A.head(2)


# In[16]:


# droping Name, Ticket, 'PassengerId' and Cabin column
Titanic_A.drop('Name', axis=1, inplace=True)
Titanic_A.drop('Cabin', axis=1, inplace=True)
Titanic_A.drop('Ticket', axis=1, inplace=True)
Titanic_A.drop('PassengerId', axis=1, inplace=True)



# In[17]:


Titanic_A.head(1)


# In[18]:


# To check the skewness of Titanic_A( Titanic Train data)
Titanic_A.skew()


# In[19]:


# Apart from Pclass and Sex data is right skewed, only these two columns are  left skewed


# In[20]:


#To check the kurtosis of Titanic_A(Titanic Train data)
Titanic_A.kurt()


# In[21]:


Titanic_A.std()


# In[22]:


Titanic_A.var()
## Age and Fare have different variance range than others and we have also find that these two coloumns has lot of outliers.


# In[23]:


Titanic_A.corr()


# In[24]:



Titanic_A.describe()


# In[25]:


#in which region people servive most than other region
sns.barplot(Titanic_A['Embarked'],Titanic_A['Survived'])


# In[26]:


## from above plot we can say that people more survive in cherbourg than other two.


# In[27]:


#According to age female survied more than male.
sns.barplot(Titanic_A['Survived'],Titanic_A['Age'])


# In[28]:


#from above plot we have found that we less than 28 age survive more 


# In[29]:


#Lets check who survie more male and female
sns.barplot(Titanic_A['Sex'],Titanic_A['Survived'])

# female survive more than male. 


# In[30]:


#Lets check for Plcass.
sns.barplot(Titanic_A['Pclass'],Titanic_A['Survived'])
#Plcass 1 survive more than other two.


# In[31]:


#Lets check for Plcass.
sns.barplot(Titanic_A['Pclass'],Titanic_A['Age'])


# In[32]:


# from above plot we can see that that maximum age in Plcass-3 is 25 year of age.
# Pclass-2 last age group is 30 and rest are in Pclass-1.


# In[33]:


sns.distplot(Titanic_A.Age)
# Maximum age group lies between 20-40


# In[34]:


#Let check for sibsp
sns.barplot(Titanic_A['SibSp'],Titanic_A['Survived'])
# siblings survive more than others.


# In[35]:


sns.distplot(Titanic_A.Sex)
# we can say that from train data male are more then female in ship.


# In[36]:


sns.distplot(Titanic_A.Pclass)
#Titanic_A.Pclass.value_counts()
#Pclass(passenger third class is more than other 2)


# In[37]:


sns.distplot(Titanic_A.Fare)


# In[38]:


sns.pairplot(Titanic_A, hue='Survived')
# We can see that there is no colliniearity found to each others.


# In[39]:


sns.violinplot(Titanic_A.Pclass, Titanic_A.Survived,data= Titanic_A)

# People less survive in Pclass-3


# In[40]:


sns.violinplot(Titanic_A.Survived, Titanic_A.Age,data= Titanic_A)

# People who didn't survive is in the group between age 20-40.


# In[41]:


sns.swarmplot(Titanic_A.Embarked, Titanic_A.Fare,data= Titanic_A)

# we can see that maximum people have fare lies between $0-$100.


# In[42]:


sns.boxplot(Titanic_A.Age,data= Titanic_A)
# Age has some outliers


# In[43]:


Titanic_A.isnull().sum()


# In[44]:


print(Titanic_A['Age'].median())
#X['Age'] = X['Age'].fillna(X['Age'].median())


# In[45]:


Titanic_A['Age']=Titanic_A['Age'].fillna(Titanic_A['Age'].median())


# In[46]:


Titanic_A.isnull().sum()


# In[47]:


sns.boxplot(Titanic_A.Age,data= Titanic_A)

# we have noticed that after filling NA values by median, outliers rise in Age column.


# In[48]:


## Lets handle the outliers in Age column.
Titanic_A.Age.describe()


# In[49]:


Q1=Titanic_A['Age'].quantile(0.25)
print(Q1)


# In[50]:


Q3=Titanic_A['Age'].quantile(0.75)
print(Q3)


# In[51]:


IQR=Q3-Q1
print(IQR)


# In[52]:


Lower_Whisker = Q1-1.5*IQR
print(Lower_Whisker)


# In[53]:


Upper_Whisker =Q3+1.5*IQR
print(Upper_Whisker)


# In[54]:


Titanic_A = Titanic_A[Titanic_A['Age'] <Upper_Whisker]


# In[55]:


sns.boxplot(Titanic_A.Age,data= Titanic_A)


# In[56]:


Titanic_A.Age.describe()


# In[57]:


## As we can see that the outliers are not completely handle.
## lets do it again to handle it. 


# In[58]:


Q1=Titanic_A['Age'].quantile(0.25)
print(Q1)


# In[59]:


Q3=Titanic_A['Age'].quantile(0.75)
print(Q3)


# In[60]:


IQR=Q3-Q1
print(IQR)


# In[61]:


Lower_Whisker = Q1-1.5*IQR
print(Lower_Whisker)


# In[62]:


Upper_Whisker =Q3+1.5*IQR
print(Upper_Whisker)


# In[63]:


Titanic_A = Titanic_A[Titanic_A['Age'] <Upper_Whisker]


# In[64]:


Titanic_A = Titanic_A[Titanic_A['Age'] >Lower_Whisker]


# In[65]:


sns.boxplot(Titanic_A.Age,data= Titanic_A)


# In[66]:


list(Titanic_A.columns)


# In[67]:


Titanic_A.drop('Embarked', axis=1, inplace=True)


# In[68]:


sns.boxplot(Titanic_A.Fare,data= Titanic_A)

## lots of outliers in fare columns.Lets handle that


# In[69]:


Titanic_A.Fare.describe()


# In[70]:


Q1_fare=Titanic_A['Fare'].quantile(0.25)
print(Q1_fare)


# In[71]:


Q3_fare=Titanic_A['Fare'].quantile(0.75)
print(Q3_fare)


# In[72]:


IQR_fare=Q3_fare-Q1_fare
print(IQR_fare)


# In[73]:


Lower_Whisker_fare = Q1_fare-1.5*IQR_fare
print(Lower_Whisker)


# In[74]:


Upper_Whisker_fare =Q3_fare+1.5*IQR_fare
print(Upper_Whisker_fare)


# In[75]:


Titanic_A = Titanic_A[Titanic_A['Fare'] <Upper_Whisker_fare]


# In[76]:


sns.boxplot(Titanic_A.Fare,data= Titanic_A)

# we handle some outliers but still there are some  outliers do it this process again. 


# In[77]:


Titanic_A.Fare.describe()


# In[78]:


Q1_fare=Titanic_A['Fare'].quantile(0.25)
print(Q1_fare)


# In[79]:


Q3_fare=Titanic_A['Fare'].quantile(0.75)
print(Q3_fare)


# In[80]:


IQR_fare=Q3_fare-Q1_fare
print(IQR_fare)


# In[81]:


Lower_Whisker_fare = Q1_fare-1.5*IQR_fare
print(Lower_Whisker)


# In[82]:


Upper_Whisker_fare =Q3_fare+1.5*IQR_fare
print(Upper_Whisker_fare)


# In[83]:


Titanic_A = Titanic_A[Titanic_A['Fare'] <Upper_Whisker_fare]


# In[84]:


sns.boxplot(Titanic_A.Fare,data= Titanic_A)


# In[85]:


Titanic_A.Fare.describe()


# In[86]:


Titanic_A.shape


# In[87]:


Survived=np.where(Titanic_A['Survived']>=1,'Yes','No')


# In[88]:


Titanic_A['Survived']=Survived


# In[89]:


Titanic_A.Survived.value_counts()


# In[90]:


## Now here Train data is ready for model building. Thank you.


# In[91]:


#Let start data processing in Test data 
# Here we do some data processing in excel sheet we add survived column in Test data set for validation directly. 


# In[92]:


Titanic_test=pd.read_csv('C:/Users/HP/Desktop/Titanic data set/test.csv')
Titanic_gender_submission=pd.read_csv('C:/Users/HP/Desktop/Titanic data set/gender_submission.csv')


# In[93]:


Titanic_test['Age']=Titanic_test['Age'].fillna(Titanic_test['Age'].median())


# In[94]:


Titanic_test['Fare']=Titanic_test['Fare'].fillna(Titanic_test['Fare'].median())


# In[95]:


Titanic_test.describe()


# In[96]:


Titanic_test.shape


# In[ ]:





# In[97]:


list(Titanic_test.columns)
Titanic_test.head(2)


# In[98]:


sns.boxplot(Titanic_test.Fare,data= Titanic_test)
# Lots of outliers in Fare columns


# In[99]:


#print(Titanic_test['Age'].median())
#X['Age'] = X['Age'].fillna(X['Age'].median())


# In[100]:


Titanic_test['Age']=Titanic_test['Age'].fillna(Titanic_test['Age'].median())


# In[101]:


#sns.boxplot(Titanic_test.Age,data= Titanic_test)
# Lots of outliers in Fare columns


# In[102]:


Titanic_test.Age.describe()


# In[103]:


Titanic_test.isnull().sum()


# In[104]:


Titanic_test['Sex']=df.fit_transform(Titanic_test['Sex'])


# In[105]:


Titanic_embarked=pd.get_dummies(Titanic_test.Embarked)
Titanic_embarked=Titanic_embarked.astype(int)


# In[106]:


Titanic_B=pd.concat([Titanic_test,Titanic_embarked],axis=1)


# In[107]:


Titanic_test.shape


# In[108]:


Titanic_B.drop('Name', axis=1, inplace=True)
Titanic_B.drop('Cabin', axis=1, inplace=True)
Titanic_B.drop('Ticket', axis=1, inplace=True)
Titanic_B.drop('Embarked', axis=1, inplace=True)
Titanic_B.drop('PassengerId', axis=1, inplace=True)


# In[109]:


Titanic_B.head(2)


# In[110]:


Titanic_B.isnull().sum()


# In[111]:


Survived=np.where(Titanic_gender_submission['Survived']>=1,'Yes','No')


# In[112]:


Titanic_gender_submission['Survived']=Survived


# In[113]:


Titanic_gender_submission.Survived.value_counts()


# In[114]:


## Test data is also ready for testing the model. Thank you.


# In[115]:


## Lets Begin first Naive Bayes.


# In[116]:


# Titanic_A(this is train data)
X=Titanic_A[['Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'C', 'Q', 'S']]
y=Titanic_A[['Survived']]


# In[117]:


X1=Titanic_B[[ 'Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'C', 'Q', 'S']]
y1=Titanic_gender_submission[['Survived']]


# In[118]:


Titanic_B.head(2)


# In[119]:


from sklearn.preprocessing import StandardScaler
scale=StandardScaler()
scale.fit_transform(X)
X=pd.DataFrame(X,columns=X.columns)


# In[120]:


from sklearn.naive_bayes import GaussianNB
mode=GaussianNB()
# Building and predicting at the same time 

pred_gnb = mode.fit(X,y).predict(X1)


# In[121]:


X1['Age']=X1['Age'].fillna(X1['Age'].median())


# In[122]:


X1['Fare']=X1['Fare'].fillna(X1['Fare'].median())


# In[123]:


X1.isnull().sum()


# In[124]:


# Confusion matrix GaussianNB model
from sklearn.metrics import confusion_matrix

confusion_matrix(y1,pred_gnb) # GaussianNB model

#np.mean(pred_gnb==y1.values.flatten()) # 80.6%


# In[125]:


pd.crosstab(y1.values.flatten(),pred_gnb) # confusion matrix using 


# In[126]:


np.mean(pred_gnb==y1.values.flatten()) 


# In[127]:


# Lets start Multinomial navie bayes.
from sklearn.naive_bayes import MultinomialNB
imnb = MultinomialNB()


# In[128]:


# Building and predicting at the same time 

pred_mnb = imnb.fit(X,y).predict(X1)


# In[129]:


#Confusion matrix multinomialNB model
from sklearn.metrics import confusion_matrix

confusion_matrix(y1,pred_mnb) # multinomialNB model


# In[130]:


pd.crosstab(y1.values.flatten(),pred_mnb) # confusion matrix using 


# In[131]:


np.mean(pred_mnb==y1.values.flatten()) # 64%

# We have noticed that applying multinomail naive bayes the accuracy of the model decrease hence we will choose
# GaussianNB model. 


# In[132]:


### Lets begin SVM Support Vector Machine.


# In[133]:


from sklearn.svm import SVC
from sklearn import metrics
from sklearn.model_selection import train_test_split
from imblearn.over_sampling import SMOTE


# In[134]:


y.Survived.value_counts()


# In[135]:


# As we can see that Survived column has 
# 0= Not survive 69.8%
#1= survive only 30.2%
# hence balance that by using smote function. 


# In[136]:



so=SMOTE()
X,y=so.fit_sample(X,y)


# In[137]:


y.Survived.value_counts()


# In[138]:


# kernel = linear

mod = SVC(kernel = "linear")
mod.fit(X,y)


# In[139]:


X1.isnull().sum()


# In[140]:


mod.fit(X,y)
mod.score(X,y)#Accuracy on train data is 


# In[141]:


pred_test_linear = mod.predict(X1)
predicted_values=pred_test_linear.reshape(418,1)


# In[142]:


np.mean(predicted_values==y1) # Accuracy =100%



# In[143]:


from sklearn.metrics import classification_report

print(classification_report(predicted_values,y1))


# In[144]:



# Kernel = poly

model_poly = SVC(kernel = "poly")
model_poly.fit(X,y)
pred_test_poly = model_poly.predict(X1)



# In[145]:


np.mean(pred_test_poly==y1)


# In[146]:


predicted_values=pred_test_poly.reshape(418,1)


# In[153]:


np.mean(predicted_values==y1) # 73% accuracy


# In[154]:


print(classification_report(predicted_values,y1))


# In[155]:


# Lets Begin  Decisoin tree 


# In[156]:


from sklearn.tree import DecisionTreeClassifier


# In[157]:


model=DecisionTreeClassifier(criterion='entropy')
model.fit(X,y)


# In[158]:


preds = model.predict(X1)
#preds is predicted values of test data


# In[159]:


pd.Series(preds).value_counts()
#pd.crosstab(y1,preds)


# In[160]:


np.mean(preds==y1.Survived) 


# In[161]:


model.score(X,y) ## Accuracy of train data
#model.score(X1,y1) ## accuracy of test data


# In[162]:


from sklearn.ensemble import BaggingClassifier


# In[163]:


clf=BaggingClassifier(base_estimator=DecisionTreeClassifier(criterion='gini'),n_estimators=10,random_state=1).fit(X,y)


# In[164]:


predict_values=clf.predict(X1)


# In[165]:


clf.score(X1,y1)


# In[166]:


from sklearn.metrics import confusion_matrix,accuracy_score


# In[167]:


confusion_matrix(y1,predict_values)


# In[168]:


pd.crosstab(y1.values.flatten(),predict_values) # confusion matrix using 


# In[169]:


#The score of this model is 75.8% after bagging technique


# In[170]:


## Random forest


# In[171]:


from sklearn.preprocessing import LabelEncoder
from sklearn.model_selection import train_test_split
from imblearn.over_sampling import SMOTE
SO=SMOTE()
from sklearn.ensemble import RandomForestClassifier


# In[172]:


trains=[]
trains2=[]
for i in range(1,20):
    X_train,y_train=SO.fit_sample(X,y)
    model_1=RandomForestClassifier(criterion='entropy',max_depth=10)
    Reg=model_1.fit(X,y)
    train=model_1.score(X,y)
    train2=model_1.score(X1,y1)
    trains.append(train)
    trains2.append(train2)
    if (i==19):
        print('Accuracy of train model',np.mean(trains))
        print('Accuracy of test model',np.mean(trains2))


# In[173]:



from sklearn.ensemble import BaggingClassifier
from sklearn.model_selection import KFold


# In[174]:


score=[]
Kf=KFold(n_splits=10,shuffle=False)
for train_index ,test_index in Kf.split(X,y):
    X_train ,X_test=X.iloc[train_index],X.iloc[test_index]
    y_train ,y_test=y.iloc[train_index],y.iloc[test_index]
    X_train1,y_train1=SO.fit_sample(X,y)
    bag=BaggingClassifier(base_estimator=RandomForestClassifier(n_estimators=100,min_samples_split=10,oob_score=True,n_jobs=4,criterion='entropy'),n_estimators=10)
    Reg1=bag.fit(X_train1,y_train1)
    s=bag.score(X_test,y_test)
    score.append(s)


# In[175]:


np.mean(bag.score(X_train1,y_train1))


# In[176]:


values=bag.predict(X1)


# In[177]:


from sklearn.metrics import classification_report,confusion_matrix


# In[178]:


confusion_matrix(y1, values)


# In[179]:



from sklearn.metrics import classification_report
print(classification_report(y1, values))


# In[180]:


pd.crosstab(y1.values.flatten(), values) # confusion matrix using 

## Accuracy is 91%


# In[ ]:





# In[ ]:





# In[ ]:




