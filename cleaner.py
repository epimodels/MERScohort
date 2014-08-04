import pandas as pd
import numpy as np


def read_data(fpath):
  df = pd.read_csv(fpath)

  return df



def clean_data(epi):
  epi.gender = epi.gender.replace(['?', 'M?'], np.nan)
  epi.age = epi.age.replace('?', np.nan)
  epi.age = epi.age.replace('60+', 60)
  epi.age = epi.age.astype('float')
  epi.Sex = epi.gender
  epi.Age = epi.age

  epi['saudi'] = np.nan
  epi.saudi[epi.country=='KSA'] = 1
  epi.saudi[epi.country!='KSA'] = 0
  epi['HCW'] = epi['HCW'].replace(['HCW', 'TRUE?', 'TRUE'], True)
  epi['HCW'] = epi['HCW'].replace(['?FALSE', 'FALSE'], False)
  epi['comorbidity'] = epi.comorbidity.fillna('FALSE')
  epi['comorbidity'] = epi.comorbidity.replace('SEE NOTE', np.nan)
  epi['comorbidity'] = epi.comorbidity.replace('?', np.nan)
  epi['severity2'] = epi.severity
  epi['secondary'] = epi['secondary'].replace('?TRUE', 'TRUE')
  epi['secondary'] = epi['secondary'].replace('FALSE?', 'FALSE')
  epi['severity2'] = epi.severity2.replace(['ICU', 'severe', 'CCU', 'critical'], 'severe')
  epi['severity2'] = epi.severity2.replace(['stable', 'pneumonia', 'hospitalized'], 'moderate')
  epi['severity2'] = epi.severity2.replace(['symptomatic', 'symptomatic?', 'mild/stable', '?mild', 'clinical', 'stable/mild'], 'mild')
  epi['severity2'] = epi.severity2.replace(['?'], np.nan)
  epi['severity2'] = epi.severity2.replace('asymptomatic?', 'asymptomatic')
  epi['condensed_health'] = epi['severity'].replace(['CCU', 'ICU', 'asymptomatic', 'critical', 'hospitalized', 'mild', 'pneumonia', 'symptomatic', 'mild/stable', '?', 'symptomatic?', 'stable/mild', '?mild', 'asymptomatic?', 'clinical', 'severe', 'stable'], 'Alive')

  return epi

#Example
fpath = 'cases.csv'
_mers = read_data(fpath)
mers = clean_data(_mers)
mers.to_csv('clean_mers.csv')

