use "/Users/pedrotremacoldirossi/Dropbox(Personal)/amazon_project/fines_robustness/reg1_n.dta", clear

g after = gleba_first_year >= 2009

areg prior_fine after if gleba_first_year > 2005, a(MUNICIPIO) cluster(MUNICIPIO)
areg prior_fine after if gleba_first_year > 2005, a(UF) cluster(MUNICIPIO)
areg prior_fine after propensity_move if gleba_first_year > 2005, a(MUNICIPIO) cluster(MUNICIPIO)

sum prior_fine if gleba_first_year < 2009 & gleba_first_year > 2005

g y2005 = gleba_first_year == 2005
g y2006 = gleba_first_year == 2006
g y2007 = gleba_first_year == 2007
g y2009 = gleba_first_year == 2009
g y2010 = gleba_first_year == 2010
g y2011 = gleba_first_year == 2011
g y2012 = gleba_first_year == 2012
g y2013 = gleba_first_year == 2013
g y2014 = gleba_first_year == 2014

areg prior_fine y2006 y2007 y2009 y2010 y2011 y2012 y2013 y2014 propensity_move if gleba_first_year > 2005, a(MUNICIPIO) cluster(MUNICIPIO)

areg prior_fine after cloud if gleba_first_year > 2005, a(MUNICIPIO) cluster(MUNICIPIO)
areg prior_fine after cloud_adj if gleba_first_year > 2005, a(MUNICIPIO) cluster(MUNICIPIO)

use "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/fines_robustness/enforcement_clouds.dta", clear

g after = year >= 2009


g enf_target_adj = enforcement_target*fraction_target
g enf_control_adj = enforcement_control*fraction_control

reg enf_target_adj after if year > 2005, robust
reg enf_control_adj after if year > 2005, robust

sum enf_target_adj if year < 2009 & year > 2005
sum enf_control_adj if year < 2009 & year > 2005
