# Chile multidimensional poverty visualization
The app can be found here: https://enneite.shinyapps.io/viz_casen_chile/
Here I present the code and the data used.

## Multidimensional poverty
After 10 months in Chile, I interested myself to the multidimentional poverty which is an international indicator (adaptated afterward to each country) that measures poverty through the deficiencies in some dimensions that have the households.
In Chile the dimensions are as follows and each one counts for 25% of final score:
- Education
- Health
- Work and social security
- Housing

Each dimension is then divided in three sub-dimensions:
- Education: education access, educational lagging, scolarship
- Health: malnutrition, healthcare, health access
- Work and social security: employment, social security, retirement
- Housing: overcrowding, house state, basic services

A household is defined as poor if it has at least 25% of total deficiencies.

More information is available here: http://www.desarrollosocialyfamilia.gob.cl/btca/txtcompleto/midesocial/berner-pobrmuldimensional.pdf.

## Shiny app description
The shiny app presented here has to main objectives:
- Visualize general ranking of communes in each sub-dimension
- Compare two communes
The first part allows to visualize the 30 communes with highest percentage of deficient households for a given sub-dimension, with a circle graph and an interactive map.
The second one allows to compare two communes and see how they perform in each sub-dimension in terms of deficient households percentage.

## Data files description
You can find 5 data files in this repository:
- dfcasen.csv: contains all the data from the 2017 CASEN census
- demo_comuna.csv: contains demographic information on Chilean communes
- names_variables.csv: contains variables names and dimension
- descripcion_variables.xlsx: contains the description of each sub-dimension
- porcentaje_carencias.csv: contains the percentage of deficient household by sub-dimension and commune

For the moment the app is entirely in Spanish.
